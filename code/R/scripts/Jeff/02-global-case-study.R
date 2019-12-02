# restore session
session::restore.session(session_path("01"))

# load parameters
glb_parameters <-
  RcppTOML::parseTOML("code/parameters/global-case-study.toml") %>%
  `[[`(MODE)

# load packages
library(sf)

# load functions
source("code/R/functions/blank_raster.R")
source("code/R/functions/calculate_targets.R")
source("code/R/functions/assemble_rij_data.R")

# load data
glb_species_data <-
  do.call(rbind, lapply(c("data/intermediate/birds.rds",
                          "data/intermediate/mammals.rds",
                          "data/intermediate/amphibians.rds"),
  function(x) {
    x <- readRDS(x)
    if (!identical(glb_parameters$number_of_species, "all"))
      x %<>% dplyr::filter(BINOMIAL %in% head(unique(BINOMIAL),
                           glb_parameters$number_of_species))
    return(x)
  }))
glb_species_data <- as(glb_species_data, "Spatial")

glb_land_data <-
  sf::read_sf("data/raw/natural-earth/ne_110m_land.shp") %>%
  lwgeom::st_make_valid() %>%
  sf::st_transform(glb_parameters$crs) %>%
  lwgeom::st_make_valid() %>%
  as("Spatial")

glb_pa_data <- {
  ## extract data from zip archive
  temp_dir <- file.path(tempdir(), basename(tempfile()))
  dir.create(temp_dir, showWarnings = FALSE)
  unzip("data/raw/world-database-on-protected-areas/WDPA.zip", exdir = temp_dir)
  ## ingest data
  out <-
    dir(temp_dir, "^.*\\.shp$", full.names = TRUE) %>%
    sf::read_sf() %>%
    as("Spatial") %>%
    `slot<-`(
       name = "proj4string",
       value = sp::CRS(paste("+proj=cea +lon_0=0 +lat_ts=30",
                             "+x_0=0 +y_0=0 +datum=WGS84",
                             "+units=m +no_defs +ellps=WGS84",
                             "+towgs84=0,0,0")))
  ## clean up temporary directory
  unlink(temp_dir, force = TRUE)
  ## subset protected area data if debugging
  if (identical(MODE, "debug"))
    out <- out[seq_len(100), ]
  ## output data
  out
}
glb_pa_data$status <- 1
gc()

# store numbers of species
glb_number_bird_species <-
  glb_species_data@data %>%
  dplyr::filter(CLASS == "bird", !duplicated(BINOMIAL)) %>%
  nrow()

glb_number_mammal_species <-
  glb_species_data@data %>%
  dplyr::filter(CLASS == "mammal", !duplicated(BINOMIAL)) %>%
  nrow()

glb_number_amphibian_species <-
  glb_species_data@data %>%
  dplyr::filter(CLASS == "amphibian", !duplicated(BINOMIAL)) %>%
  nrow()

glb_number_migrant_species <-
  glb_species_data@data %>%
  dplyr::group_by(BINOMIAL) %>%
  dplyr::summarize(n = dplyr::n_distinct(SEASONAL)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1) %>%
  nrow()

glb_number_species <- glb_number_bird_species +
                      glb_number_mammal_species +
                      glb_number_amphibian_species

# create a raster of planning units
## create blank raster
glb_pu_raster_data <-
  glb_land_data %>%
  raster::extent() %>%
  as("SpatialPolygons") %>%
  `slot<-`(name = "proj4string", value = glb_land_data@proj4string) %>%
  blank_raster(glb_parameters$resolution) %>%
  raster::setValues(1) %>%
  `slot<-`(name = "crs", value = glb_land_data@proj4string)
gc()

## set planning unit ids
glb_pu_raster_data <- raster::mask(glb_pu_raster_data, glb_land_data)
gc()
idx <- raster::Which(!is.na(glb_pu_raster_data), cells = TRUE)
gc()
glb_pu_raster_data[idx] <- seq_along(idx)
gc()

# create a raster for protected areas
glb_pa_raster_data <-
  glb_pa_data %>%
  as("sf") %>%
  fasterize::fasterize(glb_pu_raster_data, field = "status") %>%
  raster::mask(glb_pu_raster_data)

# assemble prioritization data
## planning unit data
glb_pu_data <- data.frame(id = seq_along(idx), cost = 1)
glb_pu_data$status <- FALSE
glb_pu_data$status[which(glb_pa_raster_data[idx] == 1)] <- TRUE

## rij data
glb_rij_data <- assemble_rij_data(pu = glb_pu_raster_data,
                                  features = glb_species_data,
                                  threads = min(general_parameters$threads, 5))

## feature data
glb_feature_data <-
  glb_rij_data %>%
  dplyr::group_by(species) %>%
  dplyr::summarize(amount = sum(amount)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(id = species, total_amount = amount) %>%
  dplyr::mutate(name = glb_species_data$NAME[id]) %>%
  dplyr::left_join(glb_species_data@data %>%
                   dplyr::select(NAME, RLCATEGORY, CLASS) %>%
                   dplyr::rename(name = NAME, class = CLASS,
                                 iucn_code = RLCATEGORY),
                   by = "name") %>%
  dplyr::mutate(proportion_target = calculate_targets(
    total_amount,
    glb_parameters$minimum_size_km2,
    glb_parameters$proportion_at_minimum_size,
    glb_parameters$maximum_size_km2,
    glb_parameters$proportion_at_maximum_size,
    glb_parameters$cap_range_size_km2,
    glb_parameters$cap_target_amount_km2)) %>%
  dplyr::mutate(amount_target = total_amount * proportion_target) %>%
  dplyr::left_join(glb_rij_data %>%
                   dplyr::filter(pu %in%
                                 glb_pu_data$id[glb_pu_data$status]) %>%
                   dplyr::group_by(species) %>%
                   dplyr::summarize(amount_protected = sum(amount)) %>%
                   dplyr::ungroup() %>%
                   dplyr::rename(id = species),
                   by = "id") %>%
  dplyr::mutate(amount_met = amount_protected >= amount_target) %>%
  as.data.frame()

# create problem
glb_problem_object <-
  prioritizr::problem(glb_pu_data, features = glb_feature_data,
                      rij = glb_rij_data, cost_column = "cost") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_absolute_targets("amount_target") %>%
  prioritizr::add_locked_in_constraints("status") %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_gurobi_solver(gap = glb_parameters$gurobi_gap,
                                threads = glb_parameters$gurobi_threads,
                                first_feasible =
                                  glb_parameters$gurobi_first_feasible)

# solve problem
## generate single solution
glb_solution_time <- system.time({
  glb_solution_data <-
    glb_problem_object %>%
    prioritizr::add_gurobi_solver(gap = glb_parameters$gurobi_gap,
                                  threads = glb_parameters$gurobi_threads,
                                  first_feasible =
                                    glb_parameters$gurobi_first_feasible) %>%
    prioritizr::solve()
})[[3]]

## generate replacement cost values
if (!identical(MODE, "debug")) {
  glb_replacement_cost_time <- system.time({
    glb_replacement_cost_data <-
      glb_problem_object %>%
      prioritizr::add_gurobi_solver(gap = glb_parameters$gurobi_gap,
                                    threads = 1,
                                    first_feasible =
                                      glb_parameters$gurobi_first_feasible,
                                    verbose = FALSE) %>%
      prioritizr::replacement_cost(glb_solution_data[, "solution_1"],
                                   threads = glb_parameters$gurobi_threads)
  })[[3]]
} else {
  glb_replacement_cost_data <-
    glb_solution_data %>%
    dplyr::rename(rc_1 = solution_1) %>%
    dplyr::mutate(rc_1 = rc_1 * runif(length(rc_1)))
}

# create raster data showing prioritizations
## compile raster with optimal solution and protected area status
glb_solution_raster_data <- glb_pu_raster_data
glb_solution_raster_data[idx] <- 0
glb_solution_raster_data[idx[glb_solution_data$solution_1 == 1]] <- 1
glb_solution_raster_data[idx[glb_solution_data$status == 1]] <- 2

## compile raster with replacement costs
glb_replacement_cost_raster_data <- glb_pu_raster_data
glb_replacement_cost_raster_data[idx] <- glb_replacement_cost_data$rc_1
glb_replacement_cost_raster_data[idx[glb_solution_data$status == 1]] <- 2

# save results
raster::writeRaster(glb_solution_raster_data,
                    "data/intermediate/glb-solution.tif", NAflag = -9999,
                    overwrite = TRUE)
raster::writeRaster(glb_replacement_cost_raster_data,
                    "data/intermediate/glb-replacement-cost.tif",
                    NAflag = -9999, overwrite = TRUE)
raster::writeRaster(glb_pa_raster_data,
                    "data/intermediate/glb-pa.tif", NAflag = -9999,
                    overwrite = TRUE)
raster::writeRaster(glb_pu_raster_data,
                    "data/intermediate/glb-pu.tif", NAflag = -9999,
                    overwrite = TRUE)

# cleanup
rm(glb_species_data, glb_land_data, glb_rij_data, glb_problem_object, idx,
   glb_pu_raster_data, glb_solution_raster_data, glb_pa_data,
   glb_pa_raster_data, glb_replacement_cost_raster_data)
detach("package:sf", unload = TRUE)

# save session
session::save.session(session_path("02"), compress = "xz")
