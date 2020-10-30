# Initialization
## Declare parameters
args <- commandArgs(TRUE)
print(args)
MODE <- strsplit(args[[1]], ",")[[1]][[1]]
input_path <- strsplit(args[[1]], ",")[[1]][[2]]
output_path <- strsplit(args[[1]], ",")[[1]][[3]]

## Load packages
library(magrittr)
library(dplyr)
library(sf)

## Define functions
source("code/R/functions/geo.R")

# Preliminary processing
## Load parameters
general_parameters <-
  RcppTOML::parseTOML("code/parameters/general.toml")[[MODE]]
repair_threads <- min(general_parameters$threads, 5)

species_parameters <-
  RcppTOML::parseTOML("code/parameters/species.toml")[[MODE]]

## Load data
### unzip data to temporary directory
temp_dir <- file.path(tempdir(), basename(tempfile()))
dir.create(temp_dir, showWarnings = FALSE)
if (endsWith(input_path, "zip"))
  unzip(input_path, exdir = temp_dir)
if (endsWith(input_path, "7z"))
  system(paste0("7z x ", input_path, " -o", temp_dir))

### ingest data
if ((!endsWith(input_path, ".gdb.7z")) && (!endsWith(input_path, ".gdb.zip"))) {
  input_path <- dir(temp_dir, "^.*\\.shp$", full.names = TRUE, recursive = TRUE)
  input_data <- sf::st_read(input_path)
} else {
  input_path <- gsub(".7z", "", basename(input_path), fixed = TRUE)
  input_path <- gsub(".zip", "", basename(input_path), fixed = TRUE)
  input_path <- file.path(temp_dir, input_path)
  qry <- ifelse(identical(MODE, "debug"),
                "select * from All_Species where DATE_='2015'",
                NA)
  input_data <- sf::st_read(input_path, "All_Species", query = qry)
}

# Main processing
## rename "terrestial" column if present
if ("terrestial" %in% names(input_data))
  input_data %<>% rename(terrestrial = terrestial)

## rename "Shape" column if present
if ("Shape" %in% names(input_data))
  input_data %<>% rename(geometry = Shape)

## Rename columns
names(input_data)[-ncol(input_data)] <-
  toupper(names(input_data)[-ncol(input_data)])
names(attr(input_data, "agr")) <- toupper(names(attr(input_data, "agr")))

## Create "BINOMIAL" column if not present
if (!"BINOMIAL" %in% names(input_data))
  input_data %<>% mutate(BINOMIAL = SCINAME)

## Omit species without terrestrial distributions
if ("TERRESTRIAL" %in% names(input_data))
  input_data %<>% filter(TERRESTRIAL == "t")

## Create "RLCATEGORY" column
if ("CODE" %in% names(input_data)) {
  input_data %<>% mutate(RLCATEGORY = CODE)
} else {
  gdb_file <- rgdal::ogrListLayers(input_path) %>%
    grep(pattern = "RedList", value = TRUE)
  csv_file <- tempfile(fileext = ".csv")
  gdalUtils::ogr2ogr(src_datasource_name = input_path,
                     dst_datasource_name = csv_file,
                     f = "CSV", layer = gdb_file,
                     verbose = TRUE, overwrite = TRUE)
  df <- data.table::fread(csv_file, data.table = FALSE) %>%
    dplyr::select(ScientificName, dplyr::contains("IUCN"))
  names(df) <- c("BINOMIAL", "RLCATEGORY")
  input_data$RLCATEGORY <- df$RLCATEGORY[match(input_data$BINOMIAL, df$BINOMIAL)]
}

## clean up temporary directory
unlink(temp_dir, force = TRUE)

## Subset species if needed
if (!identical(species_parameters$number_of_species, "all"))
  input_data %<>% filter(BINOMIAL %in%
                           head(unique(BINOMIAL),
                                species_parameters$number_of_species))
gc()

## Shuffle data to balance parallel operations
input_data <- input_data[sample.int(nrow(input_data)), ]

## Repair data
input_data %<>% sf::st_set_precision(1e+10) %>%
  st_parallel_make_valid(repair_threads)

## Remove uncertain distributions
input_data %<>% filter(PRESENCE == 1, ORIGIN <= 2, SEASONAL != 5)

## Project data
input_data %<>% sf::st_transform(general_parameters$crs)

## Remove slivers
input_data %<>% sf::st_cast("POLYGON")
input_data <- input_data[as.numeric(sf::st_area(input_data)) > 1, ]

## Repair data
input_data %<>% lwgeom::st_snap_to_grid(1)
input_data %<>% st_parallel_make_valid(repair_threads)
input_data %<>% sf::st_simplify(preserveTopology = TRUE, dTolerance = 1000)
input_data %<>% st_parallel_make_valid(repair_threads)

## Dissolve by species id
input_data %<>% group_by(BINOMIAL, SEASONAL) %>%
  summarise(RLCATEGORY = dplyr::first(RLCATEGORY)) %>%
  ungroup()

## Repair data
input_data %<>% st_parallel_make_valid(repair_threads)

## Extract polygons
input_data %<>% st_subset_polygons()

## Add in column indicating class
if (grepl("botw", tolower(input_path), fixed = TRUE))
  input_data %<>% mutate(CLASS = "bird")

if (grepl("amphibian", tolower(input_path), fixed = TRUE))
  input_data %<>% mutate(CLASS = "amphibian")

if (grepl("mammal", tolower(input_path), fixed = TRUE))
  input_data %<>% mutate(CLASS = "mammal")

## Add column with feature name (BINOMIAL + SEASONAL)
input_data %<>% mutate(NAME = paste0(BINOMIAL, " (", SEASONAL, ")"))

## Convert to SpatialPolygonsDataFrame
input_data <- as(input_data, "Spatial")

# Exports
## Save rds file
saveRDS(input_data, output_path, compress = "xz")