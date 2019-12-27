library(raster)
library(tidyverse)
library(magrittr)
library(foreach)
library(doParallel)
library(prioritizr)
setwd("E:/Richard/global_risk/")
# setwd("D:/Work/Papers/2019_global_risk/global_risk/")
library(here)
#library(SparseData)
# memory.limit(300000)

## Define functions
source(here("code/R/functions/multi-objective-prioritization.R"))
# parallelization
# n_cores <- 2
# cl <- makeCluster(n_cores)
# registerDoParallel(cl)

dr <- 100
data_resolution <- paste0(dr, "km2")

pu <- raster(here("data/intermediate/", data_resolution, "land.tif"))
wdpa <- raster(here("data/intermediate/", data_resolution, "wdpa_terrestrial.tif"))
locked_in <- ifelse(!is.na(wdpa[][!is.na(pu[])]), TRUE, FALSE)


if( !file.exists(paste0("data/intermediate/", data_resolution, "/rij.rds"))){

  amph <- stack(list.files(here("data/raw/IUCN/Amph/"), full.names = TRUE))
  rij_amph <- rij_matrix(pu, amph)
  saveRDS(rij_amph, here("data/intermediate/", data_resolution, "rij_amph.rds"))

  bird <- stack(list.files(here("data/raw/IUCN/Bird/"), full.names = TRUE))
  rij_bird <- rij_matrix(pu, bird)
  saveRDS(rij_bird, here("data/intermediate/", data_resolution, "rij_bird.rds"))

  mamm <- stack(list.files(here("data/raw/IUCN/Mamm/"), full.names = TRUE))
  rij_mamm <- rij_matrix(pu, mamm)
  saveRDS(rij_mamm, here("data/intermediate/", data_resolution, "rij_mamm.rds"))

  rept <- stack(list.files(here("data/raw/IUCN/Rept/"), full.names = TRUE))
  rij_rept <- rij_matrix(pu, rept)
  saveRDS(rij_rept, here("data/intermediate/", data_resolution, "rij_rept.rds"))

  rij <- rbind(rij_amph, rij_bird, rij_mamm, rij_rept)

  features <- stack(amph, bird, mamm, rept)


  spec <- data.frame(id = 1:nlayers(features),
                     name = names(features),
                     stringsAsFactors = FALSE)

  saveRDS(rij, here("data/intermediate/", data_resolution, "rij.rds"))
  saveRDS(spec, here("data/intermediate/", data_resolution, "spec.rds"))

} else {
  rij <- readRDS(here("data/intermediate/", data_resolution, "rij.rds"))
  spec <- readRDS(here("data/intermediate/", data_resolution, "spec.rds"))
}


###
# rij_amph <- readRDS(here("data/intermediate/", data_resolution, "rij_amph.rds"))
# rij_bird <- readRDS(here("data/intermediate/", data_resolution, "rij_bird.rds"))
# rij_mamm <- readRDS(here("data/intermediate/", data_resolution, "rij_mamm.rds"))
# rij_rept <- readRDS(here("data/intermediate/", data_resolution, "rij_rept.rds"))
# 
# rij <- rbind(rij_amph, rij_bird, rij_mamm, rij_rept)

#for testing
# rij <- rij_amph

wb_mean <- raster(here("data/intermediate/", data_resolution, "wb_mean.tif"))
ssp2 <- raster(here("data/intermediate/", data_resolution, "ssp2_chng_threat_score.tif"))
# clim_grid_ann <- raster(here("data/intermediate/", data_resolution, "probability-annual-iucn.tif"))
clim_vel <- raster(here("data/intermediate/", data_resolution, "climate_climate_change_velocity_T_cl1.tif"))
###
# only keep values that are present in all 3 threat layers
###

cdf <- as.data.frame(stack(pu, wb_mean, ssp2, clim_vel))
cdf_red <- cdf[!is.na(cdf$land), ]
keep <- !is.na(rowSums(cdf_red))

rij_mat <- as(rij, "TsparseMatrix")
rij_mat_red <- rij_mat[, keep]
rij_mat_use <- as(rij_mat_red, "dgCMatrix")

wb_val <- wb_mean[][!is.na(pu[])]
wb_val_red <- wb_val[keep]
wb_val_red <- (wb_val_red + min(wb_val_red)) * -1

ssp2_val <- ssp2[][!is.na(pu[])]
ssp2_val_red <- ssp2_val[keep]
ssp2_val_red <- ssp2_val_red + 0.01

clim_val <- clim_vel[][!is.na(pu[])]
clim_val_red <- clim_val[keep]

locked_in_red <- locked_in[keep]

cost <- rbind(matrix(wb_val_red, nrow = 1),
              matrix(ssp2_val_red, nrow = 1),
              matrix(clim_val_red, nrow = 1)
)



runs <- expand.grid(wb = 0:1,
                    lu = 0:1,
                    cl = 0:1,
                    flip_priority = c(FALSE, TRUE),
                    gap = 0.05)

runs_dir <- here("data", "final", data_resolution)
# gap <- 0.1
# 
# flip_priority <- FALSE

runs <- foreach(run = seq_len(nrow(runs)), .combine = bind_rows) %do% {
  r <- runs[run, ]
  str_glue_data(r, "Run ", run, 
                ": wb {wb}; lu {lu}; cl {cl}; gap {gap}; flip {flip_priority}") %>% 
    message()
  
  if(!any(c(r$wb, r$lu, r$cl))){
    cost_temp <- rep(1, ncol(cost))
    gap_temp <- r$gap
    
  } else {
    cost_temp <- cost[c(r$wb, r$lu * 2, r$cl * 3),]
    gap_temp <- rep(r$gap, sum(c(r$wb, r$lu, r$cl)))
  }
  
  #solve multi objective function
  s_gur <- multiobjective_prioritization(rij = rij_mat_use,
                                        obj = cost_temp,
                                        pu_locked_in = locked_in_red,
                                        relative_target = rep(0.30, nrow(rij)),
                                        gap = gap_temp,
                                        flip_priority = r$flip_priority,
                                        threads = parallel::detectCores() - 1)

  # save solution
  str_glue_data(r, "rds_run-", sprintf("%03d", run + 100),
                "_gap-{gap}_flip_priority-{flip_priority}_s-{wb}{lu}{cl}.rds") %>%
    file.path(runs_dir, .) %>%
    saveRDS(r, .)

  rs1 <- raster(pu)
  rs1_val <- rs1[][!is.na(pu[])]

  rs1_val_red <- s_gur$solution
  rs1_val[keep] <- rs1_val_red
  rs1[][!is.na(pu[])] <- rs1_val
  
  str_glue_data(r, "solution_run-", sprintf("%03d", run + 100),
                "_gap-{gap}_flip_priority-{flip_priority}_s-{wb}{lu}{cl}.tif") %>% 
    file.path(runs_dir, .) %>% 
    writeRaster(rs1, .)
  
  rm(cost_temp, gap_temp, s_gur, rs1)
  r
  
}


##########
## Post Processing
##########

land <- raster(here("data/intermediate/", data_resolution, "land.tif"))
land_area <- sum(land[], na.rm = TRUE) * prod(res(land))/1000000 /1000000


fls <- list.files(here("data/final/", data_resolution), pattern = "*.tif$", full.names = TRUE)
nms <- gsub(".tif", "", fls) %>% gsub(here("data/final/", data_resolution,"solution_"), "", .)


r_stack <- stack(fls)
names(r_stack) <- nms

r_df <- as.data.frame(r_stack)

prot <- sum(locked_in_red) * dr / 1000000
(selected <- colSums(r_df, na.rm = TRUE) * dr /1000000)
selected - prot

(perc_tot <- round(selected/land_area * 100, 2))

(perc_no_prot <- round((selected - prot)/land_area * 100, 2))

(perc_increse <- perc_tot - prot/land_area*100)


# ss <- sum(r_stack)
# tt <-table(round(ss[],0))
# tt[10] <- tt[9] - sum(locked_in_red)
# names(tt)[10] <- "8-prot"
# tt
# 
# writeRaster(r_stack, here("data/final/", data_resolution,"solution.tif"), bylayer = TRUE, suffix = 'names')


######
# Summarise by land use type? (maybe using Scott's simplified categories)
# summarise by country and report averages?

gadm_df <- read_csv(here("data/intermediate/", data_resolution, "gadm_country_tbl.csv")) %>%
  select(NAME_IDX, GID_0, NAME_0)

gadm_country <- raster(here("data/intermediate/", data_resolution, "gadm_country.tif"))

count_stack <- stack(gadm_country, r_stack)
count_df <- as.data.frame(count_stack) %>% drop_na() 

count_df %<>% left_join(gadm_df, by = c("gadm_country" = "NAME_IDX"))

count_sum <- count_df %>% group_by(NAME_0)  %>% summarise_at(2:(ncol(count_df)-2), list(sum = sum))
# %>% tally()

count_sum %>% write_csv(here("data/final/", data_resolution, "country_summaries_gap_0.05.csv"))

# clean up
# stopCluster(cl)
