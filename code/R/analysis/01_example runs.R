library(prioritizr)
setwd("E:/Richard/global_risk/")
library(here)
#library(SparseData)
# memory.limit(300000)

## Define functions
source(here("code/R/functions/multi-objective-prioritization.R"))


pu <- raster(here("data/intermediate/land.tif"))
wdpa <- raster(here("data/intermediate/wdpa_terrestrial.tif"))
locked_in <- ifelse(!is.na(wdpa[][!is.na(pu[])]), TRUE, FALSE)


if( !file.exists("data/intermediate/rij.rds")){
  
  amph <- stack(list.files(here("data/raw/IUCN/Amph/"), full.names = TRUE))
  rij_amph <- rij_matrix(pu, amph)
  saveRDS(rij_amph, here("data/intermediate/rij_amph.rds"))
  
  bird <- stack(list.files(here("data/raw/IUCN/Bird/"), full.names = TRUE))
  rij_bird <- rij_matrix(pu, bird)
  saveRDS(rij_bird, here("data/intermediate/rij_bird.rds"))
  
  mamm <- stack(list.files(here("data/raw/IUCN/Mamm/"), full.names = TRUE))
  rij_mamm <- rij_matrix(pu, mamm)  
  saveRDS(rij_mamm, here("data/intermediate/rij_mamm.rds"))
  
  rept <- stack(list.files(here("data/raw/IUCN/Rept/"), full.names = TRUE))
  rij_rept <- rij_matrix(pu, rept)
  saveRDS(rij_rept, here("data/intermediate/rij_rept.rds"))
  
  rij <- rbind(rij_amph, rij_bird, rij_mamm, rij_rept)
  
  features <- stack(amph, bird, mamm, rept)
  
  
  spec <- data.frame(id = 1:nlayers(features),
                     name = names(features),
                     stringsAsFactors = FALSE)
  
  saveRDS(rij, here("data/intermediate/rij.rds"))
  saveRDS(spec, here("data/intermediate/spec.rds"))
  
} else {
  rij <- readRDS("data/intermediate/rij.rds")
  spec <- readRDS("data/intermediate/spec.rds")
}


###
 # rij_amph <- readRDS("data/intermediate/rij_amph.rds")
 # rij_bird <- readRDS("data/intermediate/rij_bird.rds")
 # rij_mamm <- readRDS("data/intermediate/rij_mamm.rds")
 # rij_rept <- readRDS("data/intermediate/rij_rept.rds")
# 
# rij <- rbind(rij_amph, rij_bird, rij_mamm, rij_rept)

#for testing
 # rij <- rij_amph 

wb_mean <- raster(here("data/intermediate/wb_mean.tif"))
ssp2 <- raster(here("data/intermediate/ssp2_year_50_threat_score.tif"))
# clim_grid_ann <- raster(here("data/intermediate/probability-annual-iucn.tif"))
clim_vel <- raster(here("data/intermediate/climate_vel_garc.tif"))
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
ssp2_val_red <- ssp2_val_red + 0.0001

clim_val <- clim_vel[][!is.na(pu[])]
clim_val_red <- clim_val[keep]

locked_in_red <- locked_in[keep]

cost <- rbind(matrix(wb_val_red, nrow = 1),
              matrix(ssp2_val_red, nrow = 1),
              matrix(clim_val_red, nrow = 1)
)

s1 <- multiobjective_prioritization(rij = rij_mat_use,
                                    obj = cost,
                                    pu_locked_in = locked_in_red,
                                    relative_target = rep(0.17, nrow(rij)),
                                    gap = rep(0.1, nrow(cost)),
                                    threads = 6)




rs1 <- raster(pu)
rs1_val <- rs1[][!is.na(pu[])]
rs1_val_red <- rs1_val[keep]
rs1_val_red <- s1$solution
rs1_val[keep] <- rs1_val_red
rs1[][!is.na(pu[])] <- rs1_val

plot(rs1)
# plot(wdpa, add = TRUE)
