library(prioritizr)
setwd("D:/Work/IUCN/IUCN_processing")
library(here)
#library(SparseData)
memory.limit(300000)

pu <- raster(here("GADM/land.tif"))
# pu_sutt <- raster(here("cost/sutt.tif"))
# pu_HF <- raster(here("HF/HF09_iucn.tif"))
wdpa <- raster(here("WDPA/wdpa_terrestrial.tif"))
locked_in <- ifelse(!is.na(wdpa[][!is.na(pu[])]), TRUE, FALSE)


if( !file.exists("rij.rds")){
  
  amph <- stack(list.files(here("IUCN/Amph/"), full.names = TRUE))
  rij_amph <- rij_matrix(pu, amph)
  saveRDS(rij_amph, here("rij_amph.rds"))
  
  bird <- stack(list.files(here("IUCN/Bird/"), full.names = TRUE))
  rij_bird <- rij_matrix(pu, bird)
  saveRDS(rij_bird, here("rij_bird.rds"))
  
  mamm <- stack(list.files(here("IUCN/Mamm/"), full.names = TRUE))
  rij_mamm <- rij_matrix(pu, mamm)  
  saveRDS(rij_mamm, here("rij_mamm.rds"))
  
  rept <- stack(list.files(here("IUCN/Rept/"), full.names = TRUE))
  rij_rept <- rij_matrix(pu, rept)
  saveRDS(rij_rept, here("rij_rept.rds"))
  
  rij <- rbind(rij_amph, rij_bird, rij_mamm, rij_rept)
  
  features <- stack(amph, bird, mamm, rept)
  
  
  spec <- data.frame(id = 1:nlayers(features),
                     name = names(features),
                     stringsAsFactors = FALSE)
  
  saveRDS(rij, here("rij.rds"))
  saveRDS(spec, here("spec.rds"))
  
} else {
  rij <- readRDS("rij.rds")
  spec <- readRDS("spec.rds")
}


p1 <- problem(pu[][!is.na(pu[])], features = spec, rij = rij,
              run_checks = FALSE) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints(locked_in) %>%
  add_binary_decisions() #%>%
# add_default_solver(gap = 0)

system.time(s1 <- solve(p1, force = TRUE))



rs1 <- pu
rs1[][!is.na(rs1[])] <- s1
plot(rs1)

saveRDS(s1, here("output/s1.rds"))

p2 <- p1 %>%
  add_relative_targets(0.3)

system.time(s2 <- solve(p2, force = TRUE))



rs2 <- pu
rs2[][!is.na(rs2[])] <- s2
plot(rs2)
saveRDS(s2, here("output/s2.rds"))



#HF as cost
p3 <- problem(pu_HF[][!is.na(pu[])], features = spec, rij = rij,
              run_checks = FALSE) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints(locked_in) %>%
  add_binary_decisions() #%>%
# add_default_solver(gap = 0)

system.time(s3 <- solve(p3, force = TRUE))



rs3 <- pu
rs3[][!is.na(rs3[])] <- s3
plot(rs3)

saveRDS(s3, here("output/s3.rds"))


###
# rij_amph <- readRDS("rij_amph.rds")
# rij_bird <- readRDS("rij_bird.rds")
# rij_mamm <- readRDS("rij_mamm.rds")
# rij_rept <- readRDS("rij_rept.rds")
# 
# rij <- rbind(rij_amph, rij_bird, rij_mamm, rij_rept)

#for testing
# rij <- rij_amph 

wb_mean <- raster(here("WBD/wb_mean.tif"))
ssp2 <- raster(here("Land_use/output/ssp2_year_50_threat_score.tif"))
clim_grid_ann <- raster(here("Climate/probability-annual-iucn.tif"))

###
# only keep values that are present in all 3 threat layers
###

cdf <- as.data.frame(stack(pu, wb_mean, ssp2, clim_grid_ann))
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

clim_val <- clim_grid_ann[][!is.na(pu[])]
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
                                    threads = parallel::detectCores(TRUE) - 1)




rs1 <- raster(pu)
rs1_val <- rs1[][!is.na(pu[])]
rs1_val_red <- rs1_val[keep]
rs1_val_red <- s1$solution
rs1_val[keep] <- rs1_val_red
rs1[][!is.na(pu[])] <- rs1_val

plot(rs1)
# plot(wdpa, add = TRUE)
