library(prioritizr)
setwd("D:/Work/Papers/2019_global_risk/global_risk/")
library(here)
#library(SparseData)
# memory.limit(300000)
library(raster)
library(tidyverse)
library(magrittr)

## Define functions
source(here("code/R/functions/multi-objective-prioritization.R"))
dr <- 100
data_resolution <- paste0(dr, "km2")

pu <- raster(here("data/intermediate/", data_resolution, "land.tif"))
wdpa <- raster(here("data/intermediate/", data_resolution, "wdpa_terrestrial.tif"))
land <- raster(here("data/intermediate/", data_resolution, "land.tif"))

wb_mean <- raster(here("data/intermediate/", data_resolution, "wb_mean.tif"))
ssp2 <- raster(here("data/intermediate/", data_resolution, "ssp2_chng_threat_score.tif"))
# clim_grid_ann <- raster(here("data/intermediate/", data_resolution, "probability-annual-iucn.tif"))
clim <- raster(here("data/intermediate/", data_resolution, "climate_frank_ehe.tif"))

###
# only keep values that are present in all 3 threat layers
###

cdf <- as.data.frame(stack(pu, wb_mean, ssp2, clim))
cdf_red <- cdf[!is.na(cdf$land), ]
keep <- !is.na(rowSums(cdf_red))

locked_in <- ifelse(!is.na(wdpa[][!is.na(pu[])]), TRUE, FALSE)
locked_in_red <- locked_in[keep]


fls <- list.files(here("data/final/", data_resolution), pattern = "*.rds")
nms <- gsub(".rds", "", fls)

rds_rast <- list()

for(ii in 1:length(fls)){
  
  rs1 <- raster(pu)
  rs1_val <- rs1[][!is.na(pu[])]
  # rs1_val_red <- rs1_val[keep]

  tmp_rds <- readRDS(here("data/final/", data_resolution, fls[ii]))
  
  rs1_val_red <- tmp_rds$solution
  rs1_val[keep] <- rs1_val_red
  rs1[][!is.na(pu[])] <- rs1_val
  
  rds_rast[[ii]] <- rs1 
  names(rds_rast)[ii] <- nms[ii]
  
  rm(rs1, rs1_val, rs1_val_red, tmp_rds)
}

r_stack <- stack(rds_rast)

r_df <- as.data.frame(r_stack)

prot <- sum(locked_in_red) * dr / 1000000
(selected <- colSums(r_df, na.rm = TRUE) * dr /1000000)
selected - prot

(perc_tot <- round(selected/land_area * 100, 2))

(perc_no_prot <- round((selected - prot)/land_area * 100, 2))

(perc_increse <- perc_tot - prot/land_area*100)

land_area <- sum(land[], na.rm = TRUE) * prod(res(land))/1000000 /1000000

ss <- sum(r_stack)
tt <-table(round(ss[],0))
tt[10] <- tt[9] - sum(locked_in_red)
names(tt)[10] <- "8-prot"
tt

writeRaster(r_stack, here("data/final/", data_resolution,"solution.tif"), bylayer = TRUE, suffix = 'names')


######
# Summarise by land use type? (maybe using Scott's simplified categories)
# summarise by country and report averages?

gadm_df <- read_csv(here("data/intermediate/", data_resolution, "gadm_country_tbl.csv")) %>%
  select(NAME_IDX, GID_0, NAME_0)

gadm_country <- raster(here("data/intermediate/", data_resolution, "gadm_country.tif"))

count_stack <- stack(gadm_country, r_stack)
count_df <- as.data.frame(count_stack) %>% drop_na() 

count_df %<>% left_join(gadm_df, by = c("gadm_country" = "NAME_IDX"))

count_sum <- count_df %>% group_by(NAME_0)  %>% summarise_at(2:9, list(sum = sum))
# %>% tally()

count_sum %>% write_csv(here("data/final/", data_resolution, "country_summaries.csv"))


#####
# Maps
fls2 <- list.files(here("data/final/", data_resolution), pattern = "*.tif", full.names = TRUE)

os1 <- stack(fls2[1:8])
os1_sum <- sum(os1)
writeRaster(os1_sum, here("data/final/", data_resolution, "sum_no_flip.tif"))

sum(locked_in_red)

sel_fr <- table(os1_sum[])
sel_fr[10] <- sel_fr[9] - sum(locked_in_red)
names(sel_fr)[10] <- "8-prot"
