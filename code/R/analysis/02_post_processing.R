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
nms2 <- list.files(here("data/final/", data_resolution), pattern = "*.tif") %>% 
  gsub("_gap-0.05_flp-FALSE.tif", "", .)




os1 <- stack(fls2[1:8])
names(os1) <- sprintf("SLCA_%s",substring(nms2[1:8], 20, 24))
os1_sum <- sum(os1)
writeRaster(os1_sum, here("data/final/", data_resolution, "sum_no_flip.tif"))

sum(locked_in_red)

sel_fr <- table(os1_sum[])
sel_fr[10] <- sel_fr[9] - sum(locked_in_red)
names(sel_fr)[10] <- "8-prot"

sel_fr * 100 / 1000000/ land_area * 100


biomes <- raster(here("data/intermediate/", data_resolution, "biomes.tif"))
biom_nm <- read_csv(here("data/intermediate/", data_resolution, "biomes.csv"))


os1_b_df <- stack(os1, biomes) %>% as.data.frame(.) %>% drop_na() %>%
  left_join(biom_nm, by = c("biomes" = "BIOME_NUM")) %>%
  dplyr::select(-c(biomes))

os1_b_out <- os1_b_df %>% group_by(BIOME_NAME) %>% summarise_all(sum)

os1_b_out2 <- cbind(os1_b_out$BIOME_NAME,
  data.frame(round(os1_b_out[,-1] / os1_b_out$SLCA_0001 * 100 - 100, 2))[,-1])


library(ggradar)
library(dplyr)
library(scales)
library(tibble)

mtcars_radar <- mtcars %>% 
  as_tibble(rownames = "group") %>% 
  mutate_at(vars(-group), rescale) %>% 
  tail(4) %>% 
  select(1:10)
