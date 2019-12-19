library(sf)
library(fasterize)
library(lwgeom)
library(dplyr)
library(raster)
library(magrittr)
library(wdpar)
library(tidyverse)
setwd("E:/Richard/global_risk/")
library(here)

## Define functions
source(here("code/R/functions/geo.R"))

dr <- 500
data_resolution <- paste0(dr, "km2")

trm <- raster(here("data/raw/IUCN/richness_10km_Birds_v7_spp_edited_tax_extant_1m_dense_EckertIV_1m_dissolved_Passeriformes_raster2.tif"))
base_raster <- raster(trm) 
res(base_raster) <- sqrt(dr) * 1000

setwd("E:/Richard/IUCN/Spatial_data/")

#####
# Amphibians
#####

if(!file.exists(here("data/raw/IUCN/amph_diss.rds"))){
  basewd <- setwd("AMPHIBIANS/")
  
  amph <- st_read("AMPHIBIANS.shp")
  #filter out:
  # presence other than 1 (extant)
  # seasonal 3 (passage) and 4 (seasonal occurrence uncertain)
  # origin 3 (introduced), 4 (vagrant), 5 (origin uncertain)
  amph_red <- amph %>% filter(presence == 1, seasonal <= 3, origin <= 2)
  
  amph_proj <- amph_red %>% st_transform(crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  amph_proj <- amph_proj %>% st_make_valid()
  
  ##NEED TO GROUP BY BINOMIAL AND SEASONAL TO CAPTURE RESIDENT, BREEDING, AND NON-BREEDING
  amph_diss <- amph_proj %>% group_by(binomial, seasonal) %>% summarise()
  
  saveRDS(amph_diss, here("data/raw/IUCN/amph_diss.rds"))
  
} else {
  amph_diss <- readRDS(here("data/raw/IUCN/amph_diss.rds"))
}


for(ii in 1:nrow(amph_diss)){
  tmp_rast <- base_raster
  tmp_rast <- try(fasterize(amph_diss[ii,], tmp_rast))
  if(class(tmp_rast) == "RasterLayer" & !all(is.na(tmp_rast[]))){
    writeRaster(tmp_rast, here("data/raw/IUCN/Amph/",paste0(as.character(amph_diss[ii,]$binomial), "_", amph_diss[ii,]$seasonal, ".tif")),
                overwrite = TRUE)
  }
  rm(tmp_rast)
}

# #####
# # Birds
# #####

if(!file.exists(here("data/raw/IUCN/bird_diss.rds"))){
  # setwd(basewd)
  
  setwd("BOTW/BOTW_Jeff/")
  
  bird <- st_read("BOTW.gdb", "All_Species", stringsAsFactors = FALSE)
  
  #filter out:
  # presence other than 1 (extant)
  # seasonal 3 (passage) and 4 (seasonal occurrence uncertain)
  # origin 3 (introduced), 4 (vagrant), 5 (origin uncertain)
  bird_red <- bird %>% filter(PRESENCE == 1, SEASONAL <= 3, ORIGIN <= 2)
  
  ### Start Jeff
  repair_threads <- 5
  ## Shuffle data to balance parallel operations
  input_data <- bird_red[sample.int(nrow(bird_red)), ]
  
  ## Repair data
  input_data2 <- input_data %>% sf::st_set_precision(1e+10) %>%
    st_parallel_make_valid(repair_threads)
  
  ## Project data
  input_data3 <- input_data2 %>% sf::st_transform(crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  ## Remove slivers
  input_data4 <- input_data3 %>% sf::st_cast("POLYGON")
  input_data5 <- input_data4[as.numeric(sf::st_area(input_data)) > 1, ]
  
  ## Repair data
  input_data6 <- input_data5 %>% lwgeom::st_snap_to_grid(1)
  input_data7 <- input_data6 %>% st_parallel_make_valid(repair_threads)
  input_data8 <- input_data7 %>% sf::st_simplify(preserveTopology = TRUE, dTolerance = 1000)
  input_data9 <- input_data8 %>% st_parallel_make_valid(repair_threads)
  input_data9 <- input_data9 %>% st_buffer(1)
  ## Dissolve by species id
  input_data10 <- input_data9 %>% group_by(SCINAME, SEASONAL) %>%
    summarise() %>% ungroup()
  
  ## Repair data
  # input_data11 <- input_data9 %>% st_parallel_make_valid(repair_threads)
  # 
  # ## Extract polygons
  # input_data12 <- input_data11 %>% st_subset_polygons()
  # 
  # ## Convert to SpatialPolygonsDataFrame
  # input_data13 <- as(input_data12, "Spatial")
  
  # Exports
  ## Save rds file
  # saveRDS(input_data, output_path, compress = "xz")
  beepr::beep(3)
  
  ### End Jeff
  bird_diss <- st_as_sf(input_data10)
  
  saveRDS(bird_diss, here("data/raw/IUCN/bird_diss.rds"))
  
} else {
  bird_diss <- readRDS(here("data/raw/IUCN/bird_diss.rds"))
}

# bird_diss %<>% group_by(SCINAME) %>%
#   summarise() %>% ungroup()
# tt <- bird_diss %>% summarise()
  
  
for(ii in 1:nrow(bird_diss)){
  tmp_rast <- base_raster
  tmp_rast <- try(fasterize(bird_diss[ii,], tmp_rast))
  if(class(tmp_rast) == "RasterLayer" & !all(is.na(tmp_rast[]))){
    writeRaster(tmp_rast, here("data/raw/IUCN/Bird/",paste0(as.character(bird_diss[ii,]$SCINAME), "_", bird_diss[ii,]$SEASONAL, ".tif")),
                overwrite = TRUE)
  }
  rm(tmp_rast)
}

#####
# Mammals
#####

if(!file.exists(here("data/raw/IUCN/mamm_diss.rds"))){
  # setwd(basewd)
  
  setwd("TERRESTRIAL_MAMMALS/")
  
  mamm <- st_read("TERRESTRIAL_MAMMALS.shp")
  #filter out:
  # presence other than 1 (extant)
  # seasonal 3 (passage) and 4 (seasonal occurrence uncertain)
  # origin 3 (introduced), 4 (vagrant), 5 (origin uncertain)
  mamm_red <- mamm %>% filter(presence == 1, seasonal <= 3, origin <= 2)
  
  mamm_proj <- mamm_red %>% st_transform(crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  mamm_proj <- mamm_proj %>% st_make_valid()
  mamm_diss <- mamm_proj %>% group_by(binomial, seasonal) %>% summarise()
  
  saveRDS(mamm_diss, here("data/raw/IUCN/mamm_diss.rds"))

} else {
  mamm_diss <- readRDS(here("data/raw/IUCN/mamm_diss.rds"))
}


mamm_rast <- raster(mamm_diss, res = 10000)

for(ii in 1:nrow(mamm_diss)){
  tmp_rast <- base_raster
  tmp_rast <- try(fasterize(mamm_diss[ii,], tmp_rast))
  if(class(tmp_rast) == "RasterLayer" & !all(is.na(tmp_rast[]))){
    writeRaster(tmp_rast, here("data/raw/IUCN/Mamm/",paste0(as.character(mamm_diss[ii,]$binomial), "_", mamm_diss[ii,]$seasonal, ".tif")),
                overwrite = TRUE)
  }
  rm(tmp_rast)
}

#####
# Reptiles
#####

if(!file.exists(here("data/raw/IUCN/rept_diss.rds"))){
  # setwd(basewd)
  
  setwd("REPTILES/")
  
  rept <- st_read("REPTILES.shp")
  #filter out:
  # presence other than 1 (extant)
  # seasonal 3 (passage) and 4 (seasonal occurrence uncertain)
  # origin 3 (introduced), 4 (vagrant), 5 (origin uncertain)
  rept_red <- rept %>% filter(presence == 1, seasonal <= 3, origin <= 2)
  
  rept_proj <- rept_red %>% st_transform(crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  rept_proj <- rept_proj %>% st_make_valid()
  rept_diss <- rept_proj %>% group_by(binomial, seasonal) %>% summarise()
  
  saveRDS(rept_diss, here("data/raw/IUCN/rept_diss.rds"))
  
} else {
  rept_diss <- readRDS(here("data/raw/IUCN/rept_diss.rds"))
}

 

rept_rast <- raster(rept_diss, res = 10000)

for(ii in 1:nrow(rept_diss)){
  tmp_rast <- base_raster
  tmp_rast <- try(fasterize(rept_diss[ii,], tmp_rast))
  if(class(tmp_rast) == "RasterLayer" & !all(is.na(tmp_rast[]))){
    writeRaster(tmp_rast, here("data/raw/IUCN/Rept/",paste0(as.character(rept_diss[ii,]$binomial), "_", rept_diss[ii,]$seasonal, ".tif")),
                overwrite = TRUE)
  }
  rm(tmp_rast)
}



#####
# GADM
#####
gadm <- st_read(here("data/raw/GADM/gadm36_levels.gpkg"))
gadm$land <- 1
gadm %<>% st_transform(crs = crs(base_raster))
land <- fasterize(gadm, base_raster, field = "land")
writeRaster(land, here("data/intermediate/", data_resolution, "land.tif"), overwrite = TRUE)

#####
# WDPA
#####
wdpa <- st_read(here("data/raw/WDPA/WDPA_Eckert.shp"))
wdpa %<>% st_transform(crs = crs(base_raster))
wdpa$one <- 1


wdpa_rast <- fasterize(wdpa, base_raster, field = "one")

# remove marine
land <- raster(here("data/intermediate/", data_resolution, "land.tif"))
wdpa_rast[] <- ifelse(!is.na(land[]), wdpa_rast[], NA)
writeRaster(wdpa_rast, here("data/intermediate/", data_resolution, "wdpa_terrestrial.tif"), overwrite = TRUE)

#####
# Climate 
#####
clim_grid <- st_read(here("data/raw/Climate/cell_out-49-fix.shp"), stringsAsFactors = FALSE)
clim_grid$POLYFID <- as.numeric(clim_grid$POLYFID)
  
load(here("data/raw/Climate/probability-annual.RData"))
prob_annual <- prb
names(prob_annual)[2] <- "prob_ann"

load(here("data/raw/Climate/probability-sd.RData"))
prob_sd <- prb
names(prob_sd)[2] <- "prob_sd"


clim_grid <- clim_grid %>% left_join(prob_annual, by = "POLYFID")
clim_grid <- clim_grid %>% left_join(prob_sd, by = "POLYFID")

clim_grid %<>% drop_na() %>% st_transform(crs = crs(base_raster))

# use fasterize  
clim_grid_ann <- fasterize(clim_grid, base_raster, field = "prob_ann")
writeRaster(clim_grid_ann, here("data/intermediate/", data_resolution, "probability-annual-iucn.tif"), overwrite = TRUE)

clim_grid_sd <- fasterize(clim_grid, base_raster, field = "prob_sd")
writeRaster(clim_grid_sd, here("data/intermediate/", data_resolution, "probability-sd-iucn.tif"), overwrite = TRUE)


#####
# Climate Garcia
#####
garc_P_stack <- list.files(here("data/raw/Climate/garcia/P"), full.names = TRUE) %>% stack() 
garc_T_stack <- list.files(here("data/raw/Climate/garcia/T"), full.names = TRUE) %>% stack()
gar_stack <- stack(garc_P_stack, garc_T_stack)

gar_stack %>%   projectRaster(crs = proj4string(base_raster), method = "ngb") %>%
  resample(base_raster, method = "ngb") %>%
  writeRaster(here("data/intermediate/", data_resolution, "climate"), format = "GTiff", bylayer = TRUE, suffix = 'names')

#####
# World Bank
#####
wb_mean <- read_csv(here("data/raw/WBD/MeanIndexPerCountryWBD_rs.csv"), na = character())

gadm <- st_read(here("data/raw/GADM/gadm36_levels.gpkg"), stringsAsFactors = FALSE)

gadm_wb <- left_join(gadm, wb_mean, by = c("GID_0" = "Country.Code"))

no_match <- gadm_wb %>% filter(is.na(MeanIndex))
no_match_df <- no_match %>% st_set_geometry(NULL)

# write_csv(no_match_df, here("WBD/no_match_with_GADM.csv"))

gadm_wb %<>% drop_na() %>% st_transform(crs = crs(base_raster))
gadm_wb$NAME_IDX <- seq(1:nrow(gadm_wb))

gadm_wb %>% st_set_geometry(NULL) %>% write_csv(here("data/intermediate/", data_resolution, "gadm_country_tbl.csv"))

gadm_country <- fasterize(gadm_wb, base_raster, field = "NAME_IDX")
writeRaster(gadm_country, here("data/intermediate/", data_resolution, "gadm_country.tif"), overwrite = TRUE)

wb_mean <- fasterize(gadm_wb, base_raster, field = "MeanIndex")
writeRaster(wb_mean, here("data/intermediate/", data_resolution, "wb_mean.tif"), overwrite = TRUE)

wb_sd <- fasterize(gadm_wb, base_raster, field = "SDIndex")
writeRaster(wb_sd, here("data/intermediate/", data_resolution, "wb_sd.tif"), overwrite = TRUE)

#####
# Land use
#####
thrt_score <- readxl::read_xlsx(here("data/raw/Land_use/Land use threat scores.xlsx"), sheet = "threat score")

proj <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

lu_base <- raster(here("data/raw/Land_use/SSPs_may2017/year_2000.asc"), crs = proj) %>% 
  projectRaster(crs = proj4string(base_raster), method = "ngb") %>%
  resample(base_raster, method = "ngb")
lu_base_tib <- tibble(No = lu_base[]) %>% left_join(thrt_score, by = "No")
lu_base[] <- lu_base_tib$threat_score
writeRaster(lu_base, here("data/intermediate/", data_resolution, "year_2000_threat_score.tif"), overwrite = TRUE)

ssp1 <- raster(here("data/raw/Land_use/SSPs_may2017/ssp1_year_50.asc"), crs = proj) %>% 
  projectRaster(crs = proj4string(base_raster), method = "ngb") %>%
  resample(base_raster, method = "ngb")
ssp1_tib <- tibble(No = ssp1[]) %>% left_join(thrt_score, by = "No")
ssp1[] <- ssp1_tib$threat_score
writeRaster(ssp1, here("data/intermediate/", data_resolution, "ssp1_year_50_threat_score.tif"), overwrite = TRUE)

ssp2 <- raster(here("data/raw/Land_use/SSPs_may2017/ssp2_year_50.asc"), crs = proj) %>%
  projectRaster(crs = proj4string(base_raster), method = "ngb") %>%
  resample(base_raster, method = "ngb")
ssp2_tib <- tibble(No = ssp2[]) %>% left_join(thrt_score, by = "No")
ssp2[] <- ssp2_tib$threat_score
writeRaster(ssp2, here("data/intermediate/", data_resolution, "ssp2_year_50_threat_score.tif"), overwrite = TRUE)

ssp3 <- raster(here("data/raw/Land_use/SSPs_may2017/ssp3_year_50.asc"), crs = proj) %>%
  projectRaster(crs = proj4string(base_raster), method = "ngb") %>%
  resample(base_raster, method = "ngb")
ssp3_tib <- tibble(No = ssp3[]) %>% left_join(thrt_score, by = "No")
ssp3[] <- ssp3_tib$threat_score
writeRaster(ssp3, here("data/intermediate/", data_resolution, "ssp3_year_50_threat_score.tif"), overwrite = TRUE)


a <- 1
b <- 2

ssp1_chng <- (a * lu_base + b * ssp1)/(a + b)
writeRaster(ssp1_chng, here("data/intermediate/", data_resolution, "ssp1_chng_threat_score.tif"), overwrite = TRUE)

ssp2_chng <- (a * lu_base + b * ssp2)/(a + b)
writeRaster(ssp2_chng, here("data/intermediate/", data_resolution, "ssp2_chng_threat_score.tif"), overwrite = TRUE)

ssp3_chng <- (a * lu_base + b * ssp3)/(a + b)
writeRaster(ssp3_chng, here("data/intermediate/", data_resolution, "ssp3_chng_threat_score.tif"), overwrite = TRUE)

