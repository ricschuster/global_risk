library(rnaturalearth)
library(sf)
library(here)
library(raster)
library(magrittr)
library(stringr)


#land
land <- st_as_sf(ne_download(scale = 50, type = 'land', category = 'physical'))
countries <- st_as_sf(ne_download(scale = 50, type = 'countries', category = 'cultural'))

fls <- list.files(here("data/final/100km2/"), pattern = ".tif$", full.names = TRUE)

out_r <- stack(fls[1:8])

nms <- names(out_r) %>% gsub("solution_run.", "", .) %>%
  gsub("_gap.0.05_flp.FALSE", "", .) %>%
  str_split(pattern =  "_s.", simplify = TRUE)

nms <- nms[,2]

names(out_r) <- paste0("SLCA_", nms)

land <- st_transform(ne_land, crs = proj4string(gr_rast))