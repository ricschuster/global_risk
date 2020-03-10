library(rnaturalearth)
library(sf)

#land
land <- st_as_sf(ne_download(scale = 50, type = 'land', category = 'physical'))
countries <- st_as_sf(ne_download(scale = 50, type = 'countries', category = 'cultural'))


land <- st_transform(ne_land, crs = proj4string(gr_rast))