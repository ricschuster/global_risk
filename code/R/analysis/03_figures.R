library(rnaturalearth)
library(sf)
library(here)
library(raster)
library(magrittr)
library(stringr)
library(RColorBrewer)


#land
land <- st_as_sf(ne_download(scale = 50, type = 'land', category = 'physical'))
countries <- st_as_sf(ne_download(scale = 50, type = 'countries', category = 'cultural'))

#protected 
wdpa <- raster(here("data/intermediate/100km2/", "wdpa_terrestrial.tif"))

fls <- list.files(here("data/final/100km2/"), pattern = ".tif$", full.names = TRUE)

out_r <- stack(fls[1:8])

nms <- names(out_r) %>% gsub("solution_run.", "", .) %>%
  gsub("_gap.0.05_flp.FALSE", "", .) %>%
  str_split(pattern =  "_s.", simplify = TRUE)

nms <- nms[,2]

names(out_r) <- paste0("SLCA_", nms)

land <- st_transform(land, crs = proj4string(out_r))
countries <- st_transform(countries, crs = proj4string(out_r))


# BLM comparison figure
here("Figures", paste0("Figure S10", ".png")) %>%
  png(width = 2000, height = 3000, res = 300)

par(mfrow=c(4,2))
par(mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,3.5,1.5,0), bg = "white")

for (ii in 1:nlayers(out_r)) {
  # print map
  plot(land$geometry, col = "grey", border = NA)
  title(names(out_r)[ii], cex.main = 1.5)

  plot(out_r[[ii]], add = TRUE, col = c(rgb(0,0,0,alpha=0), "#2c7bb6"), legend = FALSE, 
       maxpixels = ncell(out_r))
  
  plot(wdpa, add = TRUE, col = "#31a354", legend = FALSE)
  # if(lh[[ii]]){
  #   add_legend("", pal, legend_offsets[3], low_high = lh[ii],
  #              text_col = text_col)
  # }
  
  # boundaries
  plot(countries$geometry, col = NA, lwd = 1, add = TRUE)
  
  # # title
  # # plot bounds
  # usr <- par("usr")
  # xwidth <- usr[2] - usr[1]
  # yheight <- usr[4] - usr[3]
  # labels
  # text(x = usr[1] - 0.1 * xwidth, y = usr[3] + 0.5 * yheight,
  # labels = ylab[ii], pos = 4, font = 1, cex = 1.5 , col = "black")
  # 
  # text(x = usr[1] + 0.1 * xwidth, y = usr[3] + 0.4 * yheight,
  #      labels = pll[ii], pos = 4, font = 1, cex = 1.5 * scl, col = text_col)
  # 
  #rasterImage(logo,usr[1] + 0.01 * xwidth, usr[3] + 0.03 * yheight,
  #            usr[1] + 0.38 * xwidth, usr[3] + 0.09 * yheight)
  
}

mtext("Gurobi", side=3, at = 0.16, cex=1, col="black", outer=TRUE) 
mtext("Symphony", side=3, at = 0.5, cex=1, col="black", outer=TRUE) 
mtext("Marxan", side=3, at = 0.825, cex=1, col="black", outer=TRUE) 

mtext("0.1", side=2, at = 0.9, cex=1, col="black", outer=TRUE, las = 1) 
mtext("1", side=2, at = 0.7, cex=1, col="black", outer=TRUE, las = 1) 
mtext("10", side=2, at = 0.5, cex=1, col="black", outer=TRUE, las = 1) 
mtext("100", side=2, at = 0.3, cex=1, col="black", outer=TRUE, las = 1) 
mtext("1,000", side=2, at = 0.1, cex=1, col="black", outer=TRUE, las = 1) 

dev.off()
# 
