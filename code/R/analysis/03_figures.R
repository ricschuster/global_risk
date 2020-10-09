library(rnaturalearth)
library(sf)
library(here)
library(raster)
library(magrittr)
library(stringr)
library(RColorBrewer)
library(classInt)
library(tidyverse)
library(fasterize)

dr <- 100
data_resolution <- paste0(dr, "km2")

#land
land <- st_as_sf(ne_download(scale = 50, type = 'land', category = 'physical'))
countries <- st_as_sf(ne_download(scale = 50, type = 'countries', category = 'cultural'))

#protected 
wdpa <- raster(here("data/intermediate/100km2/", "wdpa_terrestrial.tif"))

fls <- list.files(here("data/final/100km2/"), pattern = ".tif$", full.names = TRUE)

out_r <- stack(fls[2:length(fls)])

nms <- names(out_r) %>% gsub("solution_run.", "", .) %>%
  gsub("_gap.0.1", "", .) %>%
  str_split(pattern =  "_s.", simplify = TRUE)

nms <- nms[,2]

names(out_r) <- nms

land <- st_transform(land, crs = proj4string(out_r))
countries <- st_transform(countries, crs = proj4string(out_r))


# F1
here("manuscript/figures", paste0("Figure 1", ".png")) %>%
  png(width = 4000, height = 4000, res = 600)

old.par <- par(mfrow=c(4,4))
par(mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,3.5,1.5,0), bg = "white")

for (ii in 1:nlayers(out_r)) {
  # print map
  plot(land$geometry, col = "grey", border = NA)
  # title(names(out_r)[ii], cex.main = 1.5)
  
  plot(out_r[[ii]], add = TRUE, col = c(rgb(0,0,0,alpha=0), "#8c510a"), legend = FALSE, 
       maxpixels = ncell(out_r))
  
  plot(wdpa, add = TRUE, col = "#01665e", legend = FALSE)
  # if(lh[[ii]]){
  #   add_legend("", pal, legend_offsets[3], low_high = lh[ii],
  #              text_col = text_col)
  # }
  
  # boundaries
  plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)
  
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

# mtext("Gurobi", side=3, at = 0.16, cex=1, col="black", outer=TRUE) 
# mtext("Symphony", side=3, at = 0.5, cex=1, col="black", outer=TRUE) 
# mtext("Marxan", side=3, at = 0.825, cex=1, col="black", outer=TRUE) 
# 
# mtext("0.1", side=2, at = 0.9, cex=1, col="black", outer=TRUE, las = 1) 
# mtext("1", side=2, at = 0.7, cex=1, col="black", outer=TRUE, las = 1) 
# mtext("10", side=2, at = 0.5, cex=1, col="black", outer=TRUE, las = 1) 
# mtext("100", side=2, at = 0.3, cex=1, col="black", outer=TRUE, las = 1) 
# mtext("1,000", side=2, at = 0.1, cex=1, col="black", outer=TRUE, las = 1) 

dev.off()
# 


#################################################################################################
# F2
#################################################################################################

out_sum <- sum(out_r)
values(out_sum)[values(out_sum) == 0] <-  NA

here("manuscript/figures", paste0("Figure 2", ".png")) %>%
  png(width = 3600 * 4, height = 2000 * 4, res = 500)

par(mfrow=c(1, 1))

plot(land$geometry, col = "grey95", border = NA)

pal <- brewer.pal(9, 'YlOrRd')
pal <- colorRampPalette(pal)

plot(out_sum, add = TRUE, col = pal(8), legend = FALSE, 
     maxpixels = ncell(out_sum))

plot(wdpa, add = TRUE, col = "#01665e", legend = FALSE)
# if(lh[[ii]]){
#   add_legend("", pal, legend_offsets[3], low_high = lh[ii],
#              text_col = text_col)
# }

# boundaries
plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)


dev.off()
# 


#################################################################################################
# F3
#################################################################################################
here("manuscript/figures", paste0("Figure 3", ".png")) %>%
  png(width = 3600 * 4, height = 2000 * 4, res = 500)

par(mfrow=c(1, 1),
    mar = c(0, 0, 0, 0),
    cex = 2,
    xpd=TRUE)

plot(land$geometry, col = "grey", border = NA)
# 
pal <- brewer.pal(9, 'YlGnBu')
pal <- colorRampPalette(pal)

gadm_country2 <- round(gadm_country2, 2)
breaks <- classIntervals(gadm_country2[!is.na(gadm_country2)], n = 10, style = "kmeans")

# levelplot(gadm_country2, at = breaks$brks, col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd')),
#           margin=FALSE)

plot(gadm_country2, add = TRUE, col = pal(10), breaks = breaks$brks,
     maxpixels = ncell(gadm_country2), legend = FALSE)

plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)

plot(gadm_country2, legend.only=TRUE, col = pal(10),
     breaks = round(breaks$brks,2),
     # legend.width = 1, legend.shrink = 0.75,
     smallplot=c(0.17, 0.18, 0.15, 0.55),
     axis.args=list(#at=seq(r.range[1], r.range[2], 25),
                    #labels=seq(r.range[1], r.range[2], 25),
                    cex.axis = 2),
     legend.args=list(text='Result variability', side = 2, font=2, line=2.5, cex= 2))

# require(raster)
# data(volcano)
# r <- raster(volcano)
# plot(r, col=topo.colors(100), legend=FALSE, axes=FALSE)
# r.range <- c(minValue(r), maxValue(r))

# plot(wdpa, add = TRUE, col = "#01665e", legend = FALSE)
# if(lh[[ii]]){
#   add_legend("", pal, legend_offsets[3], low_high = lh[ii],
#              text_col = text_col)
# }

# boundaries
# plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)


dev.off()


#################################################################################################
# governance
#################################################################################################
wb_mean <- raster(here("data/intermediate/", data_resolution, "wb_mean.tif"))
wb_mean <- (wb_mean + min(wb_mean[], na.rm=T)) * -1

here("manuscript/figures", paste0("Figure S1. Governance", ".png")) %>%
  png(width = 3600 * 4, height = 2000 * 4, res = 500)

par(mfrow=c(1, 1),
    mar = c(0, 0, 0, 0),
    cex = 2,
    xpd=TRUE)

plot(land$geometry, col = "grey", border = NA)
# 
pal <- brewer.pal(9, 'YlOrRd')
pal <- colorRampPalette(pal)

# gadm_country2 <- round(gadm_country2, 2)
# breaks <- classIntervals(gadm_country2[!is.na(gadm_country2)], n = 10, style = "kmeans")

# levelplot(gadm_country2, at = breaks$brks, col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd')),
#           margin=FALSE)

plot(wb_mean, add = TRUE, col = pal(20), legend = FALSE, 
     maxpixels = ncell(wb_mean))


plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)

plot(wb_mean, legend.only=TRUE, col = pal(20),
     # breaks = round(breaks$brks,2),
     # legend.width = 1, legend.shrink = 0.75,
     smallplot=c(0.17, 0.18, 0.15, 0.55),
     axis.args=list(#at=seq(r.range[1], r.range[2], 25),
       #labels=seq(r.range[1], r.range[2], 25),
       cex.axis = 2),
     legend.args=list(text='Governance risk', side = 2, font=2, line=2.5, cex= 2))

dev.off()



#################################################################################################
#lands
#################################################################################################

lands <- raster(here("data/intermediate/", data_resolution, "kehoe_land_system.tif"))
lands <- (lands - 100) * -1

here("manuscript/figures", paste0("Figure S2. Lands", ".png")) %>%
  png(width = 3600 * 4, height = 2000 * 4, res = 500)

par(mfrow=c(1, 1),
    mar = c(0, 0, 0, 0),
    cex = 2,
    xpd=TRUE)

plot(land$geometry, col = "grey", border = NA)
# 
pal <- brewer.pal(9, 'YlOrRd')
pal <- colorRampPalette(pal)

# gadm_country2 <- round(gadm_country2, 2)
# breaks <- classIntervals(gadm_country2[!is.na(gadm_country2)], n = 10, style = "kmeans")

# levelplot(gadm_country2, at = breaks$brks, col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd')),
#           margin=FALSE)

plot(lands, add = TRUE, col = pal(20), legend = FALSE, 
     maxpixels = ncell(lands))


plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)

plot(lands, legend.only=TRUE, col = pal(20),
     # breaks = round(breaks$brks,2),
     # legend.width = 1, legend.shrink = 0.75,
     smallplot=c(0.17, 0.18, 0.15, 0.55),
     axis.args=list(#at=seq(r.range[1], r.range[2], 25),
       #labels=seq(r.range[1], r.range[2], 25),
       cex.axis = 2),
     legend.args=list(text='Land systems risk', side = 2, font=2, line=2.5, cex= 2))

dev.off()


#################################################################################################
# clim_grid_ann <- raster(here("data/intermediate/", data_resolution, "probability-annual-iucn.tif"))
#################################################################################################
clim <- raster(here("data/intermediate/", data_resolution, "climate_frank_ehe.tif"))
clim <- ((clim - min(clim[], na.rm=T)) * 100) + 0.01


here("manuscript/figures", paste0("Figure S3. Climate", ".png")) %>%
  png(width = 3600 * 4, height = 2000 * 4, res = 500)

par(mfrow=c(1, 1),
    mar = c(0, 0, 0, 0),
    cex = 2,
    xpd=TRUE)

plot(land$geometry, col = "grey", border = NA)
# 
pal <- brewer.pal(9, 'YlOrRd')
pal <- colorRampPalette(pal)

clim <- round(clim, 2)
breaks <- classIntervals(clim[!is.na(clim)], n = 10, style = "quantile")

# levelplot(gadm_country2, at = breaks$brks, col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd')),
#           margin=FALSE)

plot(clim, add = TRUE, col = pal(10), breaks = breaks$brks,
     maxpixels = ncell(clim), legend = FALSE)

plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)

plot(clim, legend.only=TRUE, col = pal(10),
     breaks = round(breaks$brks,2),
     # legend.width = 1, legend.shrink = 0.75,
     smallplot=c(0.17, 0.18, 0.15, 0.55),
     axis.args=list(#at=seq(r.range[1], r.range[2], 25),
       #labels=seq(r.range[1], r.range[2], 25),
       cex.axis = 2),
     legend.args=list(text='Climate risk', side = 2, font=2, line=2.5, cex= 2))

dev.off()



#################################################################################################
#Figure S4
#################################################################################################
trm <- raster(here("data/raw/IUCN/richness_10km_Birds_v7_spp_edited_tax_extant_1m_dense_EckertIV_1m_dissolved_Passeriformes_raster2.tif"))
base_raster <- raster(trm) 
res(base_raster) <- sqrt(dr) * 1000

out_r <- stack(fls[2:length(fls)])

nms <- names(out_r) %>% gsub("solution_run.", "", .) %>%
  gsub("_gap.0.1", "", .) %>%
  str_split(pattern =  "_s.", simplify = TRUE)
nms <- nms[,2]
names(out_r) <- nms

out_sum <- sum(out_r)
values(out_sum)[values(out_sum) == 0] <-  NA

wdpa <- raster(here("data/intermediate/", data_resolution, "wdpa_terrestrial.tif"))

hotsp <- st_read(here("data/raw/Biod/hotspots_2016_1.shp")) %>%
  filter(Type == "hotspot area")
hotsp %<>% st_transform(crs = crs(base_raster))
hotsp$one <- 1

hotsp_rast <- fasterize(hotsp[-26,], base_raster, field = "one")

ovlp <- tibble(out_sum = out_sum[],
               out_sum_10 = ifelse(out_sum > 10, 1, 0),
               wdpa = wdpa[] * 10,
               hotsp = hotsp_rast[] * 100,
               ) %>%
  mutate(sum = rowSums(.[2:4], na.rm = TRUE),
         code = recode(sum, `10` = 0, `11` = 0, `100` = 2, `101` = 3, `110` = 2, `111` = 3),
         code = na_if(code, 0),
         name = ifelse(code == 1, "Solution", 
                       ifelse(code == 2, "Hotspot", 
                              ifelse(code == 3, "Overlap", NA))))

plt_rst <- hotsp_rast
plt_rst[] <- ovlp$code

my_col = c('#7a0177','#034e7b','#8c510a')

here("manuscript/figures", paste0("Figure S4", ".png")) %>%
  png(width = 3600 * 4, height = 2000 * 4, res = 500)

par(mfrow=c(1, 1),
    mar = c(0, 0, 0, 0),
    cex = 2,
    xpd=TRUE)

plot(land$geometry, col = "grey", border = NA)
# 
pal <- brewer.pal(9, 'YlOrRd')
pal <- colorRampPalette(pal)

plot(plt_rst, add = TRUE, legend = FALSE, col = my_col)


plot(countries$geometry, col = NA, lwd = 0.5, add = TRUE)

plot(plt_rst, legend.only=TRUE, col = my_col,
     # breaks = round(breaks$brks,2),
     # legend.width = 1, legend.shrink = 0.75,
     smallplot=c(0.17, 0.18, 0.15, 0.55),
     axis.args=list(at=c(1.25, 2, 2.75),
       labels= c("Scenario agreement (>10)", "Hotspot", "Overlap"),
       cex.axis = 2),
     legend.args=list(text='', side = 2, font=2, line=2.5, cex= 2))

dev.off()


