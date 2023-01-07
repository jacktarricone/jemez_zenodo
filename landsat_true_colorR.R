# create true color imagery from landsat
# jack tarricone
# jan 7th, 2023

library(terra)

# list list rgb bands from both days
setwd("/Users/jacktarricone/ch1_jemez/landsat_fsca/")
mar5_bands <-list.files("./mar_5_sr", pattern = ".TIF", full.names = TRUE) # list all files for Aug-29 2021 (Julian day 241)

# full 12 band winter
mar5_bands_raw <-rast(mar5_bands) # create "SpatRaster" for winter image
mar5_bands_raw # inspect

# test plot using terra's plotRGB function
plotRGB(mar5_bands_raw, r = 7, g = 6, b = 5, stretch = "lin")

# create RGB 3 band rasters to save
mar5_rgb <-c(mar5_bands_raw[[7]], mar5_bands_raw[[6]], mar5_bands_raw[[5]])

# crop to study area
vg <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")
reproj <-project(mar5_rgb, crs(vg)) # proj to shapefile crs
mar5_vg_rgb <-crop(reproj, ext(vg)) # crop to vg
plotRGB(mar5_vg_rgb, stretch = "lin") # test plot
plot(vg, add = TRUE)
writeRaster(mar5_vg_rgb, "mar5_vg_rgb.tif") # save

##### feb 18
feb18_bands <-list.files("./feb_18_sr", pattern = ".TIF", full.names = TRUE) # list all files for Aug-29 2021 (Julian day 241)

# full 12 band winter
feb18_bands_raw <-rast(feb18_bands) # create "SpatRaster" for winter image
feb18_bands_raw # inspect

# test plot using terra's plotRGB function
plotRGB(feb18_bands_raw, r = 7, g = 6, b = 5, stretch = "lin")

# create RGB 3 band rasters to save
feb18_rgb <-c(feb18_bands_raw[[7]], feb18_bands_raw[[6]], feb18_bands_raw[[5]])

# crop to study area
vg <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")
reproj <-project(feb18_rgb, crs(vg)) # proj to shapefile crs
feb18_vg_rgb <-crop(reproj, ext(vg)) # crop to vg
plotRGB(feb18_vg_rgb, stretch = "lin") # test plot
plot(vg, add = TRUE)
writeRaster(feb18_vg_rgb, "feb18_vg_rgb.tif") # save

