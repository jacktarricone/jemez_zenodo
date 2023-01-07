library(terra)

# tutorial for creating harmonized Landsat/Sen-2 (HLS) figures for twitter
# jack tarricone
# december 21, 2021

# january 18th banner hls

# list list rgb bands from both days
setwd("/Users/jacktarricone/ch1_jemez/landsat_fsca/mar_05/")
bands <-list.files(pattern = ".tif", full.names = TRUE) # list all files for Aug-29 2021 (Julian day 241)
# dl_winter <-list.files(pattern = "351", full.names = TRUE) # for Dec-17 2021 (Julian day 351)

# full 12 band winter
bands_raw <-rast(bands) # create "SpatRaster" for winter image
bands_raw # inspect
plot(bands_raw[[2]], col = gray(0:100 / 100)) # grayscale test plot

# set crop extent
# box <-c(720000,795000,4290240,4400040)
# box_ext <-ext(box)

# test plot using terra's plotRGB function
plotRGB(bands_raw, r = 4, g = 3, b = 2, stretch = "hist")
plotRGB(bands_raw, r = 4, g = 3, b = 2, stretch = "lin")

# create RGB 3 band rasters to save
mar5_rgb <-c(bands_raw[[4]], bands_raw[[3]], bands_raw[[2]])
plot(mar5_rgb)
mar5_rgb[[1]]

# test plot these
plotRGB(banner_rgb, stretch = "lin")
plotRGB(banner_rgb, stretch = "hist", bgalpha = 0, add = TRUE)

# crop to study area
vg <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")
reproj <-project(mar5_rgb, crs(vg))
mar5_vg_rgb <-crop(reproj, ext(vg))
plotRGB(mar5_vg_rgb, stretch = "lin")
plot(vg, add = TRUE)

png("mar5_vg_rgb.png", height=nrow(mar5_vg_rgb), width=ncol(mar5_vg_rgb)) # same dim as raster
plotRGB(mar5_vg_rgb, stretch = "lin", maxcell=ncell(mar5_vg_rgb)) # maxcell key
dev.off()


##### stretch rbg values and convert to 1-255
## not sure i need this but good practice

####################
##### red band #####
####################

# find max and max reflectance
r_min <-as.numeric(global(winter_rgb[[1]], fun= "min", na.rm = TRUE))
r_max <-as.numeric(global(winter_rgb[[1]], fun= "max", na.rm = TRUE))

# convert to 0-255
winter_rgb[[1]] <-((winter_rgb[[1]]+abs(r_min))/(r_max+abs(r_min))*255)
plot(winter_rgb[[1]], col = gray(0:100 / 100)) # test plot

######################
##### green band #####
######################

# find max and max reflectance
g_min <-as.numeric(global(winter_rgb[[2]], fun= "min", na.rm = TRUE))
g_max <-as.numeric(global(winter_rgb[[2]], fun= "max", na.rm = TRUE))

# convert to 0-255
winter_rgb[[2]] <-((winter_rgb[[2]]+abs(g_min))/(g_max+abs(g_min))*255)
plot(winter_rgb[[2]], col = gray(0:100 / 100)) # test plot

######################
##### blue band ######
######################

# find max and max reflectance
b_min <-as.numeric(global(winter_rgb[[3]], fun= "min", na.rm = TRUE))
b_max <-as.numeric(global(winter_rgb[[3]], fun= "max", na.rm = TRUE))

# convert to 0-255
winter_rgb[[3]] <-((winter_rgb[[3]]+abs(b_min))/(b_max+abs(b_min))*255)
plot(winter_rgb[[3]], col = gray(0:100 / 100)) # test plot

##### test plot with converted values
plotRGB(winter_rgb, stretch = "hist") # no difference 

# save rasters
writeRaster(winter_rgb, "winter_rgb.tif")
writeRaster(fall_rgb, "fall_rgb.tif")

### save image at the same resolution as the raster
# https://stackoverflow.com/questions/50953192/r-how-is-it-possible-to-export-from-r-an-image-from-a-raster-layer-while-mainta

png("banner_rgb.png", height=nrow(banner_rgb), width=ncol(banner_rgb)) # same dim as raster
plotRGB(banner_rgb, stretch = "lin", maxcell=ncell(banner_rgb)) # maxcell key
dev.off()
