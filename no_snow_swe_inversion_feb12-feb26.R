# NO snow inversion for 2/12-2/26 on new JPL produced data
# jack tarricone
# january 10th, 2023

library(terra)
library(ggplot2)

# set home folder
setwd("/Users/jacktarricone/ch1_jemez/gpr_rasters_ryan/")
list.files() #pwd

# import corrected unwrapped phase data 
unw_raw <-rast("./new_swe_change/dswe_feb12-19_sp.tif")
plot(unw_raw)

# import i_angle raster and resample to unw grid bc of slight extent difference
lidar_inc_raw <-rast("lidar_inc_rad.tif")
lidar_inc_v1 <-resample(lidar_inc_raw, unw_raw)

# set crop extent and crop for better visualization
vg_aoi <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")
lidar_inc <-crop(lidar_inc_v1, vg_aoi)

# mask and crop unwrapped phase data down to extent on new inc angle raster
unw_crop <-crop(unw_raw, vg_aoi)
unw <-mask(unw_crop, lidar_inc)

# plot results
plot(lidar_inc)
plot(unw)

####################################
###### bring in fsca layers ########
####################################

# fsca
fsca_raw <-rast("landsat_fsca_2-18.tif")

# crop to .inc angle extent and mask
fsca_crop <-crop(fsca_raw, vg_aoi)
fsca <-mask(fsca_crop, lidar_inc)
plot(fsca)

# create snow mask
snow_mask <-fsca
values(snow_mask)[values(snow_mask) > 15] = 1
plot(snow_mask)
# writeRaster(snow_mask,"02_18_2020_snow_mask.tif")

# masked the unwrapped phase with snow mask
unw_snow <-mask(unw, snow_mask, maskvalue = NA)
unw_no_snow <-mask(unw, snow_mask, maskvalue = NA, inverse = TRUE)
plot(unw)
plot(unw_snow)
plot(unw_no_snow)

########################################################
######### converting phase change to SWE ##############
########################################################

#######################################################################
# don't pick denisty and di_elec value
# pick a density and LWC (from ryans equations and field measurements)
# vary density and LWC over range over measured values
########################################################################

# table ryan sent over
### use NSIDC data
pit_info <-read.csv("/Users/jacktarricone/ch1_jemez/pit_data/perm_pits.csv")
head(pit_info)

# ## define static information from pits
# # calculate density
mean_density_feb12 <- pit_info$mean_density[1]
mean_density_feb26 <- pit_info$mean_density[3]
# 
# # mean density between two flights
mean_density_feb12_26 <-(mean_density_feb12 + mean_density_feb26)/2

# dielctric constant k
k_feb12 <- pit_info$mean_k[1]
k_feb26 <- pit_info$mean_k[3]

# mean k between two flights
mean_k_feb12_26 <-(k_feb12+k_feb26)/2

#######################
#### swe inversion ####
#######################

# first step, define function for insar constant

# inc = incidence angle raster [deg]
# wL = sensor save length [cm]
# k = dielectric permittivty 

# radar wave length from uavsar annotation file
uavsar_wL <- 23.8403545

# import depth_from_phase function
devtools::source_url("https://raw.githubusercontent.com/jacktarricone/snowex_uavsar/master/insar_swe_functions.R")

# snow covered pixels
depth_change <-depth_from_phase(delta_phase = unw_snow,
                                inc_angle = lidar_inc,
                                perm = mean_k_feb12_26,
                                wavelength = uavsar_wL)

plot(depth_change)
hist(depth_change, breaks = 100)

# convert to SWE change
dswe_raw <-depth_change*(mean_density_feb12_26/1000)
plot(dswe_raw)
hist(dswe_raw, breaks = 100)
# writeRaster(dswe_raw,"./final_swe_change/raw_dswe_feb12_26.tif")

# snow covered pixels
no_snow_depth_change <-depth_from_phase(delta_phase = unw_no_snow,
                                inc_angle = lidar_inc,
                                perm = mean_k_feb12_26,
                                wavelength = uavsar_wL)

plot(no_snow_depth_change)
hist(no_snow_depth_change, breaks = 100)

# convert to SWE change
no_snow_dswe_raw <-no_snow_depth_change*(mean_density_feb12_26/1000)
plot(no_snow_dswe_raw)
hist(no_snow_dswe_raw, breaks = 100)


















#######################################
### calculating absolute SWE change ###
#######################################

# using swe change from the pit as "known" change point

# extent around gpr transect
gpr <-ext(-106.5255, -106.521, 35.856, 35.8594)
dswe_crop <-crop(dswe_raw, gpr)
plot(dswe_crop)

# pull out location info into separate df
loc <-data.frame(lat = pit_info$lat[1],
                 lon = pit_info$lon[1])

# plot pit location using terra vector funcitonality
pit_point <-vect(loc, geom = c("lon","lat"), crs = crs(unw))
points(pit_point, cex = 1)

# extract cell number from pit lat/lon point
pit_cell_v1 <-cells(dswe_raw, pit_point)
cell_number <-pit_cell_v1[1,2]
cell_number

# define neighboring cells by number and create a vector
neighbor_cells <-c(adjacent(dswe_raw, cells = cell_number, directions ="8"))

# add orginal cell back to vector
cell_vector <-c(cell_number, neighbor_cells)
cell_vector

# extract using that vector
nine_cell_dswe <-terra::extract(dswe_raw, cell_vector, xy = TRUE)
nine_cell_dswe

# mean of 9 swe changes around
mean_pit_dswe <-mean(nine_cell_dswe[1:9,3])
mean_pit_dswe

# subtract average swe change value to great absolute swe change
# therefor pixels around the pit will show no swe change
# which is consistent with what was observed on the ground
dswe_abs <-dswe_raw - mean_pit_dswe
plot(dswe_abs)
hist(dswe_abs, breaks = 100)

# mask for no snow areas
dswe_no_snow <-mask(dswe_abs, unw_no_snow_mask, maskvalue = NA)
dswe_snow <-mask(dswe_abs, unw_snow_mask, maskvalue = NA)
plot(dswe_no_snow)
hist(dswe_no_snow, breaks = 100)
hist(dswe_snow, breaks = 100)

# mean
mean_snow <-global(dswe_no_snow, mean, na.rm = TRUE)
mean_no <-global(dswe_snow, mean, na.rm = TRUE)

# sd
sd_snow <-global(dswe_no_snow, sd, na.rm = TRUE)
sd_now <-global(dswe_snow, sd, na.rm = TRUE)

# max
max_snow <-global(dswe_no_snow, max, na.rm = TRUE)
max_no <-global(dswe_snow, max, na.rm = TRUE)

# min
min_snow <-global(dswe_no_snow, min, na.rm = TRUE)
min_no <-global(dswe_snow, min, na.rm = TRUE)

# range
range_snow <-global(dswe_no_snow, range, na.rm = TRUE)
range_no <-global(dswe_snow, range, na.rm = TRUE)

# rms
rms_snow <-global(dswe_no_snow, "rms", na.rm = TRUE)
rms_no <-global(dswe_snow, "rms", na.rm = TRUE)


# save
writeRaster(dswe_abs,"./new_swe_change/rough/no_snow_dswe_feb12-26_new.tif")
