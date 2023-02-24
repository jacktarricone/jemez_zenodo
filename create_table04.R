# make swe change stats table for manuscript
# jack tarricone
# july 18th, 2022

library(terra)

# set working dir
setwd("/Users/jacktarricone/ch1_jemez/gpr_rasters_ryan/new_swe_change")
list.files()

# big in vg_aoi, vg_mask
vg_aoi <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")
vg <-vect("/Users/jacktarricone/ch1_jemez/vector_data/vg_shp.geojson")

# bring in north south raster and crop
lidar_ns_v1 <-rast("/Users/jacktarricone/ch1_jemez/jemez_lidar/ns_aspect.tif")
lidar_ns <-crop(lidar_ns_v1, vg_aoi)

# test plot
plot(lidar_ns)
plot(vg_aoi, add = TRUE)
plot(vg, add = TRUE)

# mask ns with for area outside vg
ns_no_vg <-mask(lidar_ns, vg, inverse = TRUE)
plot(ns_no_vg)

####### load in rasters
# pair 1
feb12_19_r <-rast("dswe_feb12-19_sp.tif")
p1 <-crop(feb12_19_r, vg_aoi)

# pair 2
feb19_26_r <-rast("dswe_feb19-26_sp.tif")
p2 <-crop(feb19_26_r, vg_aoi)

# pair 3
feb12_26_r <-rast("dswe_feb12-26_sp.tif")
p3 <-crop(feb12_26_r, vg_aoi)

# pair 4
feb12_26_cm_r <-rast("dswe_feb12-26_cumulative.tif")
p4 <-crop(feb12_26_cm_r, vg_aoi)

## crop for vg
p1_vg <-mask(p1, vg)
p2_vg <-mask(p2, vg)
p3_vg <-mask(p3, vg)
p4_vg <-mask(p4, vg)

# crop for north facing slopes
p1_north_v1 <-mask(p1, ns_no_vg, maskvalue = 1) # north = 1
p1_north <-mask(p1_north_v1, vg, inverse = TRUE)

p2_north_v1 <-mask(p2, ns_no_vg, maskvalue = 1) # north = 1
p2_north <-mask(p2_north_v1, vg, inverse = TRUE)

p3_north_v1 <-mask(p3, ns_no_vg, maskvalue = 1) # north = 1
p3_north <-mask(p3_north_v1, vg, inverse = TRUE)

p4_north_v1 <-mask(p4, ns_no_vg, maskvalue = 1) # north = 1
p4_north <-mask(p4_north_v1, vg, inverse = TRUE)

# crop for south facing slopes
p1_south_v1 <-mask(p1, ns_no_vg, maskvalue = 2) # south = 1
p1_south <-mask(p1_south_v1, vg, inverse = TRUE)

p2_south_v1 <-mask(p2, ns_no_vg, maskvalue = 2) # south = 1
p2_south <-mask(p2_south_v1, vg, inverse = TRUE)

p3_south_v1 <-mask(p3, ns_no_vg, maskvalue = 2) # south = 1
p3_south <-mask(p3_south_v1, vg, inverse = TRUE)

p4_south_v1 <-mask(p4, ns_no_vg, maskvalue = 2) # south = 1
p4_south <-mask(p4_south_v1, vg, inverse = TRUE)


### make dataframes

##### full scence
p1_fs_df <-as.data.frame(p1, na.rm = TRUE)
names(p1_fs_df)[1] <-"lyr.1"

p2_fs_df <-as.data.frame(p2, na.rm = TRUE)
names(p2_fs_df)[1] <-"lyr.1"

p3_fs_df <-as.data.frame(p3, na.rm = TRUE)
names(p3_fs_df)[1] <-"lyr.1"

p4_fs_df <-as.data.frame(p4, na.rm = TRUE)
names(p4_fs_df)[1] <-"lyr.1"

######## vg
p1_vg_df <-as.data.frame(p1_vg, na.rm = TRUE)
names(p1_vg_df)[1] <-"lyr.1"

p2_vg_df <-as.data.frame(p2_vg, na.rm = TRUE)
names(p2_vg_df)[1] <-"lyr.1"

p3_vg_df <-as.data.frame(p3_vg, na.rm = TRUE)
names(p3_vg_df)[1] <-"lyr.1"

p4_vg_df <-as.data.frame(p4_vg, na.rm = TRUE)
names(p4_vg_df)[1] <-"lyr.1"

######## north facing
p1_north_df <-as.data.frame(p1_north, na.rm = TRUE)
names(p1_north_df)[1] <-"lyr.1"

p2_north_df <-as.data.frame(p2_north, na.rm = TRUE)
names(p2_north_df)[1] <-"lyr.1"

p3_north_df <-as.data.frame(p3_north, na.rm = TRUE)
names(p3_north_df)[1] <-"lyr.1"

p4_north_df <-as.data.frame(p4_north, na.rm = TRUE)
names(p4_north_df)[1] <-"lyr.1"

######## south facing
p1_south_df <-as.data.frame(p1_south, na.rm = TRUE)
names(p1_south_df)[1] <-"lyr.1"

p2_south_df <-as.data.frame(p2_south, na.rm = TRUE)
names(p2_south_df)[1] <-"lyr.1"

p3_south_df <-as.data.frame(p3_south, na.rm = TRUE)
names(p3_south_df)[1] <-"lyr.1"

p4_south_df <-as.data.frame(p4_south, na.rm = TRUE)
names(p4_south_df)[1] <-"lyr.1"

#####################
## calculate stats ##
#####################

##################
### full scene ###
##################

##########
# pair 1 #
##########

# pair1 stats
p1_mean_fs <-round(as.numeric(global(p1, mean, na.rm = TRUE)), digits = 2)
p1_max_fs <-round(as.numeric(global(p1, max, na.rm = TRUE)), digits = 2)
p1_min_fs <-round(as.numeric(global(p1, min, na.rm = TRUE)), digits = 2)
p1_sd_fs <-round(as.numeric(global(p1, sd, na.rm = TRUE)), digits = 2)

# pair1 percentiles
p1_99_fs <-as.numeric(quantile(p1_fs_df$lyr.1, c(.99))) 
p1_01_fs <-as.numeric(quantile(p1_fs_df$lyr.1, c(.01)))

##########
# pair 2 #
##########

# pair2 stats
p2_mean_fs <-round(as.numeric(global(p2, mean, na.rm = TRUE)), digits = 2)
p2_max_fs <-round(as.numeric(global(p2, max, na.rm = TRUE)), digits = 2)
p2_min_fs <-round(as.numeric(global(p2, min, na.rm = TRUE)), digits = 2)
p2_sd_fs <-round(as.numeric(global(p2, sd, na.rm = TRUE)), digits = 2)

# pair2 percentiles
p2_99_fs <-as.numeric(quantile(p2_fs_df$lyr.1, c(.99))) 
p2_01_fs <-as.numeric(quantile(p2_fs_df$lyr.1, c(.01)))

##########
# pair 3 #
##########

# pair3 stats
p3_mean_fs <-round(as.numeric(global(p3, mean, na.rm = TRUE)), digits = 2)
p3_max_fs <-round(as.numeric(global(p3, max, na.rm = TRUE)), digits = 2)
p3_min_fs <-round(as.numeric(global(p3, min, na.rm = TRUE)), digits = 2)
p3_sd_fs <-round(as.numeric(global(p3, sd, na.rm = TRUE)), digits = 2)

# pair3 percentiles
p3_99_fs <-as.numeric(quantile(p3_fs_df$lyr.1, c(.99))) 
p3_01_fs <-as.numeric(quantile(p3_fs_df$lyr.1, c(.01)))

##########
# pair 4 #
##########

# pair4 stats
p4_mean_fs <-round(as.numeric(global(p4, mean, na.rm = TRUE)), digits = 2)
p4_max_fs <-round(as.numeric(global(p4, max, na.rm = TRUE)), digits = 2)
p4_min_fs <-round(as.numeric(global(p4, min, na.rm = TRUE)), digits = 2)
p4_sd_fs <-round(as.numeric(global(p4, sd, na.rm = TRUE)), digits = 2)

# pair4 percentiles
p4_99_fs <-as.numeric(quantile(p4_fs_df$lyr.1, c(.99)))
p4_01_fs <-as.numeric(quantile(p4_fs_df$lyr.1, c(.01)))


## create dataframe
fs_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
          "mean" = c(p1_mean_fs, p2_mean_fs, p3_mean_fs, p4_mean_fs),
           "p99" = c(p1_99_fs, p2_99_fs, p3_99_fs, p4_99_fs),
           "p1" = c(p1_01_fs, p2_01_fs, p3_01_fs, p4_01_fs),
           "sd" = c(p1_sd_fs, p2_sd_fs, p3_sd_fs, p4_sd_fs))

fs_stats # check

# write.csv(fs_stats, "/Users/jacktarricone/ch1_jemez/pit_data/full_scene_swe_stats.csv")

##############
###   vg   ###
##############

##########
# pair 1 #
##########

# pair1 stats
p1_mean_vg <-round(as.numeric(global(p1_vg, mean, na.rm = TRUE)), digits = 2)
p1_max_vg <-round(as.numeric(global(p1_vg, max, na.rm = TRUE)), digits = 2)
p1_min_vg <-round(as.numeric(global(p1_vg, min, na.rm = TRUE)), digits = 2)
p1_sd_vg <-round(as.numeric(global(p1_vg, sd, na.rm = TRUE)), digits = 2)

# pair1 percentiles
p1_99_vg <-as.numeric(quantile(p1_vg_df$lyr.1, c(.99))) 
p1_01_vg <-as.numeric(quantile(p1_vg_df$lyr.1, c(.01)))

##########
# pair 2 #
##########

# pair2 stats
p2_mean_vg <-round(as.numeric(global(p2_vg, mean, na.rm = TRUE)), digits = 2)
p2_max_vg <-round(as.numeric(global(p2_vg, max, na.rm = TRUE)), digits = 2)
p2_min_vg <-round(as.numeric(global(p2_vg, min, na.rm = TRUE)), digits = 2)
p2_sd_vg <-round(as.numeric(global(p2_vg, sd, na.rm = TRUE)), digits = 2)

# pair2 percentiles
p2_99_vg <-as.numeric(quantile(p2_vg_df$lyr.1, c(.99))) 
p2_01_vg <-as.numeric(quantile(p2_vg_df$lyr.1, c(.01)))

##########
# pair 3 #
##########

# pair3 stats
p3_mean_vg <-round(as.numeric(global(p3_vg, mean, na.rm = TRUE)), digits = 2)
p3_max_vg <-round(as.numeric(global(p3_vg, max, na.rm = TRUE)), digits = 2)
p3_min_vg <-round(as.numeric(global(p3_vg, min, na.rm = TRUE)), digits = 2)
p3_sd_vg <-round(as.numeric(global(p3_vg, sd, na.rm = TRUE)), digits = 2)

# pair3 percentiles
p3_99_vg <-as.numeric(quantile(p3_vg_df$lyr.1, c(.99))) 
p3_01_vg <-as.numeric(quantile(p3_vg_df$lyr.1, c(.01)))

##########
# pair 4 #
##########

# pair4 stats
p4_mean_vg <-round(as.numeric(global(p4_vg, mean, na.rm = TRUE)), digits = 2)
p4_max_vg <-round(as.numeric(global(p4_vg, max, na.rm = TRUE)), digits = 2)
p4_min_vg <-round(as.numeric(global(p4_vg, min, na.rm = TRUE)), digits = 2)
p4_sd_vg <-round(as.numeric(global(p4_vg, sd, na.rm = TRUE)), digits = 2)

# pair4 percentiles
p4_99_vg <-as.numeric(quantile(p4_vg_df$lyr.1, c(.99)))
p4_01_vg <-as.numeric(quantile(p4_vg_df$lyr.1, c(.01)))


## create dataframe
vg_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                      "mean" = c(p1_mean_vg, p2_mean_vg, p3_mean_vg, p4_mean_vg),
                      "p99" = c(p1_99_vg, p2_99_vg, p3_99_vg, p4_99_vg),
                      "p1" = c(p1_01_vg, p2_01_vg, p3_01_vg, p4_01_vg),
                      "sd" = c(p1_sd_vg, p2_sd_vg, p3_sd_vg, p4_sd_vg))

vg_stats # check

# write.csv(vg_stats, "/Users/jacktarricone/ch1_jemez/pit_data/vg_swe_stats.csv")

##############
### north  ###
##############

##########
# pair 1 #
##########

# pair1 stats
p1_mean_north <-round(as.numeric(global(p1_north, mean, na.rm = TRUE)), digits = 2)
p1_max_north <-round(as.numeric(global(p1_north, max, na.rm = TRUE)), digits = 2)
p1_min_north <-round(as.numeric(global(p1_north, min, na.rm = TRUE)), digits = 2)
p1_sd_north <-round(as.numeric(global(p1_north, sd, na.rm = TRUE)), digits = 2)

# pair1 percentiles
p1_99_north <-as.numeric(quantile(p1_north_df$lyr.1, c(.99))) 
p1_01_north <-as.numeric(quantile(p1_north_df$lyr.1, c(.01)))

##########
# pair 2 #
##########

# pair2 stats
p2_mean_north <-round(as.numeric(global(p2_north, mean, na.rm = TRUE)), digits = 2)
p2_max_north <-round(as.numeric(global(p2_north, max, na.rm = TRUE)), digits = 2)
p2_min_north <-round(as.numeric(global(p2_north, min, na.rm = TRUE)), digits = 2)
p2_sd_north <-round(as.numeric(global(p2_north, sd, na.rm = TRUE)), digits = 2)

# pair2 percentiles
p2_99_north <-as.numeric(quantile(p2_north_df$lyr.1, c(.99))) 
p2_01_north <-as.numeric(quantile(p2_north_df$lyr.1, c(.01)))

##########
# pair 3 #
##########

# pair3 stats
p3_mean_north <-round(as.numeric(global(p3_north, mean, na.rm = TRUE)), digits = 2)
p3_max_north <-round(as.numeric(global(p3_north, max, na.rm = TRUE)), digits = 2)
p3_min_north <-round(as.numeric(global(p3_north, min, na.rm = TRUE)), digits = 2)
p3_sd_north <-round(as.numeric(global(p3_north, sd, na.rm = TRUE)), digits = 2)

# pair3 percentiles
p3_99_north <-as.numeric(quantile(p3_north_df$lyr.1, c(.99))) 
p3_01_north <-as.numeric(quantile(p3_north_df$lyr.1, c(.01)))

##########
# pair 4 #
##########

# pair4 stats
p4_mean_north <-round(as.numeric(global(p4_north, mean, na.rm = TRUE)), digits = 2)
p4_max_north <-round(as.numeric(global(p4_north, max, na.rm = TRUE)), digits = 2)
p4_min_north <-round(as.numeric(global(p4_north, min, na.rm = TRUE)), digits = 2)
p4_sd_north <-round(as.numeric(global(p4_north, sd, na.rm = TRUE)), digits = 2)

# pair4 percentiles
p4_99_north <-as.numeric(quantile(p4_north_df$lyr.1, c(.99)))
p4_01_north <-as.numeric(quantile(p4_north_df$lyr.1, c(.01)))


## create dataframe
north_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                      "mean" = c(p1_mean_north, p2_mean_north, p3_mean_north, p4_mean_north),
                      "p99" = c(p1_99_north, p2_99_north, p3_99_north, p4_99_north),
                      "p1" = c(p1_01_north, p2_01_north, p3_01_north, p4_01_north),
                      "sd" = c(p1_sd_north, p2_sd_north, p3_sd_north, p4_sd_north))

north_stats # check

# write.csv(north_stats, "/Users/jacktarricone/ch1_jemez/pit_data/north_swe_stats.csv")

##############
### south  ###
##############

##########
# pair 1 #
##########

# pair1 stats
p1_mean_south <-round(as.numeric(global(p1_south, mean, na.rm = TRUE)), digits = 2)
p1_max_south <-round(as.numeric(global(p1_south, max, na.rm = TRUE)), digits = 2)
p1_min_south <-round(as.numeric(global(p1_south, min, na.rm = TRUE)), digits = 2)
p1_sd_south <-round(as.numeric(global(p1_south, sd, na.rm = TRUE)), digits = 2)

# pair1 percentiles
p1_99_south <-as.numeric(quantile(p1_south_df$lyr.1, c(.99))) 
p1_01_south <-as.numeric(quantile(p1_south_df$lyr.1, c(.01)))

##########
# pair 2 #
##########

# pair2 stats
p2_mean_south <-round(as.numeric(global(p2_south, mean, na.rm = TRUE)), digits = 2)
p2_max_south <-round(as.numeric(global(p2_south, max, na.rm = TRUE)), digits = 2)
p2_min_south <-round(as.numeric(global(p2_south, min, na.rm = TRUE)), digits = 2)
p2_sd_south <-round(as.numeric(global(p2_south, sd, na.rm = TRUE)), digits = 2)

# pair2 percentiles
p2_99_south <-as.numeric(quantile(p2_south_df$lyr.1, c(.99))) 
p2_01_south <-as.numeric(quantile(p2_south_df$lyr.1, c(.01)))

##########
# pair 3 #
##########

# pair3 stats
p3_mean_south <-round(as.numeric(global(p3_south, mean, na.rm = TRUE)), digits = 2)
p3_max_south <-round(as.numeric(global(p3_south, max, na.rm = TRUE)), digits = 2)
p3_min_south <-round(as.numeric(global(p3_south, min, na.rm = TRUE)), digits = 2)
p3_sd_south <-round(as.numeric(global(p3_south, sd, na.rm = TRUE)), digits = 2)

# pair3 percentiles
p3_99_south <-as.numeric(quantile(p3_south_df$lyr.1, c(.99))) 
p3_01_south <-as.numeric(quantile(p3_south_df$lyr.1, c(.01)))

##########
# pair 4 #
##########

# pair4 stats
p4_mean_south <-round(as.numeric(global(p4_south, mean, na.rm = TRUE)), digits = 2)
p4_max_south <-round(as.numeric(global(p4_south, max, na.rm = TRUE)), digits = 2)
p4_min_south <-round(as.numeric(global(p4_south, min, na.rm = TRUE)), digits = 2)
p4_sd_south <-round(as.numeric(global(p4_south, sd, na.rm = TRUE)), digits = 2)

# pair4 percentiles
p4_99_south <-as.numeric(quantile(p4_south_df$lyr.1, c(.99)))
p4_01_south <-as.numeric(quantile(p4_south_df$lyr.1, c(.01)))


## create dataframe
south_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                         "mean" = c(p1_mean_south, p2_mean_south, p3_mean_south, p4_mean_south),
                         "p99" = c(p1_99_south, p2_99_south, p3_99_south, p4_99_south),
                         "p1" = c(p1_01_south, p2_01_south, p3_01_south, p4_01_south),
                         "sd" = c(p1_sd_south, p2_sd_south, p3_sd_south, p4_sd_south))

south_stats # check
north_stats

write.csv(south_stats, "/Users/jacktarricone/ch1_jemez/pit_data/south_swe_stats.csv")




