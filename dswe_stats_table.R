# make swe change stats table for manuscript
# add vg, north, and south stats
# jack tarricone
# jan 16th, 2022

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
p1_north_v1 <-mask(p1, ns_no_vg, maskvalue = 2) # 2 = south, therefore masking for 2 will leave north
p1_north <-mask(p1_north_v1, vg, inverse = TRUE)
plot(p1_north)

p2_north_v1 <-mask(p2, ns_no_vg, maskvalue = 2) 
p2_north <-mask(p2_north_v1, vg, inverse = TRUE)
plot(p2_north)

p3_north_v1 <-mask(p3, ns_no_vg, maskvalue = 2)
p3_north <-mask(p3_north_v1, vg, inverse = TRUE)

p4_north_v1 <-mask(p4, ns_no_vg, maskvalue = 2) 
p4_north <-mask(p4_north_v1, vg, inverse = TRUE)

# crop for south facing slopes
p1_south_v1 <-mask(p1, ns_no_vg, maskvalue = 1) # 1 = north, therefore masking for 1 will leave south
p1_south <-mask(p1_south_v1, vg, inverse = TRUE)

p2_south_v1 <-mask(p2, ns_no_vg, maskvalue = 1) 
p2_south <-mask(p2_south_v1, vg, inverse = TRUE)

p3_south_v1 <-mask(p3, ns_no_vg, maskvalue = 1)
p3_south <-mask(p3_south_v1, vg, inverse = TRUE)

p4_south_v1 <-mask(p4, ns_no_vg, maskvalue = 1) #
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

p1_mean_fs <-round(as.numeric(global(p1, mean, na.rm = TRUE)), digits = 2)
p1_med_fs <-round(as.numeric(global(p1, median, na.rm = TRUE)), digits = 2)
p1_sd_fs <-round(as.numeric(global(p1, sd, na.rm = TRUE)), digits = 2)
p1_iqr_fs <-round(as.numeric(global(p1, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 2 #
##########

p2_mean_fs <-round(as.numeric(global(p2, mean, na.rm = TRUE)), digits = 2)
p2_med_fs <-round(as.numeric(global(p2, median, na.rm = TRUE)), digits = 2)
p2_sd_fs <-round(as.numeric(global(p2, sd, na.rm = TRUE)), digits = 2)
p2_iqr_fs <-round(as.numeric(global(p2, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 3 #
##########

p3_mean_fs <-round(as.numeric(global(p3, mean, na.rm = TRUE)), digits = 2)
p3_med_fs <-round(as.numeric(global(p3, median, na.rm = TRUE)), digits = 2)
p3_sd_fs <-round(as.numeric(global(p3, sd, na.rm = TRUE)), digits = 2)
p3_iqr_fs <-round(as.numeric(global(p3, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 4 #
##########

p4_mean_fs <-round(as.numeric(global(p4, mean, na.rm = TRUE)), digits = 2)
p4_med_fs <-round(as.numeric(global(p4, median, na.rm = TRUE)), digits = 2)
p4_sd_fs <-round(as.numeric(global(p4, sd, na.rm = TRUE)), digits = 2)
p4_iqr_fs <-round(as.numeric(global(p4, IQR, na.rm = TRUE)), digits = 2)

## create dataframe
fs_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
          "mean" = c(p1_mean_fs, p2_mean_fs, p3_mean_fs, p4_mean_fs),
          "sd" = c(p1_sd_fs, p2_sd_fs, p3_sd_fs, p4_sd_fs),
          "med" = c(p1_med_fs, p2_med_fs, p3_med_fs, p4_med_fs),
          "iqr" = c(p1_iqr_fs, p2_iqr_fs, p3_iqr_fs, p4_iqr_fs))

fs_stats # check

write.csv(fs_stats, "/Users/jacktarricone/ch1_jemez/pit_data/full_scene_swe_stats.csv")

##############
###   vg   ###
##############

##########
# pair 1 #
##########

p1_mean_vg <-round(as.numeric(global(p1_vg, mean, na.rm = TRUE)), digits = 2)
p1_med_vg <-round(as.numeric(global(p1_vg, median, na.rm = TRUE)), digits = 2)
p1_sd_vg <-round(as.numeric(global(p1_vg, sd, na.rm = TRUE)), digits = 2)
p1_iqr_vg <-round(as.numeric(global(p1_vg, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 2 #
##########

p2_mean_vg <-round(as.numeric(global(p2_vg, mean, na.rm = TRUE)), digits = 2)
p2_med_vg <-round(as.numeric(global(p2_vg, median, na.rm = TRUE)), digits = 2)
p2_sd_vg <-round(as.numeric(global(p2_vg, sd, na.rm = TRUE)), digits = 2)
p2_iqr_vg <-round(as.numeric(global(p2_vg, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 3 #
##########

p3_mean_vg <-round(as.numeric(global(p3_vg, mean, na.rm = TRUE)), digits = 2)
p3_med_vg <-round(as.numeric(global(p3_vg, median, na.rm = TRUE)), digits = 2)
p3_sd_vg <-round(as.numeric(global(p3_vg, sd, na.rm = TRUE)), digits = 2)
p3_iqr_vg <-round(as.numeric(global(p3_vg, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 4 #
##########

p4_mean_vg <-round(as.numeric(global(p4_vg, mean, na.rm = TRUE)), digits = 2)
p4_med_vg <-round(as.numeric(global(p4_vg, median, na.rm = TRUE)), digits = 2)
p4_sd_vg <-round(as.numeric(global(p4_vg, sd, na.rm = TRUE)), digits = 2)
p4_iqr_vg <-round(as.numeric(global(p4_vg, IQR, na.rm = TRUE)), digits = 2)

## create dataframe
vg_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                      "mean" = c(p1_mean_vg, p2_mean_vg, p3_mean_vg, p4_mean_vg),
                      "sd" = c(p1_sd_vg, p2_sd_vg, p3_sd_vg, p4_sd_vg),
                      "med" = c(p1_med_vg, p2_med_vg, p3_med_vg, p4_med_vg),
                      "iqr" = c(p1_iqr_vg, p2_iqr_vg, p3_iqr_vg, p4_iqr_vg))

vg_stats # check
fs_stats

write.csv(fs_stats, "/Users/jacktarricone/ch1_jemez/pit_data/vg_scene_swe_stats.csv")

##############
### north  ###
##############

# pair 1 #
##########

p1_mean_north <-round(as.numeric(global(p1_north, mean, na.rm = TRUE)), digits = 2)
p1_med_north <-round(as.numeric(global(p1_north, median, na.rm = TRUE)), digits = 2)
p1_sd_north <-round(as.numeric(global(p1_north, sd, na.rm = TRUE)), digits = 2)
p1_iqr_north <-round(as.numeric(global(p1_north, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 2 #
##########

p2_mean_north <-round(as.numeric(global(p2_north, mean, na.rm = TRUE)), digits = 2)
p2_med_north <-round(as.numeric(global(p2_north, median, na.rm = TRUE)), digits = 2)
p2_sd_north <-round(as.numeric(global(p2_north, sd, na.rm = TRUE)), digits = 2)
p2_iqr_north <-round(as.numeric(global(p2_north, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 3 #
##########

p3_mean_north <-round(as.numeric(global(p3_north, mean, na.rm = TRUE)), digits = 2)
p3_med_north <-round(as.numeric(global(p3_north, median, na.rm = TRUE)), digits = 2)
p3_sd_north <-round(as.numeric(global(p3_north, sd, na.rm = TRUE)), digits = 2)
p3_iqr_north <-round(as.numeric(global(p3_north, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 4 #
##########

p4_mean_north <-round(as.numeric(global(p4_north, mean, na.rm = TRUE)), digits = 2)
p4_med_north <-round(as.numeric(global(p4_north, median, na.rm = TRUE)), digits = 2)
p4_sd_north <-round(as.numeric(global(p4_north, sd, na.rm = TRUE)), digits = 2)
p4_iqr_north <-round(as.numeric(global(p4_north, IQR, na.rm = TRUE)), digits = 2)

## create dataframe
north_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                      "mean" = c(p1_mean_north, p2_mean_north, p3_mean_north, p4_mean_north),
                      "sd" = c(p1_sd_north, p2_sd_north, p3_sd_north, p4_sd_north),
                      "med" = c(p1_med_north, p2_med_north, p3_med_north, p4_med_north),
                      "iqr" = c(p1_iqr_north, p2_iqr_north, p3_iqr_north, p4_iqr_north))

vg_stats # check
fs_stats
north_stats

write.csv(north_stats, "/Users/jacktarricone/ch1_jemez/pit_data/north_swe_stats.csv")

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

write.csv(south_stats, "/Users/jacktarricone/ch1_jemez/pit_data/south_swe_stats.csv")


