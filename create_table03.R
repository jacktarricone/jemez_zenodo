# make swe change stats table for manuscript
# jack tarricone
# july 18th, 2022

library(terra)

# set working dir
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/new_swe_change")
list.files()

# vg extent shape file
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

####### load in rasters
# pair 1
feb12_19_r <-rast("dswe_feb12-19_sp.tif")
p1 <-crop(feb12_19_r, vg)

# pair 2
feb19_26_r <-rast("dswe_feb19-26_sp.tif")
p2 <-crop(feb19_26_r, vg)

# pair 3
feb12_26_r <-rast("dswe_feb12-26_sp.tif")
p3 <-crop(feb12_26_r, vg)

# pair 4
feb12_26_cm_r <-rast("dswe_feb12-26_cumulative.tif")
p4 <-crop(feb12_26_cm_r, vg)

### make dataframes
# convert to data frames for plotting
p1_df <-as.data.frame(p1, na.rm = TRUE)
names(p1_df)[1] <-"lyr.1"

p2_df <-as.data.frame(p2, na.rm = TRUE)
names(p2_df)[1] <-"lyr.1"

p3_df <-as.data.frame(p3, na.rm = TRUE)
names(p3_df)[1] <-"lyr.1"

p4_df <-as.data.frame(p4, na.rm = TRUE)
names(p4_df)[1] <-"lyr.1"


#####################
## calculate stats ##
#####################

##########
# pair 1 #
##########

# pair1 stats
p1_mean <-as.numeric(global(p1, mean, na.rm = TRUE))
p1_max <-as.numeric(global(p1, max, na.rm = TRUE))
p1_min <-as.numeric(global(p1, min, na.rm = TRUE))
p1_sd <-as.numeric(global(p1, sd, na.rm = TRUE))

# pair1 percentiles
p1_99 <-as.numeric(quantile(p1_df$lyr.1, c(.99))) 
p1_01 <-as.numeric(quantile(p1_df$lyr.1, c(.01)))

##########
# pair 2 #
##########

# pair2 stats
p2_mean <-as.numeric(global(p2, mean, na.rm = TRUE))
p2_max <-as.numeric(global(p2, max, na.rm = TRUE))
p2_min <-as.numeric(global(p2, min, na.rm = TRUE))
p2_sd <-as.numeric(global(p2, sd, na.rm = TRUE))

# pair2 percentiles
p2_99 <-as.numeric(quantile(p2_df$lyr.1, c(.99))) 
p2_01 <-as.numeric(quantile(p2_df$lyr.1, c(.01)))

##########
# pair 3 #
##########

# pair3 stats
p3_mean <-as.numeric(global(p3, mean, na.rm = TRUE))
p3_max <-as.numeric(global(p3, max, na.rm = TRUE))
p3_min <-as.numeric(global(p3, min, na.rm = TRUE))
p3_sd <-as.numeric(global(p3, sd, na.rm = TRUE))

# pair3 percentiles
p3_99 <-as.numeric(quantile(p3_df$lyr.1, c(.99))) 
p3_01 <-as.numeric(quantile(p3_df$lyr.1, c(.01)))

##########
# pair 4 #
##########

# pair4 stats
p4_mean <-as.numeric(global(p4, mean, na.rm = TRUE))
p4_max <-as.numeric(global(p4, max, na.rm = TRUE))
p4_min <-as.numeric(global(p4, min, na.rm = TRUE))
p4_sd <-as.numeric(global(p4, sd, na.rm = TRUE))

# pair4 percentiles
p4_99 <-as.numeric(quantile(p4_df$lyr.1, c(.99))) 
p4_01 <-as.numeric(quantile(p4_df$lyr.1, c(.01)))


## create dataframe
stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
          "mean" = c(p1_mean, p2_mean, p3_mean, p4_mean),
           "p99" = c(p1_99, p2_99, p3_99, p4_99),
           "p1" = c(p1_01, p2_01, p3_01, p4_01),
           "sd" = c(p1_sd, p2_sd, p3_sd, p4_sd))

stats # check
write.csv(stats, "/Users/jacktarricone/ch1_jemez_data/swe_change_stats.csv")






