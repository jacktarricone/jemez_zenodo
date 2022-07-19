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
stats <-data.frame("name" = c("pair1", "pair2", "cum"),
          "mean" = c(pair1_mean, pair2_mean, cum_mean),
           "99" = c(p1_99, p2_99, cum_99),
           ".01" = c(p1_01, p2_01, cum_01),
           "sd" = c(pair1_sd, pair2_sd, cum_sd))

p1_df <-as.data.frame(pair1)
quantile(df$lyr.1, c(.01, .99)) 

# hh
hh_list <-list.files(pattern = "*HH_01.unw*")
hh <-rast(hh_list[1])
values(hh)[values(hh) == 0] <-NA
plot(hh)
hh_freq <-as.data.frame(freq(hh, digits = 3))
hh_pixels <-sum(hh_freq$count)
hh_perc_lost <-100-(hh_pixels/total_pixels)*100

# hv
hv_list <-list.files(pattern = "*HV_01.unw*")
hv <-rast(hv_list[1])
values(hv)[values(hv) == 0] <-NA
plot(hv)
hv_freq <-as.data.frame(freq(hv, digits = 3))
hv_pixels <-sum(hv_freq$count)
hv_perc_lost <-100-(hv_pixels/total_pixels)*100

# vh
vh_list <-list.files(pattern = "*VH_01.unw*")
vh <-rast(vh_list[1])
values(vh)[values(vh) == 0] <-NA
plot(vh)
vh_freq <-as.data.frame(freq(vh, digits = 3))
vh_pixels <-sum(vh_freq$count)
vh_perc_lost <-100-(vh_pixels/total_pixels)*100


# vv
vv_list <-list.files(pattern = "*VV_01.unw*")
vv <-rast(vv_list[1])
values(vv)[values(vv) == 0] <-NA
plot(vv)
vv_freq <-as.data.frame(freq(vv, digits = 3))
vv_pixels <-sum(vv_freq$count)
vv_perc_lost <-100-(vv_pixels/total_pixels)*100

## bring in vallee grand wkt
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

hh_list <-list.files(pattern = "*HH_01.cor*")
hh_cor <-rast(hh_list[1])
hh_cor_vg_v1 <-mask(hh_cor, vg)
hh_cor_vg <-crop(hh_cor_vg_v1, vg)
plot(hh_cor_vg)
fr_vg <-as.data.frame(freq(hh_cor_vg, digits = 3))
total_pixels_vg <-sum(fr_vg$count)


# hh
hh_vg_v1 <-mask(hh, vg)
hh_vg <-crop(hh_vg_v1, vg)
plot(hh_vg)
hh_vg_freq <-as.data.frame(freq(hh_vg, digits = 3))
hh_vg_pixels <-sum(hh_vg_freq$count)
hh_perc_lost_vg <-100-(hh_vg_pixels/total_pixels_vg)*100


# hv
hv_vg_v1 <-mask(hv, vg)
hv_vg <-crop(hv_vg_v1, vg)
plot(hv_vg)
hv_vg_freq <-as.data.frame(freq(hv_vg, digits = 3))
hv_vg_pixels <-sum(hv_vg_freq$count)
hv_perc_lost_vg <-100-(hv_vg_pixels/total_pixels_vg)*100

# vh
vh_vg_v1 <-mask(vh, vg)
vh_vg <-crop(vh_vg_v1, vg)
plot(vh_vg)
vh_vg_freq <-as.data.frame(freq(vh_vg, digits = 3))
vh_vg_pixels <-sum(vh_vg_freq$count)
vh_perc_lost_vg <-100-(vh_vg_pixels/total_pixels_vg)*100


# vv
vv_vg_v1 <-mask(vv, vg)
vv_vg <-crop(vv_vg_v1, vg)
plot(vv_vg)
vv_vg_freq <-as.data.frame(freq(vv_vg, digits = 3))
vv_vg_pixels <-sum(vv_vg_freq$count)
vv_perc_lost_vg <-100-(vv_vg_pixels/total_pixels_vg)*100

