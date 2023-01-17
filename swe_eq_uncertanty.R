# swe uncertanity variation start
# using snow free pixels
# jack tarricone
# january 16, 2023

library(terra)

setwd("/Users/jacktarricone/ch1_jemez/")

# big in vg_aoi, vg_mask
vg_aoi <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")

# bring 12-19 phase data
p1_unw <-rast("./gpr_rasters_ryan/unw_feb19-feb26.tif")
plot(p1_unw)
fsca_mask_0218

# bring in jpl inc rad
jpl_inc_v1 <-rast("./gpr_rasters_ryan/jpl_inc_rad.tiff")
jpl_inc <-resample(jpl_inc_v1, p1_unw)
plot(jpl_inc)

# bring in fsca mask
# 18 feb
fsca_mask_0218 <-rast("./landsat_fsca/0218_mask.tif")
values(fsca_mask_0218)[values(fsca_mask_0218) < -998] = 999

# 5 march
fsca_mask_0305<-rast("./landsat_fsca/0305_mask.tif")


# resample scale down to 5.6 m
fsca1_resamp <-resample(fsca_mask_0218, p1_unw)
plot(fsca1_resamp, add = TRUE)

# mask phase
p1_unw_nosnow <-mask(p1_unw, fsca1_resamp, maskvalue = NA)
plot(p1_unw_nosnow)

hist(p1_unw_nosnow, breaks = 100)

# import depth_from_phase function
devtools::source_url("https://raw.githubusercontent.com/jacktarricone/snowex_uavsar/master/insar_swe_functions.R")

# radar wave length from uavsar annotation file
uavsar_wL <- 23.8403545

# convert to depth
depth_change <-depth_from_phase(delta_phase = p1_unw_nosnow,
                                inc_angle = jpl_inc,
                                perm = 1.3,
                                wavelength = uavsar_wL)
plot(depth_change)

# raw swe change
dswe_raw <-depth_change*(290/1000)
plot(dswe_raw)
hist(dswe_raw, breaks = 100)

dswe_raw_df <-as.data.frame(dswe_raw)
colnames(dswe_raw_df)[1] <-"dswe"
head(dswe_raw_df)
mean_fs <-mean(dswe_raw_df$dswe)
sd_fs <-sd(dswe_raw_df$dswe)

# crop to vg_aoi
dswe_raw_vg_aoi <-crop(dswe_raw, vg_aoi)
plot(dswe_raw_vg_aoi)
hist(dswe_raw_vg_aoi, breaks = 100)

dswe_vg_df <-as.data.frame(dswe_raw_vg_aoi)
colnames(dswe_vg_df)[1] <-"dswe"
head(dswe_vg_df)
mean_vg <-mean(dswe_vg_df$dswe)
sd_vg <-sd(dswe_vg_df$dswe)

