# fsca transformation 
# march 3rd
# comparing change in fsca to delta swe
# using native 30m

library(terra)
library(sf)

# uavsar swath extent shapefile
uavsar_shp <-vect("/Users/jacktarricone/ch1_jemez/vector_data/jpl_dem_extent.shp")

# feb 18th 2020
# bringg in raw fSCA feb 18th data downloaded from landsat web portal
fsca_0218_raw_v1 <-rast("/Users/jacktarricone/ch1_jemez/landsat_fsca/feb_18/LC08_CU_010012_20200218_20200227_C01_V01_SNOW.tif")
fsca_0218_raw_v2 <-fsca_0218_raw_v1/10 # correct [%] scale
plot(fsca_0218_raw_v2)

# make no pixel mask for 2/18
fsca_0218_raw_v3 <-subst(fsca_0218_raw_v2,0,-999) # sub -999 for 0
values(fsca_0218_raw_v3)[values(fsca_0218_raw_v3) > 0] = NA # if above 0 then NA
mask_v1 <-project(fsca_0218_raw_v3, "EPSG:4326") # project
mask_v2 <-mask(mask_v1, uavsar_shp) # mask
mask_v3 <-crop(mask_v2, ext(uavsar_shp)) # crop ext
plot(mask_v3)
writeRaster(mask_v3, "/Users/jacktarricone/ch1_jemez/landsat_fsca/0218_mask.tif")


# march 5th 2020
# bring in raw fSCA feb 18th data downloaded from landsat web portal
fsca_0305_raw_v1 <-rast("/Users/jacktarricone/ch1_jemez/landsat_fsca/fsca_mar_05/fsac_mar_05.tif")
fsca_0305_raw_v2 <-fsca_0305_raw_v1/10 # correct [%] scale
plot(fsca_0305_raw_v2)

# make no pixel mask for 3/5
fsca_0305_raw_v3 <-subst(fsca_0305_raw_v2,0,-999) # sub -999 for 0
values(fsca_0305_raw_v3)[values(fsca_0305_raw_v3) > 0] = NA # if above 0 then NA
test <-project(fsca_0305_raw_v3, "EPSG:4326") # project
test_v1 <-mask(test, uavsar_shp) # mask
test_v2 <-crop(test_v1, ext(uavsar_shp)) # crop ext
plot(test_v2)
writeRaster(test_v2, "/Users/jacktarricone/ch1_jemez/landsat_fsca/0305_mask.tif")

# create delta fsca product
dfsca <-fsca_0305_raw_v2 - fsca_0218_raw_v2
plot(dfsca)

# reproject to lat/lon
dfsca <-project(dfsca, "EPSG:4326")
dsca_m <-mask(dfsca, uavsar_shp) # mask
dfsca_cm <-crop(dsca_m, ext(uavsar_shp)) # crop ext
plot(dfsca_cm)
writeRaster(dfsca_cm, "/Users/jacktarricone/ch1_jemez/landsat_fsca/dfsca_uavsar_v2.tif")

#bring in DEM
dswe_cm <-rast("/Users/jacktarricone/ch1_jemez/gpr_rasters_ryan/new_swe_change/dswe_feb12-26_sp.tif")
dswe_cm
plot(dswe_cm)

# crop to extent of SWE data
dfsca_crop <-crop(dfsca, ext(dswe_cm))
plot(dfsca_crop)
# writeRaster(dfsca_crop, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dfsca_30m.tif")

# resample SWE data up to 30m landsat
dswe_cum30m <-resample(dswe_cm, dfsca_crop, method = "bilinear")
dswe_cum30m
plot(dswe_cum30m)
#writeRaster(dswe_cum30m, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/dswe_cm_30m.tif")


# mask for missing pixels in SWE data
dfsca_crop_mask <-mask(dfsca_crop, dswe_cum30m)
plot(dfsca_crop_mask)

# check both files
dswe_cum30m
dfsca_crop_mask


# mask for non 0 fsca pixels
dfsca_no0 <-dfsca_crop_mask
# values(dfsca_no0)[values(dfsca_no0) > 0] <- NA
values(dfsca_no0)[values(dfsca_no0) == 0] <- NA
plot(dfsca_no0)

# mask the swe raster
dswe_no0 <-mask(dswe_cum30m, dfsca_no0, maskvalue = NA)
dswe_no0
# values(dswe_no0)[values(dswe_no0) > -2] <- NA
# dfsca_no0_v1 <-mask(dfsca_no0, dswe_no0)
plot(dswe_no0)

####### now we can plot

# read in jemez valle grande extent
jemez_wkt <-read_sf("/Users/jacktarricone/ch1_jemez_data/vector_data/jemez_ext.geojson")

# crop fsca and swe down to just VG
dswe_vg <-crop(dswe_no0, jemez_wkt)
dfsca_vg <-crop(dfsca_no0, jemez_wkt)

# bring in canopy cover for masking
cc_v1 <-rast("/Users/jacktarricone/ch1_jemez_data/nlcd/NLCD_2016_Tree_Canopy_L48_20190831_Y6k85Ek4KV2m251VVyWr.tiff")
plot(cc_v1)

# reproject
cc_v2 <- project(cc_v1, "EPSG:4326")
plot(cc_v2)

# crop and resample down to VG ext
cc_v3 <-crop(cc_v2, jemez_wkt)
cc_v4 <-resample(cc_v3, dfsca_vg)
plot(cc_v4)

# test masking value
cc_v5 <-cc_v4
# values(cc_v5)[values(cc_v5) == 0] <- NA
values(cc_v5)[values(cc_v5) > 10] <- -999
plot(cc_v5)

## mask for canopy cover
# swe
dswe_vg_nocc <-mask(dswe_vg, cc_v5, maskvalue = -999)
plot(dswe_vg_nocc)

# fsca
dfsca_vg_nocc <-mask(dfsca_vg, cc_v5, maskvalue = -999)
plot(dfsca_vg_nocc)

# test <-dfsca_vg_nocc
# values(test)[values(test) < 0] <- NA
# plot(test)
# writeRaster(test, "/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/fsca_increase.tif")

# convert rasters to dataframe
swe_df <-as.data.frame(dswe_vg_nocc, xy = TRUE, cells = TRUE, na.rm = TRUE)
fsca_df <-as.data.frame(dfsca_vg_nocc, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(swe_df)
head(fsca_df)

# bind the data frame from plotting
df <-cbind(swe_df, fsca_df[,4])
head(df)
colnames(df)[4] <- "d_swe_cm" # rename col 4
colnames(df)[5] <- "d_fsca_percent" # rename col 5
head(df)

# quick hist
hist(df$d_swe_cm, breaks = 100)
hist(df$d_fsca_percent, breaks = 100)

## scatter
theme_set(theme_classic(11))
ggplot(df, aes(x = d_fsca_percent, y = d_swe_cm)) +
  #xlim(c(-100,50)) + ylim(c(-3,.5))+
  # cale_fill_gradient(low = "grey90", high = "darkred")+
  #geom_hex(bins = 50)+
  #geom_density_2d_filled(aes(fill = ..level..), contour_var = "count") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour_var = "count")+
  scale_fill_continuous(type = "viridis") +
  labs(title = Delta~"fSCA (2/18-3/5) vs InSAR SWE (2/12-2/26)",
       x = Delta~"fSCA [%]",
       y = Delta~"SWE [cm]")

# save image, doesnt like back slahes in the name bc it's a file path... idk
ggsave("/Users/jacktarricone/ch1_jemez_data/plots/dfsca_vs_dswe_density.png",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 300)
