# mask swe change rasters so they all have the same pixels
# july 18th, 2022

library(terra)

setwd("/Users/jacktarricone/ch1_jemez/gpr_rasters_ryan/new_swe_change/")
list.files()

#import rasters
feb12_19 <-rast("./rough/dswe_feb12-19.tif")
feb19_26_v1 <-rast("./rough/dswe_feb19-26.tif")
feb12_26_v1 <-rast("./rough/dswe_feb12-26_new.tif")

feb12_19
feb19_26_v1
feb12_26

##### resample and mask
# resample 19-26 to the 12-19 grid, slightly off for some reason
feb19_26 <-resample(feb19_26_v1, feb12_19)
feb12_26 <-resample(feb12_26_v1, feb12_19)

# mask
pair2_mask <-mask(feb19_26, feb12_19, maskvalues = NA) # mask for only same pixels
pair3_mask <-mask(feb12_26, feb12_19, maskvalues = NA) # mask for only same pixels

# test plot of both pairs
plot(pair2_mask)
plot(pair3_mask)

# mask pair 1 with pair 2
pair1_mask <-mask(feb12_19, pair2_mask)
plot(pair1_mask)
plot(pair2_mask, add = TRUE)

# check to see if same number of NaNs
global(pair1_mask, fun="isNA")
global(pair2_mask, fun="isNA")

######
## mask pair 3 (feb12-26)
#####

# remask to sites will have the exact same pixels
pair3_mask <-mask(feb12_26, pair2_mask, maskvalues = NA)
plot(pair2_mask) # test
plot(pair3_mask, add = TRUE)
global(pair3_mask, fun="isNA")

# now again with pair 3
pair1_mask_v2 <-mask(pair1_mask, pair3_mask)
global(pair1_mask_v2, fun="isNA")

pair2_mask_v2 <-mask(pair2_mask, pair3_mask)
global(pair2_mask_v2, fun="isNA")

# cmulative phase
dswe_cm <-pair1_mask_v2 + pair2_mask_v2
global(dswe_cm, fun="isNA")
plot(dswe_cm)

### save all four rasters
writeRaster(dswe_cm, "dswe_feb12-26_cumulative.tif")
writeRaster(pair1_mask, "dswe_feb12-19_sp.tif")
writeRaster(pair2_mask, "dswe_feb19-26_sp.tif")
writeRaster(pair3_mask, "dswe_feb12-26_sp.tif")

