library(terra)

setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/new_swe_change/")
list.files()

#import rasters
feb12_19 <-rast("dswe_feb12-19.tif")
feb19_26_v1 <-rast("dswe_feb19-26.tif")
feb12_26 <-rast("dswe_feb12-26.tif")

##### resample and mask
# resample 19-26 to the 12-19 grid, slightly off for some reason
feb19_26 <-resample(feb19_26_v1, feb12_26)
pair2_mask <-mask(feb12_19, feb19_26, maskvalues = NA) # mask for only same pixels

# test plot of both pairs
plot(pair2_mask)

# mask pair 1 with pair 2
pair1_mask <-mask(feb12_26, pair2_mask)
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

# cmulative phase
dswe_cm <-pair1_mask + pair2_mask
global(dswe_cm, fun="isNA")
writeRaster(dswe_cm, "dswe_feb12-26_cumulative.tif")


