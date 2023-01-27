# fsca stats
# jack tarricone
# january 26th, 2022

setwd("/Users/jacktarricone/ch1_jemez/")

# read in fsca rasters
# full
feb_fsca <-rast("./gpr_rasters_ryan/landsat_fsca_2-18v2.tif")
feb_mask <-rast("./landsat_fsca/0218_mask.tif")
mar_fsca <-rast("./gpr_rasters_ryan/landsat_fsca_3-05v2.tif")
mar_mask <-rast("./landsat_fsca/0305_mask.tif")

# read in vg aoi
vg_aoi <-vect("./vector_data/valle_grande_aoi.geojson")

# crop to vg
vg_feb_fsca <-crop(feb_fsca, vg_aoi)
vg_feb_mask <-crop(feb_mask, vg_aoi)
vg_mar_fsca <-crop(mar_fsca, vg_aoi)
vg_mar_mask <-crop(mar_mask, vg_aoi)

## test plot
# feb
plot(feb_fsca)
plot(feb_mask, add = TRUE, col = "red")
plot(vg_aoi, add = TRUE)

# march
plot(mar_fsca)
plot(mar_mask, add = TRUE, col = "red")

### calculate percent area with no snow for full scene
# feb
feb_total_swath <-expanse(feb_fsca, unit = "km")
feb_no_snow <-expanse(feb_mask, unit = "km")
feb_perecent_no_snow <-(feb_no_snow/feb_total_swath)*100

# mar
mar_total_swath <-expanse(mar_fsca, unit = "km")
mar_no_snow <-expanse(mar_mask, unit = "km")
mar_perecent_no_snow <-(mar_no_snow/mar_total_swath)*100

## for vg aoi only
# feb
vg_feb_total_swath <-expanse(vg_feb_fsca, unit = "km")
vg_feb_no_snow <-expanse(vg_feb_mask, unit = "km")
vg_feb_perecent_no_snow <-(vg_feb_no_snow/vg_feb_total_swath)*100

# mar
vg_mar_total_swath <-expanse(vg_mar_fsca, unit = "km")
vg_mar_no_snow <-expanse(vg_mar_mask, unit = "km")
vg_mar_perecent_no_snow <-(vg_mar_no_snow/vg_mar_total_swath)*100



