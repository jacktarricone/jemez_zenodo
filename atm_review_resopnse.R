library(terra)
p1 <-rast("./ch1_jemez/gpr_rasters_ryan/unw_raw_feb12-19.tif")
hist(p1, breaks = 100)
p1_df <-as.data.frame(p1)
p1_iqr <-IQR(p1_df$`alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.unw.grd`)

p1_corr <-rast("./ch1_jemez/gpr_rasters_ryan/unw_corrected_feb12-19.tif")
hist(p1_corr, breaks = 100)
p1_corr_df <-as.data.frame(p1_corr)
p1_corr_iqr <-IQR(p1_corr_df$`alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.unw.grd`)


p2 <-rast("./ch1_jemez/gpr_rasters_ryan/unw_feb19-feb26.tif")
hist(p2, breaks = 100)
p2_df <-as.data.frame(p2)
p2_iqr <-IQR(p2_df$`alamos_35915_20008-000_20013-000_0007d_s01_L090HH_02.unw`)
