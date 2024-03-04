# convert linear amp to db for amplitude data
# jack tarricone

library(terra)

# set path to '/jemez_lband_swe_code_data' that was downloaded and unzipped from zenodo
# all other file paths are relative
setwd("path/to/jemez_lband_swe_code_data")
list.files() #pwd

## bring in lin amps
# 12-19 feb
amp1 <-rast("./rasters/amplitude/linear/alamos_35915_20005-003_20008-000_0007d_s01_L090HH_01.amp1.grd.tiff")
amp2 <-rast("./rasters/amplitude/linear/alamos_35915_20008-000_20013-000_0007d_s01_L090HH_01.amp1.grd.tiff")
amp3 <-rast("./rasters/amplitude/linear/alamos_35915_20005-003_20013-000_0014d_s01_L090HH_01.amp2.grd.tiff")

# convert to db and save for the three dates
# 12 feb
amp1_db <-10*log10(amp1)
plot(amp1_db)
hist(amp1_db, breaks = 100)
writeRaster(amp1_db, "./rasters/amplitude/db/feb12_amp_db.tif")

# 19 feb
amp2_db <-10*log10(amp2)
plot(amp2_db)
hist(amp2_db, breaks = 200)
writeRaster(amp2_db, "./rasters/amplitude/db/feb19_amp_db.tif")

# 26 feb
amp3_db <-10*log10(amp3)
plot(amp3_db)
hist(amp3_db, breaks = 200)
writeRaster(amp3_db, "./rasters/amplitude/db/feb26_amp_db.tif")
