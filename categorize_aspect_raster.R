library(terra)

aspect <-rast("/Users/jacktarricone/ch1_jemez/jemez_lidar/vg_aspect.tif")
plot(aspect)
aspect

dswe <-rast("/Users/jacktarricone/ch1_jemez/gpr_rasters_ryan/new_swe_change/dswe_feb12-26_cumulative.tif")

test <-project(aspect, crs(dswe))
plot(test)

test2 <-resample(test, dswe, method = 'bilinear')


aspect_classes <-matrix(c(315,360,1, 00,45,1, # 1 = north
                          135,225,2,          # 2 = south
                          45,135,3,           # 3 = east 
                          225,315,4),         # 4 = west
                          ncol=3, byrow=TRUE)

aspect_classes

directions <-classify(test2, rcl = aspect_classes)
plot(directions)

writeRaster(directions, "/Users/jacktarricone/ch1_jemez/jemez_lidar/nsew_aspect_v2.tif")

aspect_classes2 <-matrix(c(270,360,1, 0,90,1, # 1 = north
                           90,270,2),          # 2 = south
                           ncol=3, byrow=TRUE)

directions_ns <-classify(test2, rcl = aspect_classes2)
plot(directions_ns)
writeRaster(directions_ns, "/Users/jacktarricone/ch1_jemez/jemez_lidar/ns_aspect.tif")
