install.packages('lidR')
library(lidR)

# read in las file
pingers_las <-readLAS("/Users/jacktarricone/ch1_jemez/climate_station_data/adrian_tls/Site_2_Pingers.las")

# project to original utm coords
st_crs(pingers_las) <- 26913
pingers_las
plot(pingers_las)

# rasterize
utm_rast <-rasterize_canopy(pingers_las,
                            res = .1,
                            pkg = 'terra')
# test plot
plot(utm_rast)

# reproj to lat lon
pingers_latlon <-project(utm_rast, 'EPSG:4326')
pingers_latlon

# test
plot(pingers_latlon)

writeRaster(pingers_latlon, "/Users/jacktarricone/ch1_jemez/climate_station_data/adrian_tls/pingers_rast_v3.tif")
