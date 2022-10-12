# in situ snow depth vs. insar swe
# august 1st, 2022
# jack tarricone

library(terra)
library(ggplot2)
library(dplyr)
library(sf)

setwd("/Users/jacktarricone/ch1_jemez_data/")

#######
## bring in swe change rasters
rast_list <-list.files("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/new_swe_change", 
                       pattern = ".tif",
                       full.names = TRUE)

# read in list at raster stack
stack <-rast(rast_list)
sources(stack) # check paths

# bring in swe change data
sensor_csv <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/insitu_depth_change.csv")
sensor_locations <-vect(sensor_csv, geom = c("x","y"), crs = crs(stack))
plot(sensor_locations)

# bring snow depth sensor locations shapefile for cropping
loc_raw <-vect("/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/Pingers_tower_corr/Pingers_tower_corr.shp")
locations <-project(loc_raw, crs(stack))

# crop swe change stack, just for visualization purposes
ext(locations) # get extent
shp_ext <-ext(-106.5342, -106.5335, 35.8837, 35.8842) # make a bit bigger for plotting
stack_crop <-crop(stack, shp_ext)
plot(stack_crop[[4]])
points(sensor_locations, cex = 1)

# rasters from orginal stack
feb12_19 <-stack[[1]]
feb12_26_cm <-stack[[2]]
feb12_26 <-stack[[3]]
feb19_26 <-stack[[4]]

###########################
### compare insitu depth change to insar swe change
###########################

## feb 12-19
feb12_19_dswe <-terra::extract(feb12_19, sensor_locations,  cells = TRUE, xy = TRUE)
colnames(feb12_19_dswe)[2] <-"insar_feb12_19_dswe"
feb12_19_dswe

## feb 19-26
feb19_26_dswe <-terra::extract(feb19_26, sensor_locations,  cells = TRUE, xy = TRUE)
colnames(feb19_26_dswe)[2] <-"insar_feb19_26_dswe"
feb19_26_dswe

## feb 12-26
feb12_26_dswe <-terra::extract(feb12_26, sensor_locations,  cells = TRUE, xy = TRUE)
colnames(feb12_26_dswe)[2] <-"insar_feb12_26_dswe"
feb12_26_dswe

## feb 12-26 cumulative
feb12_26_cm_dswe <-terra::extract(feb12_26_cm, sensor_locations,  cells = TRUE, xy = TRUE)
colnames(feb12_26_cm_dswe)[2] <-"insar_feb12_26_cm_dswe"
feb12_26_cm_dswe

# create new df
sensor_csv_v2 <-cbind(sensor_csv, feb12_19_dswe$insar_feb12_19_dswe, feb19_26_dswe$insar_feb19_26_dswe,
                      feb12_26_dswe$insar_feb12_26_dswe, feb12_26_cm_dswe$insar_feb12_26_cm_dswe)

# rename binded colums
names(sensor_csv_v2)[7:10] <-c("insar_feb12_19_dswe","insar_feb19_26_dswe","insar_feb12_26_dswe","insar_feb12_26_cm_dswe")
sensor_csv_v2

###################
#### convert depth to SWE for noah's depth sensors
##################

swe_df <-sensor_csv_v2[1:7,] # chop of hidden valley, wacky data
swe_df

# new snow density, taking from interval board measurements 
new_snow_density <- .24

# for the second pair 
swe_df$feb12_26_dswe <-swe_df$feb12_26*.24

# read in pit data
pit_info <-read.csv("/Users/jacktarricone/ch1_jemez_data/pit_data/perm_pits.csv")
pit_info

# calc bulk density, doesn't vary much
# this value will be used for the first pair because there was no new snow
bulk_density <-mean(pit_info$mean_density[6:7])/1000


# calc SWE change
swe_df$feb12_19_dswe <-swe_df$feb12_19*bulk_density
swe_df$feb19_26_dswe <-swe_df$feb19_26*bulk_density
swe_df$feb12_26_dswe <-swe_df$feb12_26*bulk_density
swe_df

#write.csv(sensor_csv_v2, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/insitu_insar_swe_change.csv")

#### format df for plotting and anlysis

# repeat dates so they can be grouped by
first <-rep(names(swe_df[4]), length = nrow(swe_df)) 
second <-rep(names(swe_df[5]), length = nrow(swe_df)) 
third <-rep(names(swe_df[6]), length = nrow(swe_df))
fourth <-rep("feb12_26_cm", length = nrow(swe_df))
date <-c(first, second, third, fourth) # make vector

# repeat meta data
meta <-rbind(swe_df[1:3],swe_df[1:3],swe_df[1:3],swe_df[1:3])

# bind date vector
add_date <-cbind(meta, date)
add_date

# make swe data column in proper order
insar_dswe <-c(swe_df$insar_feb12_19_dswe,
               swe_df$insar_feb19_26_dswe,
               swe_df$insar_feb12_26_dswe,
               swe_df$insar_feb12_26_cm_dswe)

# make insitu column
insitu_dswe <-c(swe_df$feb12_19_dswe,
                swe_df$feb19_26_dswe,
                swe_df$feb12_26_dswe,
                swe_df$feb12_26_dswe)

# bind together
plotting_df <-cbind(add_date,insar_dswe,insitu_dswe)
plotting_df
# write.csv(plotting_df, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/insitu_insar_swe_change_plotting_df.csv")


## new plot
my_colors <-c('darkgreen', 'plum', 'goldenrod',  'firebrick')

# create new df for lm and plotting on graph
lm_df <-plotting_df[-c(1:4)]
names(lm_df)[1:2] <-c("x","y")

#shapiro.test(plotting_df$insar_dswe)
#shapiro.test(plotting_df$insitu_dswe)

# function for running lm, plotting equation and r2 
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

# plot
p <-ggplot(plotting_df, aes(insitu_dswe, insar_dswe)) +
  geom_smooth(method = "lm", se = FALSE)+
  geom_point(aes(color = date)) +
  geom_vline(xintercept = 0, linetype=1, col = "black", alpha = 1) +
  geom_hline(yintercept = 0, linetype=1, col = "black", alpha = 1) +
  #geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_y_continuous(limits = c(-6,6),breaks = c(seq(-6,6,2))) +
  scale_x_continuous(limits = c(-6,6),breaks = c(seq(-6,6,2))) +
  ylab(Delta~"SWE InSAR [cm]") + xlab(Delta~"SWE In situ [cm]") +
  scale_color_manual(name = "InSAR Pair",
                     values = my_colors,
                     breaks = c('feb12_19', 'feb19_26',
                                'feb12_26', 'feb12_26_cm'),
                     labels = c('Feb 12-19', 'Feb 19-26',
                                'Feb 12-26', 'Feb 12-26 CM'))+
  theme(legend.position = c(.83,.85))
  
# add text
p1 <- p + geom_text(x = -3.28, y = 5, label = lm_eqn(lm_df), parse = TRUE)
print(p1)

# save image
ggsave("/Users/jacktarricone/ch1_jemez_data/plots/fig12.pdf",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 400)



