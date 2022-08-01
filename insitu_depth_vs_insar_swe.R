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
points(sensor_locations, cex = 1)

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

#### testing this
#### convert depth to SWE
sensor_csv_v2$feb12_19_dswe <-sensor_csv_v2$feb12_19*.2
sensor_csv_v2$feb19_26_dswe <-sensor_csv_v2$feb19_26*.2
sensor_csv_v2$feb12_26_dswe <-sensor_csv_v2$feb12_26*.2
sensor_csv_v2


## plot
ggplot(sensor_csv_v2) +
  geom_vline(xintercept = 0, linetype=1, col = "black", alpha = 1) +
  geom_hline(yintercept = 0, linetype=1, col = "black", alpha = 1) +
  geom_point(aes(x = feb12_19_dswe, y = insar_feb12_19_dswe, col = "pair1")) +
  geom_point(aes(x = feb19_26_dswe, y = insar_feb19_26_dswe, col = "pair2")) +
  geom_point(aes(x = feb12_26_dswe, y = insar_feb12_26_dswe, col = "pair3")) +
  geom_point(aes(x = feb12_26_dswe, y = insar_feb12_26_cm_dswe, col = "pair4")) +
  scale_y_continuous(limits = c(-4,4),breaks = c(seq(-4,4,2))) +
  scale_x_continuous(limits = c(-4,4),breaks = c(seq(-4,4,2))) +
  ylab(Delta~"SWE InSAR [cm]") + xlab(Delta~"SWE In situ [cm]") +
  scale_color_manual(name = "Date",
                     values = c('pair1' = 'darkgreen', 'pair2' = 'plum', 
                                'pair3' = 'goldenrod', 'pair4' = 'firebrick'),
                     labels = c('pair1' = 'Feb 12-19', 'pair2' = 'Feb 19-26', 
                                'pair3' = 'Feb 12-26', 'pair4' = 'Feb 12-26 CM'))



# mean of 9 swe changes around
mean_pit_dswe <-mean(nine_cell_dswe[1:9,1])
mean_pit_dswe














# bring in 2/12-2/26 gpr data
gpr_feb26_minus_feb12_v1 <-rast("./gpr_swe_bias/feb26_minus_Feb12_bias_corrected1.tif")
gpr_feb26_minus_feb12 <-gpr_feb26_minus_feb12_v1/10 # convert to cm from mm
plot(gpr_feb26_minus_feb12)
hist(gpr_feb26_minus_feb12, breaks = 50)

global(gpr_feb26_minus_feb12,mean,na.rm=T)


# resample gpr to same grid as unw, crop ext
i_swe_cum_crop <-crop(i_swe_cum, ext(gpr_feb26_minus_feb12)) # crop
gpr_feb26_minus_feb12 # check
i_swe_cum_crop # check

# test plot
plot(i_swe_cum_crop)
plot(gpr_feb26_minus_feb12, add = TRUE, col = "red")

# mask unw data with gpr
i_swe_cum_crop_mask <-mask(i_swe_cum_crop, gpr_feb26_minus_feb12, maskvalue = NA)
f26_m_12_mask <-mask(gpr_feb26_minus_feb12, i_swe_cum_crop_mask, maskvalue = NA)

# plot only pixels that have data for both gpr and unw
plot(i_swe_cum_crop_mask)
plot(f26_m_12_mask, add = TRUE, col = hcl.colors(12, "Berlin"))

# convert raster to dataframe
swe_df <-as.data.frame(i_swe_cum_crop_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
gpr_df <-as.data.frame(f26_m_12_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(gpr_df)
head(swe_df)

# bind the data frames
cm_plotting_df <-cbind(swe_df, gpr_df$feb26_minus_Feb12_bias_corrected1)
head(cm_plotting_df)
colnames(cm_plotting_df)[4] <- "dswe_insar" # rename col 4
colnames(cm_plotting_df)[5] <- "dswe_gpr" # rename col 5
head(cm_plotting_df)

# quick hists
hist(cm_plotting_df$dswe_gpr, breaks = 10)
hist(cm_plotting_df$dswe_insar, breaks = 10)

####
# plotting
####

# scattter
#theme_set(theme_light(11)) # set theme
theme_set(theme_bw(14)) # set theme
ggplot(cm_plotting_df) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ylim(c(-10,10)) + xlim(c(-10,10))+
  geom_point(aes(y = dswe_insar, x = dswe_gpr), color = "darkred", alpha = .3) +
  labs(#title = Delta~"SWE GPR vs. InSAR 2/12 - 2/26",
       x = Delta~"SWE GPR [cm]",
       y = Delta~"SWE InSAR [cm]")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

# lm_df <-cm_plotting_df[-c(1:3)]
# names(lm_df)[1:2] <-c("x","y")
# 
# lm_eqn <- function(df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(unname(coef(m)[1]), digits = 2),
#                         b = format(unname(coef(m)[2]), digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }
# 
# p1 <- p + geom_text(x = -5, y = 9, label = lm_eqn(lm_df), parse = TRUE)
# print(p1)

ggsave("/Users/jacktarricone/ch1_jemez_data/plots/dswe_gpr_vs_insar_feb26_12_bias_v5.png",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 300)

# density scatter
ggplot(cm_plotting_df, aes(y = dswe_insar, x = dswe_gpr)) +
  #geom_abline(slope = 1) +
  xlim(c(-2,2)) + ylim(c(-2,2))+
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour_var = "count")+
  scale_fill_continuous(type = "viridis") +
  labs(#title = Delta~"SWE GPR vs. InSAR 2/12 - 2/26",
       x = Delta~"SWE GPR [cm]",
       y = Delta~"SWE InSAR [cm]")

# save image, doesnt like back slahes in the name bc it's a file path... idk
ggsave("/Users/jacktarricone/ch1_jemez_data/plots/swe_gpr_vs_insar_feb12_26.png",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 300)



