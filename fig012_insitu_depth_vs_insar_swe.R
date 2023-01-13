# in situ snow depth vs. insar swe
# august 1st, 2022
# jack tarricone

library(terra)
library(ggplot2);theme_set(theme_classic(12))
library(dplyr)
library(sf)

# set custom theme
theme_classic <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # show axes
      # axis.line      = element_line(colour = "black", linewidth = rel(1)),
      
      # match legend key to panel.background
      legend.key       = element_blank(),
      
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      
      complete = TRUE
    )
}

setwd("/Users/jacktarricone/ch1_jemez/")

#######
## bring in swe change rasters
rast_list <-list.files("./gpr_rasters_ryan/new_swe_change", 
                       pattern = ".tif",
                       full.names = TRUE)

# read in list at raster stack
stack <-rast(rast_list)
sources(stack) # check paths
stack

# bring in swe change data
depth_change_csv <-read.csv("./climate_station_data/noah/insitu_depth_change.csv")
sensor_locations <-vect(depth_change_csv, geom = c("x","y"), crs = crs(stack))
plot(sensor_locations)
sensor_locations

# bring snow depth sensor locations shapefile for cropping
loc_raw <-vect("./climate_station_data/noah/Pingers_tower_corr/Pingers_tower_corr.shp")
locations <-project(loc_raw, crs(stack))

# crop swe change stack, just for visualization purposes
ext(locations) # get extent
shp_ext <-ext(-106.5342, -106.5335, 35.8837, 35.8842) # make a bit bigger for plotting
stack_crop <-crop(stack, shp_ext)
plot(stack_crop[[4]])
points(sensor_locations, cex = 1)

# rasters from orginal stack
feb12_19 <-stack[[1]]
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

# create new df
depth_change_csv_v2 <-cbind(depth_change_csv, feb12_19_dswe$insar_feb12_19_dswe, feb19_26_dswe$insar_feb19_26_dswe,
                      feb12_26_dswe$insar_feb12_26_dswe)

# rename binded colums
names(depth_change_csv_v2)[7:9] <-c("insar_feb12_19_dswe","insar_feb19_26_dswe","insar_feb12_26_dswe")
depth_change_csv_v2

###################
#### convert depth to SWE for noah's depth sensors
##################

swe_df <-depth_change_csv_v2[1:7,] # chop of hidden valley, wacky data
swe_df

# new snow density, taking from interval board measurements 
new_snow_density <- .24

# for the second pair 
swe_df$feb19_26_dswe <-swe_df$feb19_26*.24
swe_df

# read in pit data
pit_info <-read.csv("./pit_data/perm_pits.csv")
pit_info

# calc bulk density, doesn't vary much
# this value will be used for the first pair because there was no new snow
bulk_density <-mean(pit_info$mean_density[6:7])/1000

# calc SWE change
swe_df$feb12_19_dswe <-swe_df$feb12_19*bulk_density
swe_df$feb12_26_dswe <-swe_df$feb12_26*bulk_density
swe_df

### add error term two swe calc
# 10% uncertainty density
# 2 cm uncertainty depth

swe_df$feb12_19_dswe_error <-abs(swe_df$feb12_19_dswe)-abs((-2+swe_df$feb12_19)*(bulk_density+(bulk_density*.1)))
swe_df$feb19_26_dswe_error <-abs(swe_df$feb19_26_dswe)-abs((2+swe_df$feb19_26)*(.24+(.24*.1)))
swe_df$feb12_26_dswe_error <-abs(swe_df$feb12_26_dswe)-abs((-2+swe_df$feb12_26)*(bulk_density+(bulk_density*.1)))
swe_df

#write.csv(sensor_csv_v2, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/insitu_insar_swe_change.csv")

#### format df for plotting and anlysis
# repeat dates so they can be grouped by
first <-rep(names(swe_df[4]), length = nrow(swe_df)) 
second <-rep(names(swe_df[5]), length = nrow(swe_df)) 
third <-rep(names(swe_df[6]), length = nrow(swe_df))
date <-c(first, second, third) # make vector

# repeat meta data
meta <-rbind(swe_df[1:3],swe_df[1:3],swe_df[1:3])

# bind date vector
add_date <-cbind(meta, date)
add_date

# make swe data column in proper order
insar_dswe <-c(swe_df$insar_feb12_19_dswe,
               swe_df$insar_feb19_26_dswe,
               swe_df$insar_feb12_26_dswe)

# make insitu column
insitu_dswe <-c(swe_df$feb12_19_dswe,
                swe_df$feb19_26_dswe,
                swe_df$feb12_26_dswe)

# error column
insitu_error<-c(swe_df$feb12_19_dswe_error,
                swe_df$feb19_26_dswe_error,
                swe_df$feb12_26_dswe_error)

# bind together
plotting_df <-cbind(add_date,insar_dswe,insitu_dswe,insitu_error)
plotting_df
# write.csv(plotting_df, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/insitu_insar_swe_change_plotting_df.csv")

### add error term two swe calc
# 10% uncertainty density
# 2 cm uncertainty depth


ggplot()+
  #geom_point(aes(x=den_P_depth_P, y=depth_change_csv_v2$sensor), color = 'darkred', shape = 4) +
  #geom_point(aes(x=den_M_depth_M, y=depth_change_csv_v2$sensor), color = 'darkblue', shape = 4) +
  #geom_point(aes(x=den_M_depth_M, y=depth_change_csv_v2$sensor), color = 'darkgreen', shape = 4) +
  #geom_point(aes(x=den_P_depth_M, y=depth_change_csv_v2$sensor), color = 'violet', shape = 4) +
  geom_point(aes(x=normal, y=depth_change_csv_v2$sensor), color = 'black') +
  geom_errorbar(aes(y=depth_change_csv_v2$sensor, xmin=normal+1, xmax=normal-1), 
                  width=0.1, colour="orange", alpha=0.9, size=.5)
  
## new plot
my_colors <-c('darkgreen', 'plum', 'goldenrod')

# create new df for lm and plotting on graph
lm_df <-plotting_df[-c(1:4)]
names(lm_df)[1:2] <-c("x","y")

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
p <-ggplot(plotting_df, aes(x = insitu_dswe, y = insar_dswe)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = FALSE)+
  geom_errorbar(aes(y= insar_dswe, xmin=insitu_dswe-abs(insitu_error), xmax=insitu_dswe+abs(insitu_error)), 
                 width=0.1, colour = 'black', alpha=0.4, size=.5) +
  geom_point(aes(color = date)) + 
  scale_y_continuous(limits = c(-6,6),breaks = c(seq(-6,6,2))) +
  scale_x_continuous(limits = c(-6,6),breaks = c(seq(-6,6,2))) +
  ylab(Delta~"SWE InSAR (cm)") + xlab(Delta~"SWE In Situ (cm)") +
  scale_color_manual(name = "InSAR Pair",
                     values = my_colors,
                     breaks = c('feb12_19', 'feb19_26', 'feb12_26'),
                     labels = c('Feb. 12-19', 'Feb. 19-26', 'Feb. 12-26'))+
  scale_fill_discrete(breaks=c('B', 'C', 'A'))  +
  theme_classic(12) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  theme(legend.position = c(.83,.2))

p1 <- p + geom_text(x = -2.5, y = 4, label = lm_eqn(lm_df), parse = TRUE)
print(p1)


# save image, doesnt like back slahes in the name bc it's a file path... idk
ggsave("./plots/in_situ_insar_fig12.pdf",
        width = 5, 
        height = 5,
        units = "in",
        dpi = 400)



