# make swe change stats table for manuscript
# add vg, north, and south stats
# jack tarricone
# jan 16th, 2022

library(terra)
library(cowplot)
library(ggplot2)
library(grid)
library(gridExtra)
library(kableExtra)

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

theme_set(theme_classic(14))

# set working dir
setwd("/Users/jacktarricone/ch1_jemez/gpr_rasters_ryan/new_swe_change")
list.files()

# big in vg_aoi, vg_mask
vg_aoi <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")
vg <-vect("/Users/jacktarricone/ch1_jemez/vector_data/vg_shp.geojson")

# bring in north south raster and crop
lidar_ns_v1 <-rast("/Users/jacktarricone/ch1_jemez/jemez_lidar/ns_aspect.tif")
lidar_ns <-crop(lidar_ns_v1, vg_aoi)

# test plot
plot(lidar_ns)
plot(vg_aoi, add = TRUE)
plot(vg, add = TRUE)

# mask ns with for area outside vg
ns_no_vg <-mask(lidar_ns, vg, inverse = TRUE)
plot(ns_no_vg)

####### load in rasters
# pair 1
feb12_19_r <-rast("dswe_feb12-19_sp.tif")
p1 <-crop(feb12_19_r, vg_aoi)
plot(p1)
plot(no_snow_mask, col = 'red', add = TRUE)

# pair 2
feb19_26_r <-rast("dswe_feb19-26_sp.tif")
p2 <-crop(feb19_26_r, vg_aoi)
plot(p2)

# pair 3
feb12_26_r <-rast("dswe_feb12-26_sp.tif")
p3 <-crop(feb12_26_r, vg_aoi)
plot(p3)

# pair 4
feb12_26_cm_r <-rast("dswe_feb12-26_cumulative.tif")
p4 <-crop(feb12_26_cm_r, vg_aoi)

## crop for vg
p1_vg <-mask(p1, vg)
p2_vg <-mask(p2, vg)
p3_vg <-mask(p3, vg)
p4_vg <-mask(p4, vg)

# crop for north facing slopes
p1_north_v1 <-mask(p1, ns_no_vg, maskvalue = 2) # 2 = south, therefore masking for 2 will leave north
p1_north <-mask(p1_north_v1, vg, inverse = TRUE)
plot(p1_north)

p2_north_v1 <-mask(p2, ns_no_vg, maskvalue = 2) 
p2_north <-mask(p2_north_v1, vg, inverse = TRUE)
plot(p2_north)

p3_north_v1 <-mask(p3, ns_no_vg, maskvalue = 2)
p3_north <-mask(p3_north_v1, vg, inverse = TRUE)

p4_north_v1 <-mask(p4, ns_no_vg, maskvalue = 2) 
p4_north <-mask(p4_north_v1, vg, inverse = TRUE)

# crop for south facing slopes
p1_south_v1 <-mask(p1, ns_no_vg, maskvalue = 1) # 1 = north, therefore masking for 1 will leave south
p1_south <-mask(p1_south_v1, vg, inverse = TRUE)

p2_south_v1 <-mask(p2, ns_no_vg, maskvalue = 1) 
p2_south <-mask(p2_south_v1, vg, inverse = TRUE)

p3_south_v1 <-mask(p3, ns_no_vg, maskvalue = 1)
p3_south <-mask(p3_south_v1, vg, inverse = TRUE)

p4_south_v1 <-mask(p4, ns_no_vg, maskvalue = 1) #
p4_south <-mask(p4_south_v1, vg, inverse = TRUE)

### make dataframes
##### full scence
p1_fs_df <-as.data.frame(p1, na.rm = TRUE)
names(p1_fs_df)[1] <-"dswe"

p2_fs_df <-as.data.frame(p2, na.rm = TRUE)
names(p2_fs_df)[1] <-"dswe"

p3_fs_df <-as.data.frame(p3, na.rm = TRUE)
names(p3_fs_df)[1] <-"dswe"

p4_fs_df <-as.data.frame(p4, na.rm = TRUE)
names(p4_fs_df)[1] <-"dswe"

######## vg
p1_vg_df <-as.data.frame(p1_vg, na.rm = TRUE)
names(p1_vg_df)[1] <-"dswe"

p2_vg_df <-as.data.frame(p2_vg, na.rm = TRUE)
names(p2_vg_df)[1] <-"dswe"

p3_vg_df <-as.data.frame(p3_vg, na.rm = TRUE)
names(p3_vg_df)[1] <-"dswe"

p4_vg_df <-as.data.frame(p4_vg, na.rm = TRUE)
names(p4_vg_df)[1] <-"dswe"

######## north facing
p1_north_df <-as.data.frame(p1_north, na.rm = TRUE)
names(p1_north_df)[1] <-"dswe"

p2_north_df <-as.data.frame(p2_north, na.rm = TRUE)
names(p2_north_df)[1] <-"dswe"

p3_north_df <-as.data.frame(p3_north, na.rm = TRUE)
names(p3_north_df)[1] <-"dswe"

p4_north_df <-as.data.frame(p4_north, na.rm = TRUE)
names(p4_north_df)[1] <-"dswe"

######## south facing
p1_south_df <-as.data.frame(p1_south, na.rm = TRUE)
names(p1_south_df)[1] <-"dswe"

p2_south_df <-as.data.frame(p2_south, na.rm = TRUE)
names(p2_south_df)[1] <-"dswe"

p3_south_df <-as.data.frame(p3_south, na.rm = TRUE)
names(p3_south_df)[1] <-"dswe"

p4_south_df <-as.data.frame(p4_south, na.rm = TRUE)
names(p4_south_df)[1] <-"dswe"

#####################
## calculate stats ##
#####################

##################
### Full Study Area ###
##################

##########
# pair 1 #
##########

p1_mean_fs <-round(as.numeric(global(p1, mean, na.rm = TRUE)), digits = 2)
p1_med_fs <-round(as.numeric(global(p1, median, na.rm = TRUE)), digits = 2)
p1_sd_fs <-round(as.numeric(global(p1, sd, na.rm = TRUE)), digits = 2)
p1_iqr_fs <-round(as.numeric(global(p1, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 2 #
##########

p2_mean_fs <-round(as.numeric(global(p2, mean, na.rm = TRUE)), digits = 2)
p2_med_fs <-round(as.numeric(global(p2, median, na.rm = TRUE)), digits = 2)
p2_sd_fs <-round(as.numeric(global(p2, sd, na.rm = TRUE)), digits = 2)
p2_iqr_fs <-round(as.numeric(global(p2, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 3 #
##########

p3_mean_fs <-round(as.numeric(global(p3, mean, na.rm = TRUE)), digits = 2)
p3_med_fs <-round(as.numeric(global(p3, median, na.rm = TRUE)), digits = 2)
p3_sd_fs <-round(as.numeric(global(p3, sd, na.rm = TRUE)), digits = 2)
p3_iqr_fs <-round(as.numeric(global(p3, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 4 #
##########

p4_mean_fs <-round(as.numeric(global(p4, mean, na.rm = TRUE)), digits = 2)
p4_med_fs <-round(as.numeric(global(p4, median, na.rm = TRUE)), digits = 2)
p4_sd_fs <-round(as.numeric(global(p4, sd, na.rm = TRUE)), digits = 2)
p4_iqr_fs <-round(as.numeric(global(p4, IQR, na.rm = TRUE)), digits = 2)

## create dataframe
fs_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
          "mean" = c(p1_mean_fs, p2_mean_fs, p3_mean_fs, p4_mean_fs),
          "sd" = c(p1_sd_fs, p2_sd_fs, p3_sd_fs, p4_sd_fs),
          "med" = c(p1_med_fs, p2_med_fs, p3_med_fs, p4_med_fs),
          "iqr" = c(p1_iqr_fs, p2_iqr_fs, p3_iqr_fs, p4_iqr_fs))

fs_stats # check

write.csv(fs_stats, "/Users/jacktarricone/ch1_jemez/pit_data/full_scene_swe_stats.csv")

##############
###   vg   ###
##############

##########
# pair 1 #
##########

p1_mean_vg <-round(as.numeric(global(p1_vg, mean, na.rm = TRUE)), digits = 2)
p1_med_vg <-round(as.numeric(global(p1_vg, median, na.rm = TRUE)), digits = 2)
p1_sd_vg <-round(as.numeric(global(p1_vg, sd, na.rm = TRUE)), digits = 2)
p1_iqr_vg <-round(as.numeric(global(p1_vg, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 2 #
##########

p2_mean_vg <-round(as.numeric(global(p2_vg, mean, na.rm = TRUE)), digits = 2)
p2_med_vg <-round(as.numeric(global(p2_vg, median, na.rm = TRUE)), digits = 2)
p2_sd_vg <-round(as.numeric(global(p2_vg, sd, na.rm = TRUE)), digits = 2)
p2_iqr_vg <-round(as.numeric(global(p2_vg, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 3 #
##########

p3_mean_vg <-round(as.numeric(global(p3_vg, mean, na.rm = TRUE)), digits = 2)
p3_med_vg <-round(as.numeric(global(p3_vg, median, na.rm = TRUE)), digits = 2)
p3_sd_vg <-round(as.numeric(global(p3_vg, sd, na.rm = TRUE)), digits = 2)
p3_iqr_vg <-round(as.numeric(global(p3_vg, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 4 #
##########

p4_mean_vg <-round(as.numeric(global(p4_vg, mean, na.rm = TRUE)), digits = 2)
p4_med_vg <-round(as.numeric(global(p4_vg, median, na.rm = TRUE)), digits = 2)
p4_sd_vg <-round(as.numeric(global(p4_vg, sd, na.rm = TRUE)), digits = 2)
p4_iqr_vg <-round(as.numeric(global(p4_vg, IQR, na.rm = TRUE)), digits = 2)

## create dataframe
vg_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                      "mean" = c(p1_mean_vg, p2_mean_vg, p3_mean_vg, p4_mean_vg),
                      "sd" = c(p1_sd_vg, p2_sd_vg, p3_sd_vg, p4_sd_vg),
                      "med" = c(p1_med_vg, p2_med_vg, p3_med_vg, p4_med_vg),
                      "iqr" = c(p1_iqr_vg, p2_iqr_vg, p3_iqr_vg, p4_iqr_vg))

vg_stats # check
fs_stats

# write.csv(fs_stats, "/Users/jacktarricone/ch1_jemez/pit_data/vg_scene_swe_stats.csv")

##############
### north  ###
##############

##########
# pair 1 #
##########

p1_mean_north <-round(as.numeric(global(p1_north, mean, na.rm = TRUE)), digits = 2)
p1_med_north <-round(as.numeric(global(p1_north, median, na.rm = TRUE)), digits = 2)
p1_sd_north <-round(as.numeric(global(p1_north, sd, na.rm = TRUE)), digits = 2)
p1_iqr_north <-round(as.numeric(global(p1_north, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 2 #
##########

p2_mean_north <-round(as.numeric(global(p2_north, mean, na.rm = TRUE)), digits = 2)
p2_med_north <-round(as.numeric(global(p2_north, median, na.rm = TRUE)), digits = 2)
p2_sd_north <-round(as.numeric(global(p2_north, sd, na.rm = TRUE)), digits = 2)
p2_iqr_north <-round(as.numeric(global(p2_north, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 3 #
##########

p3_mean_north <-round(as.numeric(global(p3_north, mean, na.rm = TRUE)), digits = 2)
p3_med_north <-round(as.numeric(global(p3_north, median, na.rm = TRUE)), digits = 2)
p3_sd_north <-round(as.numeric(global(p3_north, sd, na.rm = TRUE)), digits = 2)
p3_iqr_north <-round(as.numeric(global(p3_north, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 4 #
##########

p4_mean_north <-round(as.numeric(global(p4_north, mean, na.rm = TRUE)), digits = 2)
p4_med_north <-round(as.numeric(global(p4_north, median, na.rm = TRUE)), digits = 2)
p4_sd_north <-round(as.numeric(global(p4_north, sd, na.rm = TRUE)), digits = 2)
p4_iqr_north <-round(as.numeric(global(p4_north, IQR, na.rm = TRUE)), digits = 2)

## create dataframe
north_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                      "mean" = c(p1_mean_north, p2_mean_north, p3_mean_north, p4_mean_north),
                      "sd" = c(p1_sd_north, p2_sd_north, p3_sd_north, p4_sd_north),
                      "med" = c(p1_med_north, p2_med_north, p3_med_north, p4_med_north),
                      "iqr" = c(p1_iqr_north, p2_iqr_north, p3_iqr_north, p4_iqr_north))

vg_stats # check
fs_stats
north_stats

# write.csv(north_stats, "/Users/jacktarricone/ch1_jemez/pit_data/north_swe_stats.csv")

##############
### south  ###
##############

##########
# pair 1 #
##########

p1_mean_south <-round(as.numeric(global(p1_south, mean, na.rm = TRUE)), digits = 2)
p1_med_south <-round(as.numeric(global(p1_south, median, na.rm = TRUE)), digits = 2)
p1_sd_south <-round(as.numeric(global(p1_south, sd, na.rm = TRUE)), digits = 2)
p1_iqr_south <-round(as.numeric(global(p1_south, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 2 #
##########

p2_mean_south <-round(as.numeric(global(p2_south, mean, na.rm = TRUE)), digits = 2)
p2_med_south <-round(as.numeric(global(p2_south, median, na.rm = TRUE)), digits = 2)
p2_sd_south <-round(as.numeric(global(p2_south, sd, na.rm = TRUE)), digits = 2)
p2_iqr_south <-round(as.numeric(global(p2_south, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 3 #
##########

p3_mean_south <-round(as.numeric(global(p3_south, mean, na.rm = TRUE)), digits = 2)
p3_med_south <-round(as.numeric(global(p3_south, median, na.rm = TRUE)), digits = 2)
p3_sd_south <-round(as.numeric(global(p3_south, sd, na.rm = TRUE)), digits = 2)
p3_iqr_south <-round(as.numeric(global(p3_south, IQR, na.rm = TRUE)), digits = 2)

##########
# pair 4 #
##########

p4_mean_south <-round(as.numeric(global(p4_south, mean, na.rm = TRUE)), digits = 2)
p4_med_south <-round(as.numeric(global(p4_south, median, na.rm = TRUE)), digits = 2)
p4_sd_south <-round(as.numeric(global(p4_south, sd, na.rm = TRUE)), digits = 2)
p4_iqr_south <-round(as.numeric(global(p4_south, IQR, na.rm = TRUE)), digits = 2)

## create dataframe
south_stats <-data.frame("name" = c("pair1", "pair2", "pair3", "pair4"),
                         "mean" = c(p1_mean_south, p2_mean_south, p3_mean_south, p4_mean_south),
                         "sd" = c(p1_sd_south, p2_sd_south, p3_sd_south, p4_sd_south),
                         "med" = c(p1_med_south, p2_med_south, p3_med_south, p4_med_south),
                         "iqr" = c(p1_iqr_south, p2_iqr_south, p3_iqr_south, p4_iqr_south))


fs_stats
vg_stats 
north_stats
south_stats


table <-as.data.frame(rbind(fs_stats, vg_stats, north_stats, south_stats))

table %>%
  kbl(caption="Summary Statistics of Financial Well-Being Score by Gender and Education",
      format="latex",
      col.names = c("Pair","Mean","SD","Median","IQR"),
      align="r") %>%
  kable_minimal(full_width = F,  html_font = "Source Sans Pro")

# write.csv(south_stats, "/Users/jacktarricone/ch1_jemez/pit_data/south_swe_stats.csv")


####
# histograms
###

# set y axis scientific theme
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}



####################
####################
## Full Study Area pair 1 & 2
####################
####################

fs_12_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p2_fs_df, mapping = aes(x=dswe, y=stat(count),fill = "19-26 Feb.", color = "19-26 Feb."), alpha=0.1) +
  geom_density(p1_fs_df, mapping = aes(x=dswe, y=stat(count),fill = "12-19 Feb.", color = "12-19 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-19 Feb.","19-26 Feb."),
                      values = c("darkorchid4","goldenrod"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-19 Feb.","19-26 Feb."),
                    values = c("darkorchid4","goldenrod"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  ylab("Count") +
  #xlab(expression(Delta~'SWE'))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = c(.21,.82)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# plot(fs_12_v1)

fs_1_lab <-c(paste0("Mean = ",p1_mean_fs,"\n","SD = ",p1_sd_fs))
fs_1_lab

fs_2_lab <-c(paste0("Mean = ",p2_mean_fs,"\n","SD = ",p2_sd_fs))
fs_2_lab

fs_12 <- fs_12_v1 + 
  annotate(geom="text", x=4.2, y=1.1e6, label= "Full Study Area", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12 - 19 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= fs_1_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "19 - 26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= fs_2_lab)

print(fs_12)











####################
####################
## vg pair 1 & 2
####################
####################

vg_12_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p2_vg_df, mapping = aes(x=dswe, y=stat(count),fill = "19-26 Feb.", color = "19-26 Feb."), alpha=0.1) +
  geom_density(p1_vg_df, mapping = aes(x=dswe, y=stat(count),fill = "12-19 Feb.", color = "12-19 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-19 Feb.","19-26 Feb."),
                      values = c("darkorchid4","goldenrod"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-19 Feb.","19-26 Feb."),
                    values = c("darkorchid4","goldenrod"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  # ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = 'none') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

# plot(vg_12_v1)

vg_1_lab <-c(paste0("Mean = ",p1_mean_vg,"\n","SD = ",p1_sd_vg))
vg_1_lab

vg_2_lab <-c(paste0("Mean = ",p2_mean_vg,"\n","SD = ",p2_sd_vg))
vg_2_lab

vg_12 <-vg_12_v1 +
  annotate(geom="text", x=4.2, y=1.1e6, label= "VG", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12 - 19 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= vg_1_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "19 - 26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= vg_2_lab)

print(vg_12)






####################
####################
## north pair 1 & 2
####################
####################

north_12_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p2_north_df, mapping = aes(x=dswe, y=stat(count),fill = "19-26 Feb.", color = "19-26 Feb."), alpha=0.1) +
  geom_density(p1_north_df, mapping = aes(x=dswe, y=stat(count),fill = "12-19 Feb.", color = "12-19 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-19 Feb.","19-26 Feb."),
                      values = c("darkorchid4","goldenrod"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-19 Feb.","19-26 Feb."),
                    values = c("darkorchid4","goldenrod"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  # ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = 'none') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

plot(north_12_v1)

north_1_lab <-c(paste0("Mean = ",p1_mean_north,"\n","SD = ",p1_sd_north))
north_1_lab

north_2_lab <-c(paste0("Mean = ",p2_mean_north,"\n","SD = ",p2_sd_north))
north_2_lab

north_12 <-north_12_v1 +
  annotate(geom="text", x=4.2, y=1.1e6, label= "North Facing", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12 - 19 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= north_1_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "19 - 26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= north_2_lab)

print(north_12)










####################
####################
## south pair 1 & 2
####################
####################

south_12_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p2_south_df, mapping = aes(x=dswe, y=stat(count),fill = "19-26 Feb.", color = "19-26 Feb."), alpha=0.1) +
  geom_density(p1_south_df, mapping = aes(x=dswe, y=stat(count),fill = "12-19 Feb.", color = "12-19 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-19 Feb.","19-26 Feb."),
                      values = c("darkorchid4","goldenrod"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-19 Feb.","19-26 Feb."),
                    values = c("darkorchid4","goldenrod"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  # ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = 'none') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

plot(south_12_v1)

south_1_lab <-c(paste0("Mean = ",p1_mean_south,"\n","SD = ",p1_sd_south))
south_1_lab

south_2_lab <-c(paste0("Mean = ",p2_mean_south,"\n","SD = ",p2_sd_south))
south_2_lab

south_12 <-south_12_v1 +
  annotate(geom="text", x=4.2, y=1.1e6, label= "South Facing", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12 - 19 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= south_1_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "19 - 26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= south_2_lab)

print(south_12)

## grid arrange
p1p2 <-plot_grid(fs_12, vg_12, north_12, south_12,
          labels = c("(a)","(b)","(c)","(d)"),
          # align = "v", 
          nrow = 1,
          ncol = 4,
          vjust = 3.5,
          hjust = -2,
          rel_widths = c(.28,.24,.24,.24))

plot(p1p2)


y.grob <- textGrob('Count',
                   gp=gpar(fontface="bold", col="black", fontsize=17), rot=90)

final_p1p2 <-grid.arrange(arrangeGrob(p1p2, left = y.grob))


ggsave2("/Users/jacktarricone/ch1_jemez/plots/dswe_p1p2_v1.pdf",
        final_p1p1,
       width = 18, 
       height = 4,
       units = "in",
       dpi = 500)










####################
####################
## Full Study Area pair 3 & 4
####################
####################

fs_34_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p4_fs_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb. Cumulative", color = "12-26 Feb. Cumulative"), alpha=0.1) +
  geom_density(p3_fs_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb.", color = "12-26 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-26 Feb.", "12-26 Feb. CM"),
                      values = c("darkred","darkblue"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-26 Feb.","12-26 Feb. CM"),
                    values = c("darkred","darkblue"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  # xlab("SWE Change (cm)") + 
  ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = c(.21,.82)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

plot(fs_34_v1)

fs_3_lab <-c(paste0("Mean = ",p3_mean_fs,"\n","SD = ",p3_sd_fs))
fs_3_lab

fs_4_lab <-c(paste0("Mean = ",p4_mean_fs,"\n","SD = ",p4_sd_fs))
fs_4_lab

fs_34 <- fs_34_v1 + 
  annotate(geom="text", x=4.2, y=1.1e6, label= "Full Study Area", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12-26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= fs_3_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "12-26 Feb. CM", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= fs_4_lab)

print(fs_34)











####################
####################
## vg pair 3 & 4
####################
####################

vg_34_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p4_vg_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb. Cumulative", color = "12-26 Feb. Cumulative"), alpha=0.1) +
  geom_density(p3_vg_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb.", color = "12-26 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-26 Feb.", "12-26 Feb. CM"),
                      values = c("darkred","darkblue"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-26 Feb.","12-26 Feb. CM"),
                    values = c("darkred","darkblue"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  # xlab("SWE Change (cm)") + 
  # ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = 'none') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

plot(vg_34_v1)

vg_3_lab <-c(paste0("Mean = ",p3_mean_vg,"\n","SD = ",p3_sd_vg))
vg_3_lab

vg_4_lab <-c(paste0("Mean = ",p4_mean_vg,"\n","SD = ",p4_sd_vg))
vg_4_lab

vg_34 <- vg_34_v1 + 
  annotate(geom="text", x=4.2, y=1.1e6, label= "VG", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12-26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= vg_3_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "12-26 Feb. CM", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= vg_4_lab)

print(vg_34)















####################
####################
## north pair 3 & 4
####################
####################

north_34_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p4_north_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb. Cumulative", color = "12-26 Feb. Cumulative"), alpha=0.1) +
  geom_density(p3_north_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb.", color = "12-26 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-26 Feb.", "12-26 Feb. CM"),
                      values = c("darkred","darkblue"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-26 Feb.","12-26 Feb. CM"),
                    values = c("darkred","darkblue"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  # xlab("SWE Change (cm)") + 
  # ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = 'none') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

plot(north_34_v1)

north_3_lab <-c(paste0("Mean = ",p3_mean_north,"\n","SD = ",p3_sd_north))
north_3_lab

north_4_lab <-c(paste0("Mean = ",p4_mean_north,"\n","SD = ",p4_sd_north))
north_4_lab

north_34 <- north_34_v1 + 
  annotate(geom="text", x=4.2, y=1.1e6, label= "North Facing", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12-26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= north_3_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "12-26 Feb. CM", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= north_4_lab)

print(north_34)












####################
####################
## south pair 3 & 4
####################
####################

south_34_v1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p4_south_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb. Cumulative", color = "12-26 Feb. Cumulative"), alpha=0.1) +
  geom_density(p3_south_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb.", color = "12-26 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-26 Feb.", "12-26 Feb. CM"),
                      values = c("darkred","darkblue"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-26 Feb.","12-26 Feb. CM"),
                    values = c("darkred","darkblue"))+
  scale_x_continuous(limits = c(-8,8), 
                     breaks = seq(-8,8,2), 
                     expand = c(0,0)) + 
  # xlab("SWE Change (cm)") + 
  # ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = 'none') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

plot(south_34_v1)

south_3_lab <-c(paste0("Mean = ",p3_mean_south,"\n","SD = ",p3_sd_south))
south_3_lab

south_4_lab <-c(paste0("Mean = ",p4_mean_south,"\n","SD = ",p4_sd_south))
south_4_lab

south_34 <- south_34_v1 + 
  annotate(geom="text", x=4.2, y=1.1e6, label= "South Facing", size = 5, fontface = "bold") +
  annotate(geom="text", x=4.2, y=9.5e5, label= "12-26 Feb.", fontface = "bold") +
  annotate(geom="text", x=4.2, y=8.5e5, label= south_3_lab) +
  annotate(geom="text", x=4.2, y=7e5, label= "12-26 Feb. CM", fontface = "bold") +
  annotate(geom="text", x=4.2, y=6e5, label= south_4_lab)

print(south_34)


## grid arrange
p3p4 <-plot_grid(fs_34, vg_34, north_34, south_34,
          labels = c("(e)","(f)","(g)","(h)"),
          # align = "v", 
          nrow = 1,
          ncol = 4,
          vjust = 3.5,
          hjust = -2,
          rel_widths = c(.28,.24,.24,.24))

plot(p3p4)

x.grob <- textGrob(expression(Delta ~ 'SWE (cm)'),
                   gp=gpar(fontface="bold", col="black", fontsize=24))

y.grob <- textGrob('Count',
                   gp=gpar(fontface="bold", col="black", fontsize=17), rot=90)

final_p3p4 <-grid.arrange(arrangeGrob(p3p4, left = y.grob, bottom = x.grob))
plot(final_p3p4)

ggsave2("/Users/jacktarricone/ch1_jemez/plots/dswe_p3p4_v2.pdf",
        final_p3p4,
       width = 18, 
       height = 4,
       units = "in",
       dpi = 500)

### stitch both together

# final_1234 <-grid.arrange(
#   grobs = list(final_p1p2,
#   final_p3p4),
#   nrow = 2,
#   heigths = c(.3,.7))

final_1234 <-plot_grid(final_p1p2, final_p3p4, align = "v", nrow = 2, rel_heights = c(.48, .52))

ggsave2("/Users/jacktarricone/ch1_jemez/plots/dswe_hist_full_v3.pdf",
        final_1234,
        width = 18, 
        height = 8,
        units = "in",
        dpi = 500)



























