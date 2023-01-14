# gpr swe vs isce insar swe
# august 3, 2022

library(terra)
library(ggplot2);theme_set(theme_classic(12))
library(dplyr)

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
#######
## read in swe change data
dswe <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb12-26_sp.tif") # 
dswe_cm <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb12-26_cumulative.tif") # cumulative insar pair
dswe
dswe_cm

# bring in 2/12-2/26 gpr data
gpr_feb26_minus_feb12_v1 <-rast("./gpr_swe_bias/feb26_minus_Feb12_bias_corrected1.tif")
gpr_feb26_minus_feb12 <-gpr_feb26_minus_feb12_v1/10 # convert to cm from mm
plot(gpr_feb26_minus_feb12)
hist(gpr_feb26_minus_feb12, breaks = 50)
#global(gpr_feb26_minus_feb12,mean,na.rm=T)

# resample gpr to same grid as unw, crop ext
dswe_crop <-crop(dswe, ext(gpr_feb26_minus_feb12)) # crop
dswe_cm_crop <-crop(dswe_cm, ext(gpr_feb26_minus_feb12)) # crop
gpr_feb26_minus_feb12 # check
dswe_crop # check
dswe_cm_crop # check

# test plot
plot(dswe_crop)
plot(dswe_cm_crop)
plot(gpr_feb26_minus_feb12, add = TRUE, col = "red")

# mask unw data with gpr
dswe_crop_mask <-mask(dswe_crop, gpr_feb26_minus_feb12, maskvalue = NA)
dswe_cm_crop_mask_v1 <-mask(dswe_cm_crop, gpr_feb26_minus_feb12, maskvalue = NA)
dswe_cm_crop_mask <-mask(dswe_cm_crop_mask_v1, dswe_crop_mask, maskvalue = NA)
f26_m_12_mask <-mask(gpr_feb26_minus_feb12, dswe_crop_mask, maskvalue = NA)

# plot only pixels that have data for both gpr and unw
plot(dswe_crop_mask)
plot(dswe_cm_crop_mask)
plot(f26_m_12_mask, add = TRUE, col = hcl.colors(12, "Berlin"))

# convert raster to dataframe
df <-as.data.frame(dswe_crop_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
cm_df <-as.data.frame(dswe_cm_crop_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
gpr_df <-as.data.frame(f26_m_12_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(gpr_df)
head(isce_df)
head(cm_df)

# bind the data frames
plotting_df <-cbind(df, cm_df[,4] ,gpr_df[,4])
head(plotting_df)
colnames(plotting_df)[4] <- "dswe_insar_isce" # rename col 4
colnames(plotting_df)[5] <- "dswe_insar_cm" # rename col 5
colnames(plotting_df)[6] <- "dswe_gpr" # rename col 6
head(plotting_df)

# quick hists
hist(plotting_df$dswe_gpr, breaks = 20)
hist(plotting_df$dswe_insar_isce, breaks = 20)
hist(plotting_df$dswe_insar_cm, breaks = 20)

####
# plotting
####
ggplot(plotting_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  ylim(c(-10,10)) + xlim(c(-10,10))+
  geom_point(aes(y = dswe_insar_isce, x = dswe_gpr, color = "isce"), alpha = .5, size = 1) +
  geom_point(aes(y = dswe_insar_cm, x = dswe_gpr, color = "cm"), alpha = .5, size = 1) +
  scale_color_manual(name = "InSAR Pair",
                     values = c('isce' = 'darkviolet', 'cm' = 'goldenrod'),
                     labels = c('12-26 Feb.', '12-26 Feb. CM'))+
  labs(x = Delta~"SWE GPR (cm)",
       y = Delta~"SWE InSAR (cm)")+
  theme_classic(12) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  theme(legend.position = c(.3,.83))
    
# save
ggsave("/Users/jacktarricone/ch1_jemez/plots/gpr_insar_fig10.pdf",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 400)




