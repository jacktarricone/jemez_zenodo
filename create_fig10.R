# gpr swe vs isce insar swe
# august 3, 2022

# updating my old code with the new feb12-26 bias corrected raster ryan sent me
# and new isce generated 12-26 interferogram

library(terra)
library(ggplot2);theme_set(theme_classic(12))
library(dplyr)

setwd("/Users/jacktarricone/ch1_jemez_data/")

#######
#######
## read in swe change data
dswe_isce <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb12-26_sp.tif") # isce producted insar pair
dswe_cm <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb12-26_cumulative.tif") # cumulative insar pair
dswe_isce
dswe_cm

# bring in 2/12-2/26 gpr data
gpr_feb26_minus_feb12_v1 <-rast("./gpr_swe_bias/feb26_minus_Feb12_bias_corrected1.tif")
gpr_feb26_minus_feb12 <-gpr_feb26_minus_feb12_v1/10 # convert to cm from mm
plot(gpr_feb26_minus_feb12)
hist(gpr_feb26_minus_feb12, breaks = 50)
#global(gpr_feb26_minus_feb12,mean,na.rm=T)

# resample gpr to same grid as unw, crop ext
dswe_isce_crop <-crop(dswe_isce, ext(gpr_feb26_minus_feb12)) # crop
dswe_cm_crop <-crop(dswe_cm, ext(gpr_feb26_minus_feb12)) # crop
gpr_feb26_minus_feb12 # check
dswe_isce_crop # check
dswe_cm_crop # check

# test plot
plot(dswe_isce_crop)
plot(dswe_cm_crop)
plot(gpr_feb26_minus_feb12, add = TRUE, col = "red")

# mask unw data with gpr
dswe_isce_crop_mask <-mask(dswe_isce_crop, gpr_feb26_minus_feb12, maskvalue = NA)
dswe_cm_crop_mask <-mask(dswe_cm_crop, gpr_feb26_minus_feb12, maskvalue = NA)
f26_m_12_mask <-mask(gpr_feb26_minus_feb12, dswe_isce_crop_mask, maskvalue = NA)

# plot only pixels that have data for both gpr and unw
plot(dswe_isce_crop_mask)
plot(dswe_cm_crop_mask)
plot(f26_m_12_mask, add = TRUE, col = hcl.colors(12, "Berlin"))

# convert raster to dataframe
isce_df <-as.data.frame(dswe_isce_crop_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
cm_df <-as.data.frame(dswe_cm_crop_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
gpr_df <-as.data.frame(f26_m_12_mask, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(gpr_df)
head(isce_df)
head(cm_df)

# bind the data frames
plotting_df <-cbind(isce_df, cm_df[,4] ,gpr_df[,4])
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
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ylim(c(-10,10)) + xlim(c(-10,10))+
  geom_point(aes(y = dswe_insar_isce, x = dswe_gpr, color = "isce"), alpha = .5) +
  geom_point(aes(y = dswe_insar_cm, x = dswe_gpr, color = "cm"), alpha = .5) +
  scale_color_manual(name = "InSAR Pairs",
                     values = c('isce' = 'darkred', 'cm' = 'goldenrod'),
                     labels = c('12-26 Feb.', '12-26 Feb. Cumulative'))+
  labs(x = Delta~"SWE GPR [cm]",
       y = Delta~"SWE InSAR [cm]")+
  theme(legend.position = c(.78,.80))
    
# save
ggsave("/Users/jacktarricone/ch1_jemez_data/plots/fig10.pdf",
       width = 5, 
       height = 5,
       units = "in",
       dpi = 400)




