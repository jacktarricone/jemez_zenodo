# atmosphereic correction for 12-26 pair
# jack tarricone
# january 10th, 2023

library(terra)
library(ggplot2)

# set home folder
setwd("/Users/jacktarricone/ch1_jemez/")
list.files() #pwd

# path length raster 
plv_km_raw <-rast("./gpr_rasters_ryan/plv_km.tif")
plv_km_raw
plot(plv_km_raw)

# bring in coherence raster for masking
cor <-rast("./feb12-26/HH/alamos_35915_20005-003_20013-000_0014d_s01_L090HH_01.cor.grd.tiff")
cor
plot(cor)

# create masking raster with smallest possible extent
cor_mask <-cor
plv_km_crop <-resample(plv_km_raw, cor_mask)
cor_mask_v2 <-mask(cor_mask, plv_km_crop, maskvalue=NA)
plv_masked <-mask(plv_km_crop, cor_mask_v2, maskvalue=NA)
plv_masked
plot(plv_masked, add = TRUE)

##############
### bring in all the insar data 
##############

# unw
unw_raw <-rast("./feb12-26/HH/alamos_35915_20005-003_20013-000_0014d_s01_L090HH_01.unw.grd.tiff")
unw_raw
plot(unw_raw, add = TRUE)

#########################################
## resample and crop to one size ########
#########################################

unw_masked <-mask(unw_raw, plv_masked, maskvalue=NA)

#### crop down to largest size possible with all overlapping pixels
# create new rast, set non NA values to 0 for unw
unw_non_na <-unw_masked
values(unw_non_na)[!is.na(unw_non_na[])] = 1
plot(unw_non_na)

# same thing for plv
plv_resamp_non_na <-plv_masked

# crop plv with unw, this leaves only the cells that exist in both data sets for plotting
plv_unw_mask <-mask(plv_resamp_non_na, unw_non_na, maskvalues=NA)
plot(plv_unw_mask)

# test plot, looks good
plot(plv_masked)
plot(unw_masked, add = TRUE)
plot(plv_unw_mask, add = TRUE)

########################################
## bring in the no snow mask ###########
########################################

# using the snow mask, only analyze pixels that have no snow to check for atmospheric delay
# we do this because we're assuming there is some snow signal combine with atm signal in no pixels
# by doing just these, in theory we're just focusing on the atmospheric portion

### snow mask
snow_mask_raw <-rast("./gpr_rasters_ryan/landsat_fsca_2-18.tif")
plot(snow_mask_raw)

# clip edges off no snow mask to make it same size as plv and unw
sm_v1 <-resample(snow_mask_raw, unw_masked)
snow_mask <-mask(sm_v1, unw_raw, maskvalue = NA)
plot(snow_mask)

#### snow unw and plv
# snow unw
snow_unw <-mask(unw_masked, snow_mask, maskvalue = NA)
plot(snow_unw)
snow_unw

# snow plv
snow_plv <-mask(plv_unw_mask, snow_unw, maskvalue = NA)
plot(snow_plv)
snow_plv

### convert no snow plv and unw rasters to dataframes, rename data columns
# unw
unw_df <-as.data.frame(snow_unw, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(unw_df)[4] <- "unwrapped_phase"
head(unw_df)
hist(unw_df$unwrapped_phase, breaks = 100) #quick hist to check

#plv
plv_df <-as.data.frame(snow_plv, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(plv_df)[4] <- "plv_km"
head(plv_df)
hist(plv_df$plv_km, breaks = 100) #quick hist to check

#bind last column on for future plot
snow_df <-cbind(unw_df, plv_df$plv_km)
colnames(snow_df)[5] <- "plv_km"
head(snow_df)

# run linear model to plot trend line
lm_fit <-lm(snow_df$unwrapped_phase ~ snow_df$plv_km)
summary(lm_fit)

# create new df for lm and plotting on graph
head(snow_df)
lm_df <-snow_df[-c(1:3)]
names(lm_df)[1:2] <-c("y","x")
head(lm_df)

# function for running lm, plotting equation and r2 
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

# create eq
eq_label <-lm_eqn(lm_df)
print(eq_label)

########################################
########### unw vs plv #################
########################################

theme_set(theme_classic(12))

p12 <-ggplot(snow_df, aes(plv_km, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "white", high = "seagreen") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  annotate("text", x = 14, y = 4, parse = TRUE,
           label = "italic(y) == \"0.12\" + \"0.14\" %.% italic(x) * \",\" ~ ~italic(r)^2 ~ \"=\" ~ \"0.39\"") +
  ylim(-5,5) + xlim(10,30)+
  labs(#title = "Jemez Radar Path Length vs. Unwrapped Phase 2/12-2/19",
       x = "PLV (km)",
       y = "Unwrapped Phase (radians)")+
  theme(legend.position = c(.85, .30),
        legend.key.size = unit(.5, 'cm'))

print(p12)

# print(p12)
# label <-lm_eqn(lm_df)
# p_text <- p12 + geom_text(x = 15, y = 8, label = label, parse = TRUE)
# print(p_text)

# save
ggsave(p12,
       file = "/Users/jacktarricone/ch1_jemez_data/plots/fig05.pdf",
       width = 6,
       height = 4,
       dpi = 400)


### correct unw data using path length and the linear estimation we generated

path_length_correction <-function(unw, plv){
  return((unw - ((plv * coef(lm_fit)[[2]]) + coef(lm_fit)[[1]])))
  }

unw_corrected <-path_length_correction(unw_masked, plv_masked)
plot(unw_corrected)

writeRaster(unw_corrected, "./gpr_rasters_ryan/unw_corrected_new_feb12-26.tif")

# test plot with corrected data

unw_corrected_df <-as.data.frame(unw_corrected, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(unw_corrected_df)[4] <- "unwrapped_phase"
head(unw_corrected_df)

p13 <-ggplot(unw_corrected_df, aes(x, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "grey90", high = "red") +
  labs(title = "Jemez Unwrapped Phase Corrected",
       x = "Longitude (degrees)",
       y = "Unwrapped Phase (radians)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
print(p13)


ggsave(p13,
       file = "jemez_phase_corrected.png",
       width = 6, 
       height = 4,
       dpi = 400)
