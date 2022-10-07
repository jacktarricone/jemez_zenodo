####### atmospheric correction #######
# for the feb 12th - feb 19th jemez data
# updated jan 28th


library(terra)
library(ggplot2)

# set home folder
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/")
list.files() #pwd

# path length raster 
plv_km <-rast("plv_km.tif")
plv_km
plot(plv_km)

##############
### bring in all the insar data 
##############

# unw
unw_raw <-rast("unw_raw_feb12-19.tif")
unw_raw
plot(unw_raw)

# cor
cor <-rast("cor_feb12-19.tif")
cor
plot(cor)


#########################################
## resample and crop to one size ########
#########################################

# resample look vector to unwrapped phase
plv_resamp <-resample(plv_km, unw_raw, method = "bilinear")
plv_resamp
ext(plv_resamp) <-ext(unw_raw) # set extent as same as unw
plv_resamp

# test plot
plot(unw_raw)
plot(plv_resamp, add = TRUE)

#### crop down to largest size possible with all overlapping pixels
# create new rast, set non NA values to 0 for unw
unw_non_na <-unw_raw
values(unw_non_na)[!is.na(unw_non_na[])] = 1
plot(unw_non_na)

# same thing for plv
plv_resamp_non_na <-plv_resamp
values(plv_resamp_non_na)[!is.na(plv_resamp_non_na[])] = 1
plot(plv_resamp_non_na)

# crop plv with unw, this leaves only the cells that exist in both data sets for plotting
plv_crop1 <-terra::mask(plv_resamp_non_na, unw_non_na, maskvalues=NA)
plv_unw_mask <-terra::mask(unw_non_na, plv_crop1, maskvalues=NA)

# test plot, looks good
plot(plv_resamp)
plot(unw_raw, add = TRUE)
plot(plv_unw_mask, add = TRUE)

# mask both unw and plv with the mask
unw_masked <-terra::mask(unw_raw, plv_unw_mask, maskvalues=NA)
plot(unw_masked)

plv_masked <-terra::mask(plv_resamp, plv_unw_mask, maskvalues=NA)
plot(plv_masked, add = TRUE)


########################################
## bring in the no snow mask ###########
########################################

# using the snow mask, only analyze pixels that have no snow to check for atmospheric delay
# we do this because we're assuming there is some snow signal combine with atm signal in no pixels
# by doing just these, in theory we're just focusing on the atmospheric protion

no_snow_mask <-rast("landsat_fsca_2-18.tif")
plot(no_snow_mask, add = TRUE)

# clip edges off no snow mask to make it same size as plv and unw
clipped_nsm <-mask(no_snow_mask, unw_masked, maskvalue = NA)
plot(clipped_nsm, add = TRUE)

# snow unw
snow_unw <-mask(unw_masked, clipped_nsm, maskvalue = NA)
plot(snow_unw)

# snow plv
snow_plv <-mask(plv_resamp, clipped_nsm, maskvalue = NA)
plot(snow_plv)

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

# create eq
eq_label <-lm_eqn(lm_df)
print(eq_label)

########################################
########### unw vs plv #################
########################################

p12 <-ggplot(snow_df, aes(plv_km, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "white", high = "seagreen") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  annotate("text", x = 14, y = 4, parse = TRUE,
           label = "italic(y) == \"-4.3\" + \"0.26\" %.% italic(x) * \",\" ~ ~italic(r)^2 ~ \"=\" ~ \"0.81\"") +
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
       file = "/Users/jacktarricone/ch1_jemez_data/plots/snow_plv_vs_unw_v3.png",
       width = 6,
       height = 4,
       dpi = 400)


### correct unw data using path length and the linear estimation we generated

path_length_correction <-function(unw, plv){
  return((unw - ((plv * coef(lm_fit)[[2]]) + coef(lm_fit)[[1]])))
  }
unw_corrected <-path_length_correction(unw_masked, plv_masked)
plot(unw_corrected)

writeRaster(unw_corrected, "unw_corrected_feb12-19.tif")

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
