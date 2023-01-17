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

# plv
plv_df <-as.data.frame(snow_plv, xy=TRUE, cells=TRUE, na.rm=TRUE)
colnames(plv_df)[4] <- "plv_km"
head(plv_df)
hist(plv_df$plv_km, breaks = 100) #quick hist to check

# bind last column on for future plot
plotting_df <-cbind(unw_df, plv_df$plv_km)
head(plotting_df)
colnames(plotting_df)[5] <- "plv_km"
head(plotting_df)

# plot unw vs plv
# call stat smooth function for trend line
stat_smooth_func2 <- function(mapping = NULL, data = NULL,
                              geom = "smooth", position = "identity",
                              ...,
                              method = "auto",
                              formula = y ~ x,
                              se = TRUE,
                              n = 80,
                              span = 0.75,
                              fullrange = FALSE,
                              level = 0.95,
                              method.args = list(),
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              xpos = NULL,
                              ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq1 <- substitute(italic(y) == a + b %.% italic(x), 
                                              list(a = format(coef(m)[[1]], digits = 3), 
                                                   b = format(coef(m)[[2]], digits = 3),
                                                   r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq1))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)


#######################################
# run linear model to plot trend line #
#######################################
lm_fit <-lm(plotting_df$unwrapped_phase ~ plotting_df$plv_km)
summary(lm_fit)

########################################
########### unw vs plv #################
########################################

theme_set(theme_light(base_size =12))

p9 <-ggplot(plotting_df, aes(plv_km, unwrapped_phase)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "white", high = "firebrick") +
  stat_smooth_func2(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_abline(slope = coef(lm_fit)[[2]], intercept = coef(lm_fit)[[1]], size = 1)+
  #scale_y_continuous(breaks = seq(-5,6,2))+
  labs(#title = "Unwrapped Phase vs. Radar Look Vector Length 2/12-2/26 Pair",
       x = "LKV (km)",
       y = "Unwrapped Phase (radians)")+
  scale_x_reverse()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

print(p9)


path_length_correction <-function(unw, plv){
   atm_corrected <-unw - ((plv * coef(lm_fit)[[2]]) + coef(lm_fit)[[1]])
   return(atm_corrected)
}

# save original
unw_corrected <-path_length_correction(unw_raw, plv_unw_mask)
plot(unw_corrected)
# writeRaster(unw_corrected, "unw_corrected_feb12-26.tif")

# snow mask
snow_unw_corrected <-mask(unw_corrected, snow_mask)
plot(snow_unw_corrected)
# writeRaster(snow_unw_corrected, "snow_unw_corrected_feb12-26.tif")


###################
# test plot with corrected data
###################

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


# ggsave(p13,
#        file = "jemez_phase_corrected_12-26.png",
#        width = 6, 
#        height = 4,
#        dpi = 400)
