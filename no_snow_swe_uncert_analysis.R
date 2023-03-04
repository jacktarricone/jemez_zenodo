# NO snow inversion for 2/12-2/26 on new JPL produced data
# jack tarricone
# january 10th, 2023

library(terra)
library(ggplot2)

# set home folder
setwd("/Users/jacktarricone/ch1_jemez/rasters")
list.files() #pwd

# bring in unmasked rasters
p1 <-rast("./no_snow_uncert/p1_dswe_no_mask.tif")
plot(p1)
p2 <-rast("./no_snow_uncert/p2_dswe_no_mask.tif")
plot(p2)
p3 <-rast("./no_snow_uncert/p3_dswe_no_mask.tif")
plot(p3)

# fsca
snow_mask <-rast("./fsca/study_area_02_18_2020_snow_mask.tif")

# p1 for no and snow
p1_snow <-mask(p1, snow_mask, maskvalue = NA)
plot(p1_snow)
p1_no <-mask(p1, snow_mask, maskvalue = NA, inverse = TRUE)
plot(p1_no)

# p2 for no and snow
p2_snow <-mask(p2, snow_mask, maskvalue = NA)
plot(p2_snow)
p2_no <-mask(p2, snow_mask, maskvalue = NA, inverse = TRUE)
plot(p2_no)

# p3 for no and snow
p3_snow <-mask(p3, snow_mask, maskvalue = NA)
plot(p3_snow)
p3_no <-mask(p3, snow_mask, maskvalue = NA, inverse = TRUE)
plot(p3_no)

# stack the no snow
p123_no <-c(p1_no,p2_no,p3_no)
p123_no

# stack the snow
p123_snow <-c(p1_snow,p2_snow,p3_snow)
p123_snow

# mean
t1 <-as.numeric(global(p123_no[[1]], mean, na.rm = TRUE))
t2 <-as.numeric(global(p123_no[[2]], mean, na.rm = TRUE))
t3 <-as.numeric(global(p123_no[[3]], mean, na.rm = TRUE))

sd1 <-as.numeric(global(p123_no[[1]], sd, na.rm = TRUE))
sd2 <-as.numeric(global(p123_no[[2]], sd, na.rm = TRUE))
sd3 <-as.numeric(global(p123_no[[3]], sd, na.rm = TRUE))

# test hists
hist(p123_no[[3]], breaks = 50, col = 'blue')
hist(p123_no[[2]], breaks = 50, col = 'red', add = TRUE, alpha = .5)
hist(p123_no[[1]], breaks = 50, add = TRUE)

hist(p123_snow[[3]], breaks = 100, col = 'blue')
hist(p123_snow[[2]], breaks = 100, col = 'red', add = TRUE, alpha = .5)
hist(p123_snow[[1]], breaks = 200, add = TRUE)

# make df for now snow
no_snow_df <-as.data.frame(p123_no, xy = TRUE)
colnames(no_snow_df)[3:5] <-c('p1','p2','p3')
head(no_snow_df)

# bind
all_no_dswe <-data.frame(c(no_snow_df$p1, no_snow_df$p2, no_snow_df$p3),
                         group = 'no_snow')

colnames(all_no_dswe)[1] <-c('dswe')
hist(all_no_dswe$dswe, breaks = 100)

total_no_mean <-mean(all_no_dswe$dswe, na.rm = TRUE)
total_no_sd <-sd(all_no_dswe$dswe, na.rm = TRUE)
total_no_iqr <-IQR(all_no_dswe$dswe, na.rm = TRUE)

# make df for now snow
snow_df <-as.data.frame(p123_snow, xy = TRUE)
colnames(snow_df)[3:5] <-c('p1','p2','p3')
head(snow_df)

# bind
all_snow_dswe <-data.frame(c(snow_df$p1, snow_df$p2, snow_df$p3), 
                           group = 'snow')
colnames(all_snow_dswe)[1] <-c('dswe')
hist(all_snow_dswe$dswe, breaks = 100)

total_snow_mean <-mean(all_snow_dswe$dswe, na.rm = TRUE)
total_snow_sd <-sd(all_snow_dswe$dswe, na.rm = TRUE)
total_snow_iqr <-IQR(all_snow_dswe$dswe, na.rm = TRUE)

hist(all_snow_dswe$dswe, breaks = 200)
hist(all_no_dswe$dswe, breaks = 20, add = TRUE, col = 'red')

# bind for plotting
plotting_df <-rbind(all_no_dswe, all_snow_dswe)

ggplot(plotting_df, aes(dswe, fill=group, colour=group)) +
  geom_density(alpha=0.4, lwd=0.8, adjust=0.5) +
  xlab('dSWE (cm)') +
  theme_classic(12)
  






# mean
mean_no <-as.numeric(global(dswe_no_snow, mean, na.rm = TRUE))
mean_no
mean_snow <-as.numeric(global(dswe_snow, mean, na.rm = TRUE))
mean_snow

# sd
sd_no <-as.numeric(global(dswe_no_snow, sd, na.rm = TRUE))
sd_no
sd_snow <-as.numeric(global(dswe_snow, sd, na.rm = TRUE))
sd_snow

# max
max_no <-as.numeric(global(dswe_no_snow, max, na.rm = TRUE))
max_no
max_snow <-as.numeric(global(dswe_snow, max, na.rm = TRUE))
max_snow

# min
min_no <-as.numeric(global(dswe_no_snow, min, na.rm = TRUE))
min_no
min_snow <-as.numeric(global(dswe_snow, min, na.rm = TRUE))
min_snow

# iqr
iqr_no <-as.numeric(global(dswe_no_snow, IQR, na.rm = TRUE))
iqr_no
iqr_snow <-as.numeric(global(dswe_snow, IQR, na.rm = TRUE))
iqr_snow

error <-sqrt((sd_no/mean_no)^2)
error

hist(dswe_snow, breaks = 200)
hist(dswe_no_snow, breaks = 20, add = TRUE, col = 'red')






# save
writeRaster(dswe_abs,"./new_swe_change/rough/no_snow_dswe_feb12-26_new.tif")
