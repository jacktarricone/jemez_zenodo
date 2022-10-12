# masking histogram for swe plots in manuscript
# july 19th, 2022
# jack tarricone

library(terra)
library(ggplot2)
library(ggpubr)

setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/new_swe_change")
list.files()

# vg extent
vg <-vect("/Users/jacktarricone/ch1_jemez_data/vector_data/valle_grande_aoi.geojson")

# pair 1
feb12_19_r <-rast("dswe_feb12-19_sp.tif")
p1 <-crop(feb12_19_r, vg)
hist(p1, breaks = 70)
plot(p1)

# pair 2
feb19_26_r <-rast("dswe_feb19-26_sp.tif")
p2 <-crop(feb19_26_r, vg)
hist(p2, breaks = 70)
plot(p2)

# pair 3
feb12_26_r <-rast("dswe_feb12-26_sp.tif")
p3 <-crop(feb12_26_r, vg)
hist(p3, breaks = 70)
plot(fp3)

# pair 4
feb12_26_cm_r <-rast("dswe_feb12-26_cumulative.tif")
p4 <-crop(feb12_26_cm_r, vg)
hist(p4, breaks = 70)
plot(p4)

# convert to data frames for plotting
p1_df <-as.data.frame(p1, xy = TRUE, cells = TRUE, na.rm = TRUE)
names(p1_df)[4] <-"dswe"

p2_df <-as.data.frame(p2, xy = TRUE, cells = TRUE, na.rm = TRUE)
names(p2_df)[4] <-"dswe"

p3_df <-as.data.frame(p3, xy = TRUE, cells = TRUE, na.rm = TRUE)
names(p3_df)[4] <-"dswe"

p4_df <-as.data.frame(p4, xy = TRUE, cells = TRUE, na.rm = TRUE)
names(p4_df)[4] <-"dswe"



# density plot
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

# set theme
theme_set(theme_classic(base_size =12))

# density plot for first two pairs
p1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  #geom_density(p4_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb. Cumulative", color = "12-26 Feb. Cumulative"), alpha=0.1) +
  #geom_density(p3_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb.", color = "12-26 Feb."), alpha=0.1) +
  geom_density(p2_df, mapping = aes(x=dswe, y=stat(count),fill = "19-26 Feb.", color = "19-26 Feb."), alpha=0.1) +
  geom_density(p1_df, mapping = aes(x=dswe, y=stat(count),fill = "12-19 Feb.", color = "12-19 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-19 Feb.","19-26 Feb."),
                      values = c("darkorchid4","goldenrod"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-19 Feb.","19-26 Feb."),
                    values = c("darkorchid4","goldenrod"))+
  xlim(c(-10,10)) + xlab("SWE Change [cm]")+ylab("Count")+
  scale_y_continuous(labels=fancy_scientific) +
  theme(legend.position = c(.80,.75))

# density plot for 12026
p2 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p4_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb. Cumulative", color = "12-26 Feb. Cumulative"), alpha=0.1) +
  geom_density(p3_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb.", color = "12-26 Feb."), alpha=0.1) +
  #geom_density(p2_df, mapping = aes(x=dswe, y=stat(count),fill = "19-26 Feb.", color = "19-26 Feb."), alpha=0.1) +
  #geom_density(p1_df, mapping = aes(x=dswe, y=stat(count),fill = "12-19 Feb.", color = "12-19 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-26 Feb.", "12-26 Feb. Cumulative"),
                      values = c("darkred","darkblue"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-26 Feb.","12-26 Feb. Cumulative"),
                    values = c("darkred","darkblue"))+
  xlim(c(-10,10)) + xlab("SWE Change [cm]")+ylab("Count")+
  scale_y_continuous(labels=fancy_scientific) +
  theme(legend.position = c(.80,.75))




# box and whisker
ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_boxplot(p4_df, mapping = aes(y=lyr.1), color = "12-26 Feb. Cumulative") +
  geom_boxplot(p3_df, mapping = aes(y=lyr.1), color = "12-26 Feb.") +
  geom_boxplot(p2_df, mapping = aes(y=lyr.1), color = "19-26 Feb.") +
  geom_boxplot(p1_df, mapping = aes(y=lyr.1), color = "12-19 feb.") +
  scale_colour_manual(name = "InSAR Dates",
                      labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th", "Feb 12-26th Cumulative"),
                      values=c("darkblue","goldenrod","black","darkolivegreen"))+
  scale_fill_manual(name = "InSAR Dates",
                    labels = c("Feb 12-19th","Feb 19-26th","Feb 12-26th","Feb 12-26th Cumulative"),
                    values = c("darkblue","goldenrod","black","darkolivegreen"))+
  xlim(c(-10,10)) + xlab("SWE Change [cm]")+ylab("Count")+
  scale_y_continuous(labels=fancy_scientific) +
  theme(legend.position = c(.80,.75))


setwd("/Users/jacktarricone/ch1_jemez_data/plots")
ggsave("dswe_hist_new.png",
       width = 6, 
       height = 4,
       units = "in",
       dpi = 300)

# global(cum, mean, na.rm = TRUE)
# global(feb12_19, mean, na.rm = TRUE)
# global(feb19_26, mean, na.rm = TRUE)
  