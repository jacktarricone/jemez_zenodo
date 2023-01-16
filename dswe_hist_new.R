# masking histogram for swe plots in manuscript
# july 19th, 2022
# jack tarricone

library(terra)
library(ggplot2)
library(cowplot)

setwd("/Users/jacktarricone/ch1_jemez/")
list.files()

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

# vg extent
vg <-vect("./vector_data/valle_grande_aoi.geojson")

# pair 1
feb12_19_r <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb12-19_sp.tif")
p1 <-crop(feb12_19_r, vg)
hist(p1, breaks = 70)
plot(p1)

# pair 2
feb19_26_r <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb19-26_sp.tif")
p2 <-crop(feb19_26_r, vg)
hist(p2, breaks = 70)
plot(p2)

# pair 3
feb12_26_r <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb12-26_sp.tif")
p3 <-crop(feb12_26_r, vg)
hist(p3, breaks = 70)
plot(fp3)

# pair 4
feb12_26_cm_r <-rast("./gpr_rasters_ryan/new_swe_change/dswe_feb12-26_cumulative.tif")
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

# density plot for feb 12-19 and feb 19-26
plot_1 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p2_df, mapping = aes(x=dswe, y=stat(count),fill = "19-26 Feb.", color = "19-26 Feb."), alpha=0.1) +
  geom_density(p1_df, mapping = aes(x=dswe, y=stat(count),fill = "12-19 Feb.", color = "12-19 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-19 Feb.","19-26 Feb."),
                      values = c("darkorchid4","goldenrod"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-19 Feb.","19-26 Feb."),
                    values = c("darkorchid4","goldenrod"))+
  scale_x_continuous(limits = c(-10,10), 
                     breaks = seq(-10,10,2), 
                     expand = c(0,0)) + 
  ylab("Count") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.2e6), labels=fancy_scientific) +
  theme(legend.position = c(.8,.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
                                axis.title.x=element_blank())

plot(plot_1)

# density plot for 12026
plot_2 <-ggplot()+
  geom_vline(xintercept = 0, linetype=3, col = "black") +
  geom_density(p4_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb. Cumulative", color = "12-26 Feb. Cumulative"), alpha=0.1) +
  geom_density(p3_df, mapping = aes(x=dswe, y=stat(count),fill = "12-26 Feb.", color = "12-26 Feb."), alpha=0.1) +
  scale_colour_manual(name = "InSAR Pair",
                      labels = c("12-26 Feb.", "12-26 Feb. CM"),
                      values = c("darkred","darkblue"))+
  scale_fill_manual(name = "InSAR Pair",
                    labels = c("12-26 Feb.","12-26 Feb. CM"),
                    values = c("darkred","darkblue"))+
  scale_x_continuous(limits = c(-10,10), 
                     breaks = seq(-10,10,2), 
                     expand = c(0,0)) + 
  xlab("SWE Change (cm)") + 
  ylab("Count") +
  scale_y_continuous(labels=fancy_scientific) +
  theme(legend.position = c(.8,.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1))

plot(plot_2)

# stack with cow plot
plot_grid(plot_1, plot_2,
          labels = c("(a)","(b)"),
          align = "v", 
          nrow = 2,
          vjust = 1.5,
          hjust = -.2,
          rel_heights = c(1/2, 1/2))


ggsave("./plots/dswe_hist_new.pdf",
       width = 7, 
       height = 5,
       units = "in",
       dpi = 500)

  