# make hq pit bar graph
# jack tarricone
# december 30th

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)

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

theme_set(theme_classic(12))

# read in data
perm <-read.csv("/Users/jacktarricone/ch1_jemez/pit_data/hq_density_perm_formatted.csv")

# convert to date data type
perm$date <-ymd(perm$date)

#chop off 2020-
display_date <- gsub("2020-", "", perm$date)

#add new column
perm <-cbind(perm,display_date)

# take off zeros for graph
perm$display_date <-gsub("01-", "1/", perm$display_date)
perm$display_date <-gsub("02-", "2/", perm$display_date)
perm$display_date <-gsub("03-", "3/", perm$display_date)

# plot perm values by depth for all of the days
p1 <-ggplot(perm, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = avg_dielec), stat="identity", color = "black", width = .4) +
  scale_fill_continuous(high = "firebrick", low = "#f1eef6", name = expression("A2"~epsilon[s]),
                        limits = c(1.1, 2.1), breaks = c(1.1, 1.3, 1.5, 1.7, 1.9, 2.1)) +
  scale_y_continuous(breaks = seq(0,80,10), expand = c(0, 0), limits = c(0, 80)) +
  labs(y = "Depth (cm)", x = "Date") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1))

# # find color scale range
# min(perm$avg_density, na.rm = TRUE)
# max(perm$avg_density, na.rm = TRUE)
# hist(perm$avg_density, breaks = 40)

# plot density values by depth
p2 <-ggplot(perm, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = avg_density), stat="identity", color = "black", width = .4) +
  scale_fill_continuous(high = "darkblue", low = "#f1eef6", 
                        # name = expression(atop(textstyle("Density")))~(kg~m^{-3})),
                        name = expression(atop(textstyle("Density"),
                            (kg~m^{-3}))),
                        limits = c(120, 370), breaks = c(120, 170, 220, 270, 320, 370)) +
                        # limits = c(220, 370), breaks = c(220, 270, 370)) +
  scale_y_continuous(breaks = seq(0,80,10), expand = c(0, 0), limits = c(0, 80)) +
  labs(y = "Depth (cm)", x = "Date") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1))

# combine
figure <-ggarrange(p1, p2, 
                   labels = c("(a)", "(b)"),
                   label.x = .85, label.y = .2,
                   ncol = 1, nrow = 2)
plot(figure)

??ggarrange

setwd("/Users/jacktarricone/ch1_jemez/plots/drafts/")
ggsave("hq_pit.pdf",
       width = 6, 
       height = 5,
       units = "in",
       dpi = 500)


























# assign levels to location data so you can put in proper order for poster

neworder <- c("Tower 4","Forest","Open")
library(plyr)  ## or dplyr (transform -> mutate)
all_pits_good <- arrange(transform(all_pits_good,
                           location=factor(location,levels=neworder)),location)

# filter for single sites
tower4 <-filter(all_pits_good, location == "Tower 4")
open <-filter(all_pits_good, location == "Open")
forest <-filter(all_pits_good, location == "Forest")

# tower 4
ggplot(tower4, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = density), stat="identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"})),
                        limits = c(200, 500), breaks = c(200, 300, 400, 500)) +
  scale_y_continuous(breaks = seq(0,120,10)) +
  labs(title="Tower 4 Snow Pit Timeseries", y = "Depth (cm)", x = "Date")

# open
ggplot(open, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = density), stat="identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6",name = expression('Density'~(kg/m^{"3"})), 
                        limits = c(200, 500), breaks = c(200, 300, 400, 500)) +
  scale_y_continuous("Depth (cm)", limits = c(0, 115), breaks = seq(0, 120, by = 10)) +
  labs(title="Open Snow Pit Timeseries", x = "Date")

# forest
ggplot(forest, aes(x = display_date, y = seg_cm)) +
  geom_bar(aes(fill = density), stat="identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"})), 
                        limits = c(200, 500), breaks = c(200, 300, 400, 500)) +
  scale_y_continuous("Depth (cm)", breaks = seq(0, 120, by = 10), expand = c(0, 0), limits = c(0, NA)) +
  labs(title="Forest Snow Pit Timeseries", x = "Date")








##### pit avgs plots

# convert to date data type
pit_avgs$date <- as.Date(pit_avgs$date)

# chop off 2020-
display_date <- gsub("2020-", "", pit_avgs$date)

# add new column
pit_avgs <-cbind(pit_avgs, display_date)

# take off zeros for graph
pit_avgs$display_date <- gsub("01-", "1/", pit_avgs$display_date)
pit_avgs$display_date <- gsub("02-", "2/", pit_avgs$display_date)
pit_avgs$display_date <- gsub("03-", "3/", pit_avgs$display_date)


pit_avgs$depth_mm_graph <- pit_avgs$depth_mm - pit_avgs$swe_mm

#write.csv(pit_avgs, "/Volumes/JTARRICONE/UNR_fall_20/snowex/data/pits/pit_avgs.csv") 

neworder <- c("Tower 4","Forest","Open")
library(plyr)  ## or dplyr (transform -> mutate)
pit_avgs <- arrange(transform(pit_avgs,
                                   location=factor(location,levels=neworder)),location)

theme_set(theme_light(base_size =11))
ggplot(pit_avgs, aes(display_date, swe_mm)) +
  geom_bar(stat="identity", fill = "firebrick" ) +
  scale_y_continuous(breaks = seq(0,1200,100)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Creek SnowEx 2020 Snow Pit SWE Time Series", y = "SWE (mm)", x = "Date") 

ggplot(data = df, aes(x = dateValues, y = value, fill = variable)) + 
  geom_bar(stat = "identity")



ggplot(NULL, aes(lab, perc)) + 
  geom_bar(aes(fill = "dEQ"), data = dEQ, alpha = 0.5) +
  geom_bar(aes(fill = "LMD"), data = LMD, alpha = 0.5)

my_data_long <- melt(my_data, id.vars = c("Block"))

ggplot(data=my_data_long,aes(x=Block, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3, .8))

#####
# 3/22 swe vs cc test plot

ggplot(swe)+ 
  geom_point(aes(cc_percent, swe_20200322, color="03/22/2020"),size = 2) +
  scale_x_continuous("LiDAR Canopy Cover %", breaks = c(0,20,40,60,80,100)) +
  scale_y_continuous("SWE (mm)", breaks = c(0,50,100,150,200,250,300,350)) +
  labs(title="Sagehen Creek SWE vs. Canopy Cover %", 
       y="SWE (mm)", x="Distance along Transect (m)", color = "Date")



theme_set(theme_light(base_size =11))
ggplot(depth)+ 
  geom_line(aes(distance_m, depth_20201220, color="12/20/2019"),size = .7) +
  geom_line(aes(distance_m, depth_20200129, color="01/29/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200205, color="02/05/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200212, color="02/12/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200219, color="02/19/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200226, color="02/26/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200304, color="03/04/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200311, color="03/11/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200319, color="03/19/2020"),size = .7) +
  geom_line(aes(distance_m, depth_20200322, color="03/22/2020"),size = .7) +
  scale_x_continuous(breaks = seq(0,320,20)) +
  scale_y_continuous(breaks = seq(0,350,20)) +
  scale_colour_manual(values=c('12/20/2019'="red",'01/29/2020'="darkblue",
                               '02/05/2020' = "turquoise3", '02/12/2020' = "darkseagreen1", 
                               '02/19/2020' = "grey45", '02/26/2020' = "deeppink3",
                               '03/04/2020' = "red", '03/11/2020' = "green",
                               '03/19/2020' = "black", '03/22/2020' = "blue"))+
  labs(title="Sagehen Creek WY2020 Depth Transect", 
       y="Depth (cm)", x="Distance along Transect (m)", color = "Date")



### test with new data 9/15

# read in data
lwc_den <-read.csv("/Volumes/jt/UNR_fall_20/snowex/data_analysis/pits/lwc/lwc_density_r_new.csv")

# convert to date data type
lwc_den$date <- as.Date(lwc_den$date)

#chop off 2020-
display_date <- gsub("2020-", "", lwc_den$date)

#add new column
lwc_den<-cbind(lwc_den, display_date)

# take off zeros for graph
lwc_den$display_date <- gsub("01-", "1/", lwc_den$display_date)
lwc_den$display_date <- gsub("02-", "2/", lwc_den$display_date)
lwc_den$display_date <- gsub("03-", "3/", lwc_den$display_date)

#set chart order
neworder <- c("Tower 4","Forest","Open")
lwc_den <- arrange(transform(lwc_den,
                              location=factor(location,levels=neworder)),location)








# Webb LWC
# .037Es+((((Es-.051rho))-(3.17e-5)rhoe2)/158.8)-.08

lwc <- .037*(lwc_den$avg_dielec)+(((lwc_den$avg_dielec +.051*lwc_den$avg_density)-((3.17e-5)*(lwc_den$avg_density)^2))/158.8)-.08
lwc_den <-cbind(lwc_den, lwc)

# pit density time series

theme_set(theme_light(base_size =11))
density <-ggplot(lwc_den, aes(display_date, seg_cm)) +
  geom_bar(aes(fill = avg_density, group = location), stat = "identity") +
  scale_fill_continuous(high = "#034e7b", low = "#f1eef6", name = expression('Density'~(kg/m^{"3"}))) +
  scale_y_continuous(breaks = seq(0,120,10)) +
  facet_grid( ~ location) +
  labs(title="Sagehen Creek SnowEx 2020 Snow Pit Time Series", y = "Depth (cm)", x = "Date") 

print(density)








