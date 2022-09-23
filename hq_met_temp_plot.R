# jack Tarricone
# sept 23 2022
# remake temp plot

library(lubridate)
library(dplyr)
library(ggplot2);theme_set(theme_classic(19))

# read back in the proper data!!!
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v2.csv")
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time)
vg_met_data$date <-mdy(vg_met_data$date)

# filter down to time range between insar pair one
pair_1 <-filter(vg_met_data, date >= "2020-02-12" & date <="2020-02-26")

# test plot pair 1
ggplot(pair_1) +
  geom_line(aes(x = date_time, y = mean_air_temp_c))

# define flight times
flight1 <-pair_1$date_time[9]
flight2 <-pair_1$date_time[177]
flight3 <-pair_1$date_time[345]

# plot
theme_set(theme_classic(18))
ggplot(pair_1) +
  geom_hline(yintercept = 0, linetype=3, col = "grey50", alpha = .7) +
  geom_vline(xintercept = flight1, linetype=1, col = "blue", alpha = .5) +
  geom_vline(xintercept = flight2, linetype=1, col = "blue", alpha = .5) +
  geom_vline(xintercept = flight3, linetype=1, col = "blue", alpha = .5) +
  geom_line(aes(x = date_time, y = mean_air_temp_c), col = "black") + 
  scale_x_datetime(breaks = "2 day", date_labels="%d", limits = ymd_hm(c("2020-02-12 01:00", "2020-02-26 23:00")))+
  scale_y_continuous(breaks = seq(-20,10,5))+
  xlab("Day of Feb") + ylab("Air Temperature (°C)") + 
  theme(
    axis.title.x = element_text(size = 14),
    #axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))
  
# save
setwd("/Users/jacktarricone/ch1_jemez_data/plots")
ggsave("hq_met_temp_v6.png",
       width = 6, 
       height = 3,
       units = "in",
       dpi = 400)

# solar rad plot
# ggplot(pair_1) +
#   geom_hline(yintercept = 0, linetype=3, col = "red", alpha = .5) +
#   geom_vline(xintercept = pair_1$date_time[7], linetype=1, col = "blue", alpha = .5) +
#   geom_vline(xintercept = pair_1$date_time[177], linetype=1, col = "blue", alpha = .5) +
#   geom_vline(xintercept = pair_1$date_time[345], linetype=1, col = "blue", alpha = .5) +
#   geom_line(aes(x = date_time, y = solar_rad_kwh), col = "black") + 
#   scale_x_datetime(breaks = "2 day", date_labels="%b %d", limits = ymd_hm(c("2020-02-12 01:00", "2020-03-04 23:00")))+
#   scale_y_continuous(breaks = seq(0,1,.2))+
#   xlab("Date") + ylab("Incoming Solar (kWh)")
# 
# 
# setwd("/Users/jacktarricone/ch1_jemez_data/plots")
# ggsave("hq_met_solar.png",
#        width = 8, 
#        height = 3,
#        units = "in",
#        dpi = 300)

              