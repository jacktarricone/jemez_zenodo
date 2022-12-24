# jack Tarricone
# sept 23 2022
# remake temp plot

library(lubridate)
library(dplyr)
library(ggplot2)

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

# read back in the proper data
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez/climate_station_data/vg/vg_met_data_v2.csv")
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time)
vg_met_data$date <-mdy(vg_met_data$date)

# read back in the proper data
redondo_met_data <-read.csv("/Users/jacktarricone/ch1_jemez/climate_station_data/redondo/redondo_met_data_v2.csv")
redondo_met_data$date_time <-ymd_hms(redondo_met_data$date_time)
redondo_met_data$date <-ymd(redondo_met_data$date)

# test plot pair 1
ggplot() +
  geom_line(data = redondo_met_data, aes(x = date_time, y = mean_air_temp_c), color = 'red')+
  geom_line(data = vg_met_data, aes(x = date_time, y = mean_air_temp_c))

# define flight times
vg_met_data$date_time

# uavsar flights
flight1 <-vg_met_data$date_time[273]
flight2 <-vg_met_data$date_time[441]
flight3 <-vg_met_data$date_time[609]

# landsat aquisistions
fsca1 <-vg_met_data$date_time[421]
fsca2 <-vg_met_data$date_time[805]

# plot air temp
theme_set(theme_classic(16))
ggplot() +
  geom_hline(yintercept = 0, linetype=3, col = "grey50", alpha = .7) +
  geom_vline(xintercept = flight1, linetype=1, col = "blue", alpha = .8) +
  geom_vline(xintercept = flight2, linetype=1, col = "blue", alpha = .8) +
  geom_vline(xintercept = flight3, linetype=1, col = "blue", alpha = .8) +
  geom_vline(xintercept = fsca1, linetype=1, col = "darkgreen", alpha = .8) +
  geom_vline(xintercept = fsca2, linetype=1, col = "darkgreen", alpha = .8) +
  geom_line(data = vg_met_data, aes(x = date_time, y = mean_air_temp_c), col = "black", size = .4) + 
  geom_line(data = redondo_met_data, aes(x = date_time, y = mean_air_temp_c), col = "red", size = .4) + 
  scale_x_datetime(breaks = "2 day", date_labels="%d", limits = ymd_hm(c("2020-02-12 01:00", "2020-03-06 23:00")))+
  scale_y_continuous(breaks = seq(-20,10,5), limit = c(-20,10))+
  xlab("Day of February") + ylab("Air Temperature (°C)") + 
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size = 1),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))

# save
setwd("/Users/jacktarricone/ch1_jemez/plots")
ggsave("temp_rough.pdf",
       width = 6, 
       height = 3,
       units = "in",
       dpi = 500)



# plot air temp
ggplot() +
  geom_hline(yintercept = 0, linetype=3, col = "grey50", alpha = .7) +
  geom_vline(xintercept = flight1, linetype=1, col = "blue", alpha = .8) +
  geom_vline(xintercept = flight2, linetype=1, col = "blue", alpha = .8) +
  geom_vline(xintercept = flight3, linetype=1, col = "blue", alpha = .8) +
  geom_vline(xintercept = fsca1, linetype=1, col = "darkgreen", alpha = .8) +
  geom_vline(xintercept = fsca2, linetype=1, col = "darkgreen", alpha = .8) +
  geom_line(data = vg_met_data, aes(x = date_time, y = avg_wind_ms), col = "black", size = .3) + 
  geom_line(data = redondo_met_data, aes(x = date_time, y = avg_wind_ms), col = "red", size = .3) + 
  scale_x_datetime(breaks = "2 day", date_labels="%d", limits = ymd_hm(c("2020-02-12 01:00", "2020-03-06 23:00")))+
  scale_y_continuous(breaks = seq(0,8,2), limit = c(0,8))+
  xlab("Day of February") + ylab("Wind Speed (m/s)") + 
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size = 1),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))

  
# save
ggsave("wind_rough.pdf",
       width = 6, 
       height = 3,
       units = "in",
       dpi = 500)
              