# plotting all the snow depth sensors in the CZO jemez study area
# data provided by noah molotch and processed by Leanne Lestak

# july 28th, 2022

# 5 of noah's on reondo
# 3 WRCC: valle grande, hidden valley, reondo

library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2);theme_set(theme_classic(12))

# read in data from noah
dat <-read_xlsx("/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/wy2020/Data_File/nms_wy2020_level0-1.xlsx")

###### format data for plotting
## reads in with double column/row header that needs to be fixed

# Level 0 = raw data
# Level 1 = QA/QC data

# remove first row
dat <-dat[-1,]

# column names string
headers <-c("date_time", 
            "DSTempC_1", "DSDepth_1_raw", "DSDepth_1",
            "DSTempC_2", "DSDepth_2_raw", "DSDepth_2",
            "DSTempC_3", "DSDepth_3_raw", "DSDepth_3",
            "DSTempC_4", "DSDepth_4_raw", "DSDepth_4",
            "DSTempC_5", "DSDepth_5_raw", "DSDepth_5",
            "DSTempC_6", "DSDepth_6_raw", "DSDepth_6",
            "DSTempC_7", "DSDepth_7_raw", "DSDepth_7",
            "DSTempC_8", "DSDepth_8_raw", "DSDepth_8",
            "DSTempC_9", "DSDepth_9_raw", "DSDepth_9")

# change names
names(dat) <-headers

# change to numeric
dat[, 2:28] <- sapply(dat[, 2:28], as.numeric)

# quick test plot
ggplot(dat)+
  geom_point(aes(x = date_time, y = DSDepth_1))+
  lims(y = c(0, 100))

# filter and plot for 2/12-2/26
filt <-filter(dat, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

######## read in HQ met data
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v2.csv")
vg_met_data$vg_snow_depth_cm <-as.numeric(vg_met_data$vg_snow_depth_cm)
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time)
vg_met_data$date <-mdy(vg_met_data$date)

# filter down to same date range
vg_filt <-filter(vg_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

######### read in redondo met data
redondo_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/redondo_met_data_v1.csv")
redondo_met_data$date_time <-ymd_hms(redondo_met_data$date_time)
redondo_met_data$date <-ymd(redondo_met_data$date)

# filter down to same date range
redondo_filt <-filter(redondo_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

######### read in hv met data
hv_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/hv/hv_met_data_v1.csv")
hv_met_data$date_time <-ymd_hms(hv_met_data$date_time)
hv_met_data$date <-ymd(hv_met_data$date)

# filter down to same date range
hv_filt <-filter(hv_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")
head(hv_filt)

# bind snow depth and date_time cols to df
insar <-cbind(filt,
              vg_filt$vg_snow_depth_cm, 
              redondo_filt$redondo_snow_depth_cm, 
              hv_filt$hv_snow_depth_cm)

# rename new cols
names(insar)[29] <- "vg_snow_depth_cm"
names(insar)[30] <- "redondo_snow_depth_cm"
names(insar)[31] <- "hv_snow_depth_cm"

# test_plot
ggplot(insar) +
  geom_point(aes(x = date_time, y = DSDepth_4))

# plot
ggplot(insar)+
  geom_line(aes(x = date_time, y = DSDepth_1, col = "1"), size = .5)+
  #geom_point(aes(x = date_time, y = DSDepth_2, col = "2"), size = .1)+
  geom_line(aes(x = date_time, y = DSDepth_3, col = "3"), size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_4, col = "4"), size = .5)+
  #geom_point(aes(x = date_time, y = DSDepth_5, col = "5"), size = .1)+
  geom_line(aes(x = date_time, y = DSDepth_6, col = "6"), size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_7, col = "7"), size = .5)+
  #geom_line(aes(x = date_time, y = DSDepth_8, col = "8"), size = .1)+
  geom_line(aes(x = date_time, y = DSDepth_9, col = "9"), size = .5)+
  geom_point(aes(x = date_time, y = vg_snow_depth_cm, col = "10"), size = .1)+
  geom_point(aes(x = date_time, y = redondo_snow_depth_cm, col = "11"), size = .1)+
  geom_point(aes(x = date_time, y = hv_snow_depth_cm, col = "12"), size = .1)+
  scale_y_continuous(limits = c(50,120),breaks = c(seq(50,120,10)))+
  ylab("Depth (cm)") + xlab("Date") +
  scale_color_manual(name = "Sensor",
                     values = c('1' = 'darkgreen', '3' = 'plum', 
                                '4' = 'goldenrod', '6' = 'firebrick', 
                                '7' = 'darkorange','9'='aquamarine',
                                '10' = 'red','11'='black','12'='darkblue'),
                     labels = c('1' = 'DSDepth_1', '3' = 'DSDepth_3', 
                                '4' = 'DSDepth_4', '6' = 'DSDepth_6', 
                                '7' = 'DSDepth_7', '9' = 'DSDepth_9',
                                '10' = 'VG','11'='Redondo','12'='HV'))+
  scale_x_datetime(breaks = "2 day", 
                   date_labels="%b %d", 
                   limits = ymd_hms(c("2020-02-11 00:03:00", "2020-02-27 22:59:59")))


setwd("/Users/jacktarricone/ch1_jemez_data/plots")
ggsave(file = "czo_jemez_depth.png",
       width = 7,
       height = 4,
       dpi = 400)

### test with other data




