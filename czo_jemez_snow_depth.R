# plotting all the snow depth sensors in the CZO jemez study area
# data provided by noah molotch and processed by Leanne Lestak

# july 28th, 2022

# 6 of noah's on reondo
# 2 WRCC: valle grande, hidden valley

library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2);theme_set(theme_classic(12))

# read in data from noah
dat <-read_xlsx("/Users/jacktarricone/ch1_jemez_data/climate_station_data/noah/wy2020/Data_File/nms_wy2020_level0-1.xlsx")
dat$date_time <-ymd_hms(dat$date_time, tz = "MST") #format to mst

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
  geom_line(aes(x = date_time, y = DSDepth_6))+
  lims(y = c(0, 100))

# filter and plot for 2/12-2/26
filt <-filter(dat, date_time >= "2020-02-12 00:00:00" & date_time <= "2020-02-26 18:50:00")

######## read in HQ met data
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_snow_depth_qaqc_v2.csv")
vg_met_data$vg_snow_depth_cm <-as.numeric(vg_met_data$vg_snow_depth_cm)
vg_met_data$date_time <-ymd_hms(vg_met_data$date_time, tz = "MST")
vg_met_data$date <-ymd(vg_met_data$date)

# filter down to same date range
vg_filt <-filter(vg_met_data, date_time >= "2020-02-12 00:00:00" & date_time <= "2020-02-26 18:50:00")

######### read in redondo met data
redondo_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/redondo_snow_depth_qaqc_v2.csv")
redondo_met_data$date_time <-ymd_hms(redondo_met_data$date_time, tz = "MST")
redondo_met_data$date <-ymd(redondo_met_data$date)

# filter down to same date range
redondo_filt <-filter(redondo_met_data, date_time >= "2020-02-12 00:00:00" & date_time <= "2020-02-26 18:50:00")

######### read in hv met data
hv_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/hv/hv_snow_depth_qaqc_v2.csv")
hv_met_data$date_time <-ymd_hms(hv_met_data$date_time)
hv_met_data$date <-ymd(hv_met_data$date, tz = "MST")

# filter down to same date range
hv_filt <-filter(hv_met_data, date_time >= "2020-02-12 00:00:00" & date_time <= "2020-02-26 18:50:00")

# bind snow depth and date_time cols to df
insar <-cbind(filt,
              vg_filt$sd_interp_v3_smooth20, 
              redondo_filt$sd_interp_v3_smooth20, 
              hv_filt$sd_interp_v3_smooth20)

# rename new cols
names(insar)[29] <- "vg_snow_depth_cm"
names(insar)[30] <- "redondo_snow_depth_cm"
names(insar)[31] <- "hv_snow_depth_cm"

# test_plot
ggplot(insar) +
  geom_point(aes(x = date_time, y = redondo_snow_depth_cm))

# define flight dates and times
flight1 <-as.numeric(insar$date_time[9]) # row for correct date time
flight2 <-as.numeric(insar$date_time[178])
flight3 <-as.numeric(insar$date_time[345])

# fine storm start and end
storm_start <-insar$date_time[240]
storm_end <-insar$date_time[270]

# plot
ggplot(insar)+
  geom_vline(xintercept = flight1, linetype=3, col = "red", alpha = .7) +
  geom_vline(xintercept = flight2, linetype=3, col = "red", alpha = .7) +
  geom_vline(xintercept = flight3, linetype=3, col = "red", alpha = .7) +
  annotate("rect", xmin = storm_start, xmax = storm_end,
    ymin = -Inf, ymax = Inf, alpha = .2)+
  geom_line(aes(x = date_time, y = DSDepth_1, col = "1"), size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_3, col = "3"), size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_4, col = "4"), size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_6, col = "6"), size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_7, col = "7"), size = .5)+
  geom_line(aes(x = date_time, y = DSDepth_9, col = "9"), size = .5)+
  geom_line(aes(x = date_time, y = vg_snow_depth_cm, col = "10"), size = .5)+
  geom_line(aes(x = date_time, y = hv_snow_depth_cm, col = "12"), size = .5)+
  scale_y_continuous(limits = c(50,120),breaks = c(seq(50,120,10)))+
  ylab("Depth (cm)") + xlab("Date") +
  scale_color_manual(name = "Sensor",
                     values = c('1' = 'darkgreen', '3' = 'plum', 
                                '4' = 'goldenrod', '6' = 'firebrick', 
                                '7' = 'darkorange','9'='aquamarine',
                                '10' = 'red','12'='darkblue'),
                     labels = c('1' = 'DSDepth_1', '3' = 'DSDepth_3', 
                                '4' = 'DSDepth_4', '6' = 'DSDepth_6', 
                                '7' = 'DSDepth_7', '9' = 'DSDepth_9',
                                '10' = 'VG','12'='HV'))+
  scale_x_datetime(breaks = "2 day", 
                   date_labels="%b %d", 
                   limits = ymd_hms(c("2020-02-12 01:00:00", "2020-02-26 19:00:00"), tz = "MST"))


setwd("/Users/jacktarricone/ch1_jemez_data/plots")
ggsave(file = "czo_jemez_depth_v3.png",
       width = 7,
       height = 3,
       dpi = 400)

#######################################################
###### changes in depth during insar periods ##########
#######################################################

### pair 1
# define flight dates and times
# [9] flight 1
# [178] flight 2
# [345] flight 3

cols_string <-c("DSDepth_1", "DSDepth_3", "DSDepth_4", "DSDepth_6", "DSDepth_7",
                "DSDepth_9", "vg_snow_depth_cm", "hv_snow_depth_cm")

##### function for pair 1 depth change
dswe_pair1 <-function(x){
  change <-x[178]-x[9]
  return(change)
}

pair1 <- apply(insar[,cols_string], 2, dswe_pair1)  # Apply function to specific columns
pair1

##### pair 2
dswe_pair2 <-function(x){
  change <-x[345]- x[178]
  return(change)
}

pair2 <- apply(insar[,cols_string], 2, dswe_pair2)  # Apply function to specific columns
pair2                                      # Print matrix containing updated values

##### pair 2
dswe_pair3 <-function(x){
  change <-x[345]- x[9]
  return(change)
}

pair3 <- apply(insar[,cols_string], 2, dswe_pair3)  # Apply function to specific columns
pair3 

# define row names
dates <-c("feb12_19","feb19_26","feb12_26")
results <-as.data.frame(rbind(pair1,pair2,pair3))
results_v2 <-cbind(dates,results)
print(results_v2)
