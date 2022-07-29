# Run this script first when running depthsensor_qc_infill
# Loads packages, scripts, identifies variables, and imports data

# written by Dr. Keith Jennings
# adapted by Jack Tarricone
# July 28th, 2022


#################################################################################
#Load packages
#These must be already installed on your system 

library(ggplot2)  #plotting
library(dplyr)    #data manipulation
library(plyr)     #data manipulation
library(plotly)   #interactive plotting
library(cowplot)  #publication-ready plots
library(fda)      #fourier transforms

#Source these scripts
devtools::source_url("https://raw.githubusercontent.com/SnowHydrology/date_functions/main/doy_dowy.R")

#################################################################################
# Import depth sensor data 
# for each of the three stations: valle grande (vg), redondo, hidden valley (hv)

################################ 
######## read in vg met data
vg_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/vg/vg_met_data_v2.csv")
vg_met_data <-vg_met_data[,-1] # delete first col
vg_met_data$vg_snow_depth_cm <-as.numeric(vg_met_data$vg_snow_depth_cm) # convert to numeric
vg_met_data$date_time <-mdy_hm(vg_met_data$date_time) # format date time
vg_met_data$date <-mdy(vg_met_data$date) # format date
head(vg_met_data)

# filter down to same date range
vg_filt <-filter(vg_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

################################ 
######### read in redondo met data
redondo_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/redondo/redondo_met_data_v1.csv")
redondo_met_data$date_time <-ymd_hms(redondo_met_data$date_time) # format date time
redondo_met_data$date <-ymd(redondo_met_data$date) # format time

# filter down to same date range
redondo_filt <-filter(redondo_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

################################ 
######### read in hv met data
hv_met_data <-read.csv("/Users/jacktarricone/ch1_jemez_data/climate_station_data/hv/hv_met_data_v1.csv")
hv_met_data$date_time <-ymd_hms(hv_met_data$date_time) # format date time
hv_met_data$date <-ymd(hv_met_data$date) # format time

# filter down to same date range
hv_filt <-filter(hv_met_data, date_time >= "2020-02-10 18:50:00" & date_time <= "2020-02-26 18:50:00")

# bind snow depth and date_time cols to and make df
depth <-as.data.frame(cbind(as.character(vg_filt$date_time), # as character to move date time
                               vg_filt$vg_snow_depth_cm,
                               redondo_filt$redondo_snow_depth_cm, 
                               hv_filt$hv_snow_depth_cm))

# transform back to date
depth$V1 <-ymd_hms(depth_df$V1, tz = "MST")

# rename new cols
names(depth_df)[1] <- "date_time"
names(depth_df)[2] <- "DSDepth_1" # vg
names(depth_df)[3] <- "DSDepth_2" # redondo
names(depth_df)[4] <- "DSDepth_3" # hv

# check
head(depth)

#################################################################################
#Format data

# ddd doy, and dowy info
depth$doy <- doy_FUN(depth_df$date_time)
depth$dowy <- dowy_FUN(depth_df$date_time, depth_df$doy)

# unload plyr
detach("package:plyr", unload=TRUE)

#write.csv(depth_df, "/Users/jacktarricone/ch1_jemez_data/climate_station_data/wrcc_depth_formatted.csv")

#################################################################################
#Write function for identifying infill types
#Assigns the type of infilling to be performed based on gap length
#Omits any gaps occurring before veg_effect_end and after snow_end_dowy
w = gap_num
x = gap_length
y = TIMESTAMP
z = dowy
fun_FILLTYPE <- #Assigns the type of infilling to be performed based on gap length (x)
 function(w,x,y,z){ifelse(w == 1 | z >= snow_end_dowy[i],
                       "MANUAL",
                      ifelse(x > 48,
                            "SPLINE",
                           ifelse(x <= 48 & x > 3,
                                 "AVG24",
                                "INTERP")))}


