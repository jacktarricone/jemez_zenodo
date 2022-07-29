#Modifications for depth sensor infill script
#R spline interpolation introduces too many spikes into the data, so I am changing 
  #to just linear interpolation

#Code to process and output multiple version levels of snow depth data

#Keith Jennings
#ksjennin@gmail.com
#2017-10-20

#Run the RUNFIRST file
#NOTE: YOU MUST BE CONNECTED TO SNOWSERVER
source("/Users/jacktarricone/ch1_jemez_data/jemez_zenodo/jemez_depthsensor_qc_infill_RUNFIRST.R")

###############################################################################################
###############################################################################################
#***********************Change these values***********************
   #snow_start_dowy = when the snow season starts (should be same for all)
       #Note visual inspection shows snow starts on 2017-11-17 (DOWY = 48)
       #Corroborated with SNOTEL data from GM
   #veg_effect_end = when vegetation no longer affects depth value in Fall (varies from sensor to sensor)
   #snow_end_dowy = when snow is gone (varies from sensor to sensor)
   #bias = vertical error in the offset
   #max_thresh = value above which all values are bad (varies from sensor to sensor)
   #min_thresh = value below which all values are bad (varies from sensor to sensor)
       #Same as bias
  #fft_basis = value for fast fourier transform (lower = smoother, higher = spikier)
       #must be an odd number

n_sensors <- 3
badsensors <-  NA
snow_start_dowy <- rep(134, times = 3)
veg_effect_end <-  ymd_hms(c("2020-02-11 03:00:00",
                            "2020-02-11 03:00:00",
                            "2020-02-11 03:00:00"),
                             tz = "MST")
snow_end_dowy <-    rep(150, times = 3)
bias <-             rep(0, times = 3)
max_thresh <-       c(70,90,90)
min_thresh <-       c(50,57,75)
fft_basis <-        751

#Above values can be assessed visually using example plot code below
ggplotly(ggplot(depth_df, aes(date_time, hv_snow_depth_cm)) + 
          geom_point() +
          geom_hline(yintercept = 0, lty = "dashed", color = "gray"))

###############################################################################################
###############################################################################################
#QC, infill, and smooth the data

#Load zoo package for spline infilling
library(zoo)

#Create dummy dataset for depth data
depth_final <- data.frame()     #dummy data frame for final product

#Loop through sensors and run QC procedure
########START QC########
for (i in 1:n_sensors){
  if(i %in% badsensors) next    #skip to next sensor if sensor data not usable
  
  ###############################################################################################
  #1: Subset data
  tmp <- dplyr::select(depth_def, 
                      date_time, dowy, #grab date data
                      matches(paste0("DSDepth_", i, "$")))  #$ = exact match at end of string
  
  ###############################################################################################
  #2: Identify the Level 0 data
  #Raw, unfiltered
  colnames(tmp)[3] = "depth_lvl_0"
  
  ###############################################################################################
  #3: Run the first QC
  #a: Max and min thresholding
  #b: Remove dates outside veg effects
  tmp$depth_lvl_1a <- ifelse(tmp$depth_lvl_0 < min_thresh[i] | 
                               tmp$depth_lvl_0 > max_thresh[i],
                             NA,
                             tmp$depth_lvl_0)
  
  tmp$depth_lvl_1b <- ifelse(tmp$date_time < veg_effect_end[i] |
                               tmp$dowy >= snow_end_dowy[i],
                             NA,
                             tmp$depth_lvl_1a)
  
  ###############################################################################################
  #4: Run the second QC 
  #4a: Remove values outside ±2 standard deviations (± 5 cm per 15 min)
  #4b: Think about using more complex routine that identifies spikes
  #based on rate of change and length (as in Lehning et al., 2002)
  #this might be more accurate
  #5 cm seems to be about the max of real snowfall events
  del_thresh = 5
  tmp$del_depth <- c(0, diff(tmp$depth_lvl_1b))
  
  tmp$depth_lvl_2 <- ifelse(tmp$del_depth > del_thresh |
                              tmp$del_depth < (-1 * del_thresh) |
                              is.na(tmp$del_depth),
                            NA,
                            tmp$depth_lvl_1b)
  
  ###############################################################################################
  #5: Adjust the data for bias
  tmp$depth_lvl_3 <- tmp$depth_lvl_2 - bias[i]
  
  ###############################################################################################
  #6a: Infill the data using hierarchical protocol
  #First, gaps =< 3 h filled with linear interpolation
  #Infill remaining data using a spline
  
  tmp_gap <- #subset to all missing observations
    filter(tmp, is.na(depth_lvl_3))
  
  tmp_gap$time_diff <- #calculate time diff in hours
    c(0, diff.POSIXt(tmp_gap$date_time)) 
  
  #Check date_time diff units
  #That diff.posix units can't be controlled is a known error in R
  tmp_date_time <-
    head(diff.POSIXt(tmp_gap$date_time))
  if(attr(tmp_date_time, which = "units") == "secs") {
    #Divide time_diff by 60 if differences are in seconds not minutes
    tmp_gap$time_diff <- tmp_gap$time_diff / 60
  }
  
  #assign dummy gap number and initiate gap counter (for numbering gaps)
  tmp_gap$gap_num <- 0
  gap_counter = 1
  
  #Loop through the gaps and number them, increment each time gap > 15 min
  for (j in 1:length(tmp_gap$time_diff)){
    if (tmp_gap[j, "time_diff"] > 15) { 
      gap_counter = #increase gap counter by 1 if gap longer than 15 min
        gap_counter + 1
      tmp_gap[j, "gap_num"] = #assign new gap#
        gap_counter
    } else {
      tmp_gap[j, "gap_num"] = #assign gap number
        gap_counter
    }
  }
  
  tmp_gap <- #add gap length using pipes and dplyr (more efficient than with and merge)
    tmp_gap %>% 
    group_by(gap_num) %>% 
    mutate(gap_length = length(depth_lvl_3)/4) #gap_length is in hours
  
  #tmp_gap$fill_type <- #assign gap category
  # fun_FILLTYPE(tmp_gap$gap_num,
  #             tmp_gap$gap_length,
  #            tmp_gap$date_time,
  #           tmp_gap$dowy)
  
  tmp <- #Merge gap information with complete dataset
    left_join(tmp, tmp_gap[ , c("date_time", "gap_num", "gap_length")],
              by = "date_time")
  
  tmp <- #make sure df is ordered by date_time
    arrange(tmp, date_time)

  ######END QC PROTOCOL######## 
  

  ########START INFILL + SMOOTH########
  
  #Fill data after end of snow season
  tmp$depth_lvl_4a <- ifelse(tmp$dowy >= snow_end_dowy[i],
                             0,
                             tmp$depth_lvl_3)
  
  #Filter values for infilling to not include gap 1 (the Fall snow-free season)
  #These data will be filled manually later
  tmp_fill_values <- filter(dplyr::select(tmp, date_time, depth_lvl_4a, gap_num),
                            gap_num != 1 | is.na(gap_num))
  
  #Fill data using linear interpolation
  tmp_fill <- na.approx(zoo(tmp_fill_values$depth_lvl_4a, #first entry is data for zoo object
                            tmp_fill_values$date_time))  #second entry is time index for zoo object
  
  #Make tmp_fill a data frame 
  tmp_fill <- data.frame(date_time = as.POSIXct(index(tmp_fill), tz = "MST"),
                         depth_lvl_4b = coredata(tmp_fill))
  
  #Join with rest of data
  tmp <- left_join(tmp, tmp_fill, 
                   by = "date_time")
  
  #Fill data using spline interpolation
  tmp_fill <- na.spline(zoo(tmp_fill_values$depth_lvl_4a, #first entry is data for zoo object
                            tmp_fill_values$date_time),   #second entry is time index for zoo object
  )  
  
  #Make tmp_fill a data frame 
  tmp_fill <- data.frame(date_time = as.POSIXct(index(tmp_fill), tz = "MST"),
                         depth_lvl_4c = coredata(tmp_fill))
  
  #Join with rest of data
  tmp <- left_join(tmp, tmp_fill, 
                   by = "date_time")
  
  #Make final infill column
  #Value is based on whether linear interpolation or spline should be used
  fill_gap_length_max_linear = 96
  tmp$depth_lvl_4 <- ifelse(is.na(tmp$gap_length) == T | tmp$dowy >= snow_end_dowy[i],
                            tmp$depth_lvl_4a,
                            ifelse(tmp$gap_length <= fill_gap_length_max_linear,
                                   tmp$depth_lvl_4b,
                                   tmp$depth_lvl_4c))
  
  ###############################################################################################
  #7: Smooth the data using various filters
  #7a: fourier transform
  #7b: 3 h moving average
  #7c: 6 h moving average
  #7d: Savistky-Golay (preserves more spikes)
  #S-G coefficient table: https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter#Appendix
  
  #Subset to serially complete dataset
  #Above techniques cannot cope with gaps
  tmp_smooth <- filter(dplyr::select(tmp, date_time, depth_lvl_4), 
                       is.na(depth_lvl_4) == F)
  
  #7a: smooth with fft
  #value of 751 determined before hand to balance conserving real spikes w/smoothing data
  depth.basis = create.fourier.basis(rangeval = as.numeric(range(tmp_smooth$date_time)),
                                     nbasis = fft_basis)
  depth.fourier.fd = smooth.basis(argvals = as.numeric(tmp_smooth$date_time), 
                                  y = tmp_smooth$depth_lvl_4, fdParobj = depth.basis)$fd 
  tmp_smooth$depth_lvl_5a = as.numeric(eval.fd(tmp_smooth$date_time, depth.fourier.fd))
  
  #7b: 3 h moving average
  ma_3h = c(rep(1, 12))/12
  tmp_smooth$depth_lvl_5b <- stats::filter(tmp_smooth$depth_lvl_4, ma_3h)
  
  #7c: 6 h moving average
  ma_6h = c(rep(1, 24))/24
  tmp_smooth$depth_lvl_5c <- stats::filter(tmp_smooth$depth_lvl_4, ma_6h)
  
  #7d: S-G 7
  sg_7 = c(-2,3,6,7,6,3,-2)/21
  tmp_smooth$depth_lvl_5d <- stats::filter(tmp_smooth$depth_lvl_4, sg_7)
  
  #7e: S-G 9
  sg_9 = c(-21,14,39,54,59,54,39,14,-21)/231
  tmp_smooth$depth_lvl_5e <- stats::filter(tmp_smooth$depth_lvl_4, sg_9)
  
  #Join smoothed data with original dataset
  tmp <- left_join(tmp, dplyr::select(tmp_smooth, -depth_lvl_4), 
                   by = "date_time")
  
  #Clean up fourier values at end of snow season
  tmp$depth_lvl_5a <- ifelse(tmp$dowy >= snow_end_dowy[i],
                             0,
                             tmp$depth_lvl_5a)
  
  ###############################################################################################
  #8: Create final dataset
  tmp$sensor <- paste0(site_name, i)
  depth_final <- bind_rows(depth_final, tmp)
}



###############################################################################################
#9: Manual infilling
#This comprises three steps
#9a: Inspect plots and remove data that passed QC checks but failed visually
#9b: Fill data for good snow sensors between start of snow season and where sensor picks up accurate data
#9c: Blend the good sensor observations from 9b above to estimate the start of snow season depth for the bad sensors

#Plot example for visual inspection
#ggplotly(ggplot(filter(depth_final, sensor %in% c("SXN1","SXN10")), 
#               aes(date_time, depth_lvl_5a, color = sensor)) + 
#         geom_line(lwd = 0.5))

#Identify the bad data points from visual plot inspections
vis_qc_fail <- data.frame(sensors = c("SXK1",
                                      "SXK3","SXK3","SXK3",
                                      "SXK5","SXK5","SXK5"),
                          date_time_start = as.POSIXct(c("2016-12-10 04:15", 
                                                         "2016-12-18 19:00", "2017-05-10 12:30", "2017-05-15 11:45", 
                                                         "2017-04-11 21:15", "2017-05-11 16:45", "2017-05-23 04:00"), 
                                                       tz = "MST"),
                          date_time_end = as.POSIXct(c("2016-12-11 15:15", 
                                                       "2016-12-19 05:15", "2017-05-11 23:45", "2017-05-16 18:30", 
                                                       "2017-04-16 19:15", "2017-05-17 05:00", "2017-05-27 12:00"), 
                                                     tz = "MST"))

#Write function for marking whether the data failed the visual check
vis_fail_FUN <- function(x,y,z){
  ifelse(x == vis_qc_fail[i, "sensors"] &           #x = sensor
           y >= vis_qc_fail[i, "date_time_start"] & #y = date_time
           y <= vis_qc_fail[i, "date_time_end"],    #y = date_time
         "6a",                                         #6a = infilled values failed vis inspection
         z)                                         #z = original vis fail value
}

#Assign flag 6 NA to begin
depth_final$flag_lvl_6 <- NA

#Loop through failed observations and replace the vis fail value
for(i in 1:length(vis_qc_fail$sensors)){
  depth_final$flag_lvl_6 <- vis_fail_FUN(depth_final$sensor, depth_final$date_time, depth_final$flag_lvl_6)
}

#Assign NAs to all values failing the visual checks for the 5 smooth types
smooth_types = c("a", "b", "c", "d", "e")
for(i in 1:length(smooth_types)){
  depth_final[, paste0("depth_lvl_6", smooth_types[i])] <-
    depth_final[ , paste0("depth_lvl_5", smooth_types[i])]
}

#Set all failing observations to NA
#Note: May need to change row numbers
depth_final[22:26] <- 
  lapply(depth_final[22:26], function(x) ifelse(is.na(depth_final$flag_lvl_6), x, NA))

#Use dplyr to approximate all NA values
depth_final <- depth_final %>% 
  group_by(sensor) %>% 
  mutate(depth_lvl_6a = na.approx(depth_lvl_6a, na.rm = F),
         depth_lvl_6b = na.approx(depth_lvl_6b, na.rm = F),
         depth_lvl_6c = na.approx(depth_lvl_6c, na.rm = F),
         depth_lvl_6d = na.approx(depth_lvl_6d, na.rm = F),
         depth_lvl_6e = na.approx(depth_lvl_6e, na.rm = F))

#Unload zoo
#It masks some functionality from base R
detach("package:zoo", unload = T)

#Next identify the best sites for filling in before end of veg effects
#Back fill them to start of snow season
#Average their values and fill remaining sensors
#Snow season starts at 2016-11-17 10:30 (average of when 2 SNOTEL sites reported first snowfall)
snow_start_date_time = as.POSIXct("2016-11-17 10:30", tz = "MST")
sensors_best = c("SXK1", "SXK2", "SXK3", "SXK6", "SXK7", "SXK8", "SXK9")
depth_final[22:26] <- 
  lapply(depth_final[22:26], function(x) ifelse(depth_final$date_time <= snow_start_date_time, 0, x))
depth_final[22:26] <- 
  lapply(depth_final[22:26], function(x) ifelse(depth_final$sensor %in% sensors_best, 
                                                na.approx(x, na.rm =T), x))

#Average the best sensors from start to 2016-11-24 12:00
depth_start_blend <- filter(depth_final, sensor %in% sensors_best & 
                              date_time <= as.POSIXct("2016-11-24 12:00", tz = "MST")) %>% 
  group_by(date_time) %>% 
  summarise(depth_lvl_6a_blend = mean(depth_lvl_6a),
            depth_lvl_6b_blend = mean(depth_lvl_6b),
            depth_lvl_6c_blend = mean(depth_lvl_6c),
            depth_lvl_6d_blend = mean(depth_lvl_6d),
            depth_lvl_6e_blend = mean(depth_lvl_6e))

#Join the blended sensor values to the main data frame
depth_final <- left_join(depth_final, depth_start_blend, by = "date_time")

#Calculate the scalars for each bad sensor and smooth type
sensors_worst = c("SXK4", "SXK5", "SXK10")

#Calculate when the "bad" sensors come online
depth_start_join_times <- filter(depth_final, sensor %in% sensors_worst &
                                   date_time > snow_start_date_time) %>% 
  group_by(sensor) %>% 
  summarise(start_depth_lvl_6a = snow_start_date_time + (min(which(!is.na(depth_lvl_6a)) * 900)),
            start_depth_lvl_6b = snow_start_date_time + (min(which(!is.na(depth_lvl_6b)) * 900)),
            start_depth_lvl_6c = snow_start_date_time + (min(which(!is.na(depth_lvl_6c)) * 900)),
            start_depth_lvl_6d = snow_start_date_time + (min(which(!is.na(depth_lvl_6d)) * 900)),
            start_depth_lvl_6e = snow_start_date_time + (min(which(!is.na(depth_lvl_6e)) * 900)))

#Loop through the sensors and smooth types to get the values for calculating the scalars
depth_start_scalar <- data.frame()
for(i in 1:length(depth_start_join_times$sensor)){
  depth_start_scalar[i, "sensor"] = depth_start_join_times[i, "sensor"]
  for(j in 1:length(smooth_types)){
    tmp.time <- as.POSIXct(unlist(depth_start_join_times[i, (j + 1)]), origin = "1970-01-01 00:00")
    tmp <- depth_final[c("sensor", "date_time",
                         paste0("depth_lvl_6", smooth_types[j]),
                         paste0("depth_lvl_6", smooth_types[j], "_blend"))]
    tmp <- filter(tmp, date_time == tmp.time & sensor == unlist(depth_start_join_times[i, "sensor"]))
    depth_start_scalar[i, paste0("depth_lvl_6", smooth_types[j], "_scalar")] = 
      tmp[1,3]/tmp[1,4]
  }
}

#Join to data set
depth_final <- left_join(depth_final, depth_start_scalar,
                         by = "sensor")

#Compute values for bad sensors
#Below code is ugly, but it works
depth_final$depth_lvl_6a <- ifelse(is.na(depth_final$depth_lvl_6a),
                                   depth_final$depth_lvl_6a_blend * depth_final$depth_lvl_6a_scalar,
                                   depth_final$depth_lvl_6a)
depth_final$depth_lvl_6b <- ifelse(is.na(depth_final$depth_lvl_6b),
                                   depth_final$depth_lvl_6b_blend * depth_final$depth_lvl_6b_scalar,
                                   depth_final$depth_lvl_6b)
depth_final$depth_lvl_6c <- ifelse(is.na(depth_final$depth_lvl_6c),
                                   depth_final$depth_lvl_6c_blend * depth_final$depth_lvl_6c_scalar,
                                   depth_final$depth_lvl_6c)
depth_final$depth_lvl_6d <- ifelse(is.na(depth_final$depth_lvl_6d),
                                   depth_final$depth_lvl_6d_blend * depth_final$depth_lvl_6d_scalar,
                                   depth_final$depth_lvl_6d)
depth_final$depth_lvl_6e <- ifelse(is.na(depth_final$depth_lvl_6e),
                                   depth_final$depth_lvl_6e_blend * depth_final$depth_lvl_6e_scalar,
                                   depth_final$depth_lvl_6e)



