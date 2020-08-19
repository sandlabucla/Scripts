########################################################################################## 
#The Pittsburgh Sleep Quality Index (PSQI) Scoring 
#Created by Yael Waizman
########################################################################################## 

# NOTE: 666 scores = error, i.e. any result other than the specified coding scheme for each 
# variable; check raw data to determine cause of error; Note that PSQI_1 and 
# PSQI_3, which are the bed and wake times, must be checked thoroughly to ensure participant 
# input was correct, e.g. participant did not select am when they meant pm, which will 
# invalidate data. This is also done to ensure code correctly calculated hours slept.

PSQI <- function(dataframe){
  # Recoding was necessary to convert from default Qualtrics coding scheme for drill-down question imported from 
  # SANDLab/General_documents/Qualtrics/TimeQuestion_RecodedValues.csv (i.e., if hour is 1 coded value = 1 o'clock,  
  # 38 coded value = 2 o'clock, 75 coded value = 3 o'clock, 112 coded value = 4 o'clock, 149 coded value = 5 o'clock,  
  # 186 coded value = 6 o'clock, 223 coded value = 7 o'clock, 260 coded value = 8 o'clock, 297 coded value = 9 o'clock,
  # 334 coded value = 10 o'clock, 371 coded value = 11 o'clock, 408 coded value = 12 o'clock, otherwise give it 666)
  convert_Qualtrics_hour_to_normal_hour <- function(hour){
    hour <- ifelse(hour == 1, 1,
               ifelse(hour == 38, 2, 
                      ifelse(hour == 75, 3,
                             ifelse(hour == 112, 4,
                                    ifelse(hour == 149, 5, 
                                           ifelse(hour == 186, 6, 
                                                  ifelse(hour == 223, 7, 
                                                         ifelse(hour == 260, 8, 
                                                                ifelse(hour == 297, 9, 
                                                                       ifelse(hour == 334, 10,
                                                                              ifelse(hour == 371, 11,
                                                                                     ifelse(hour == 408, 12, 666))))))))))))
  return(hour)
  }
  # Recoding was necessary to convert from default Qualtrics coding scheme for drill-down question imported from 
  # SANDLab/General_documents/Qualtrics/TimeQuestion_RecodedValues.csv (i.e., if min is 2 coded value = :00,  
  # 5 coded value = :05, 8 coded value = :10, 11 coded value = :15, 14 coded value = :20,  
  # 17 coded value = :25, 20 coded value = :30, 23 coded value = :35, 26 coded value = :40,
  # 29 coded value = :45, 32 coded value = :50, 35 coded value = :55, otherwise give it 666)
  convert_Qualtrics_min_to_normal_min <- function(min){
    min <- ifelse(min == 2, ":00",
             ifelse(min == 5, ":05", 
                    ifelse(min == 8, ":10",
                           ifelse(min == 11, ":15",
                                  ifelse(min == 14, ":20", 
                                         ifelse(min == 17, ":25", 
                                                ifelse(min == 20, ":30", 
                                                       ifelse(min == 23, ":35", 
                                                              ifelse(min == 26, ":40", 
                                                                     ifelse(min == 29, ":45",
                                                                            ifelse(min == 32, ":50",
                                                                                   ifelse(min == 35, ":55", 666))))))))))))
  return(min)
  }
  # recode am to 3 and pm to 4, otherwise 666
  convert_Qualtrics_AmPm_to_normal_AmPm <- function(AmPm){
    AmPm <- ifelse(AmPm == 3, "am",
              ifelse(AmPm == 4, "pm", 666))
  return(AmPm)
  }
  
  #### Component 1: Subjective sleep quality
  dataframe$PSQI_comp1 <- dataframe$PSQI_6 # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  
  #### Component 2: Sleep latency
  # first, recode PSQI_2, if PSQI_2new values =< 15 give 0, 16-30 give 1, 31-60 give 2, and greater than 60 give 3, otherwise give 666
  dataframe$PSQI_2new <- ifelse(dataframe$PSQI_2 <= 15, 0,
                                  ifelse(dataframe$PSQI_2 > 15 & dataframe$PSQI_2 <= 30, 1,
                                         ifelse(dataframe$PSQI_2 > 30 & dataframe$PSQI_2 <= 60, 2, 
                                                ifelse(dataframe$PSQI_2 > 60, 3, 666))))
  # then, add PSQI_2 and PSQI_5a to calculate sleep latency 
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  dataframe$sumPSQI_2new_5_A <- rowSums(dataframe[,paste("PSQI_",c("2new","5_A"),sep="")], na.rm = FALSE)
  # then assign component 2 score as follows: if sumPSQI_2new_5_A values = 0 give 0, 1-2 give 1, 3-4 give 2, if 5-6 give 3
  dataframe$PSQI_comp2 <- ifelse(dataframe$sumPSQI_2new_5_A == 0, 0, 
                                       ifelse(dataframe$sumPSQI_2new_5_A >= 1 & dataframe$sumPSQI_2new_5_A <= 2, 1, 
                                              ifelse(dataframe$sumPSQI_2new_5_A > 2 & dataframe$sumPSQI_2new_5_A <= 4, 2, 
                                                     ifelse(dataframe$sumPSQI_2new_5_A > 4 & dataframe$sumPSQI_2new_5_A <= 6, 3, 666))))
  
  #### Component 3: Sleep duration 
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  # If someone put 6 I would say their sleep gets a score of 2 but anything greater than 6 gets a 1
  # if PSQI_4 values < 5 give 3, if 5 - 6 give 2, if > 6 to 7 give 1, if > 7 give 0, otherwise give 666 
  dataframe$PSQI_comp3 <- ifelse(dataframe$PSQI_4 < 5, 3,
                                     ifelse(dataframe$PSQI_4 >= 5 & dataframe$PSQI_4 <= 6, 2,
                                            ifelse(dataframe$PSQI_4 > 6 & dataframe$PSQI_4 <= 7, 1,
                                                   ifelse(dataframe$PSQI_4 > 7, 0, 666))))
  
  
  #### Component 4: Habitual sleep efficiency 
  # Uses function to convert Qualtrics coded hour data to normal hour data
  dataframe$PSQI_1_Hour <- convert_Qualtrics_hour_to_normal_hour(dataframe$PSQI_1_1) 
  # Uses function to convert Qualtrics coded min data to normal min data
  dataframe$PSQI_1_min <- convert_Qualtrics_min_to_normal_min(dataframe$PSQI_1_2)
  # Uses function to convert Qualtrics coded min data to normal min data
  dataframe$PSQI_1_AmPm <- convert_Qualtrics_AmPm_to_normal_AmPm(dataframe$PSQI_1_3)
  # Convert Hour, Min, and AmPm variables for PSQI_1 into a format R can read
  dataframe$PSQI_1 <- paste(dataframe$PSQI_1_Hour,dataframe$PSQI_1_min," ", dataframe$PSQI_1_AmPm, sep="")
  dataframe$PSQI_1 <- as.double(strptime(dataframe$PSQI_1, "%I:%M %p"))
  dataframe$PSQI_1 <- as.POSIXct(dataframe$PSQI_1, origin = "1970-01-01")
  # Uses function to convert Qualtrics coded hour data to normal hour data
  dataframe$PSQI_3_Hour <- convert_Qualtrics_hour_to_normal_hour(dataframe$PSQI_3_1) 
  # Uses function to convert Qualtrics coded min data to normal min data
  dataframe$PSQI_3_min <- convert_Qualtrics_min_to_normal_min(dataframe$PSQI_3_2)
  # Uses function to convert Qualtrics coded min data to normal min data
  dataframe$PSQI_3_AmPm <- convert_Qualtrics_AmPm_to_normal_AmPm(dataframe$PSQI_3_3)
  # Convert Hour, Min, and AmPm variables for PSQI_3 into a format R can read
  dataframe$PSQI_3 <- paste(dataframe$PSQI_3_Hour,dataframe$PSQI_3_min," ", dataframe$PSQI_3_AmPm, sep="")
  dataframe$PSQI_3 <- as.double(strptime(dataframe$PSQI_3, "%I:%M %p"))
  dataframe$PSQI_3 <- as.POSIXct(dataframe$PSQI_3, origin = "1970-01-01")
  # Calculate difference in seconds and hours between PSQI_1 (usual bed time) and PSQI_3 (usual getting up time).
  PSQI_Diffsec <- ifelse(as.numeric(substring(dataframe$PSQI_1,12,13))<12,as.double(abs(difftime(dataframe$PSQI_3, dataframe$PSQI_1, units = "secs"))), as.double(abs(difftime(dataframe$PSQI_3, dataframe$PSQI_1-86400, units = "secs"))))
  dataframe$PSQI_Diffhour <- abs(PSQI_Diffsec / 3600)
  # Instead of using the participants self-reported number of hours slept, calculate it by subtracting the number of minutes it takes them to fall asleep, but the calculated number of hours they spend in bed
  dataframe$PSQI_4 <- dataframe$PSQI_Diffhour - dataframe$PSQI_2/60 
  # Calculate habiiual sleep efficiency as foflows: (Calculated Number of hours slept/Number of hours spent in bed) X 100 = Habitual sleep efficiency (%) 
  dataframe$PSQI_tmphse <- (dataframe$PSQI_4 / dataframe$PSQI_Diffhour) * 100 # note the scoring guide says to use the self-report number of hours slept, but since many participants reported that incorrectly, we are calculating this here instead
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  # if PSQI_tmphse is >=85 give 0, if 75-84 give 1, if 65-74 give 2, if < 65 give 3
  dataframe$PSQI_comp4 <- ifelse(dataframe$PSQI_tmphse >= 85, 0, 
                             ifelse(dataframe$PSQI_tmphse >= 75 & dataframe$PSQI_tmphse < 85, 1,
                                    ifelse(dataframe$PSQI_tmphse >= 65 & dataframe$PSQI_tmphse < 75, 2, 
                                           ifelse(dataframe$PSQI_tmphse < 65, 3, 666))))
  
  #### Component 5: Sleep disturbances 
  # Add the scores for questions # 5b-5j: 
  dataframe$PSQI_5 <- rowSums(dataframe[,paste("PSQI_",c("5_B", "5_C", "5_D", "5_E", "5_F", "5_G", "5_H", "5_I", "5J_HOWOFTEN"),"",sep="")], na.rm = TRUE)
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  # if PSQI_5 adds up to a score 0 give 0, if 1-9 give 1, if 10-18 give 2, if 19-27 give 3
  dataframe$PSQI_comp5 <- ifelse(dataframe$PSQI_5 == 0, 0, 
                             ifelse(dataframe$PSQI_5 >= 1 & dataframe$PSQI_5 <= 9, 1,
                                    ifelse(dataframe$PSQI_5 > 9 & dataframe$PSQI_5 <= 18, 2, 
                                           ifelse(dataframe$PSQI_5 > 18 & dataframe$PSQI_5 <= 27, 3, 666))))
  
  #### Component 6: Use of sleeping medication 
  dataframe$PSQI_comp6 <- dataframe$PSQI_6
  
  #### Component 7: Daytime dystunction
  # Add the scores for question # 8 and ft 9:
  DAYDYS_sum <- rowSums(dataframe[,paste("PSQI_",c(8,9),"",sep="")], na.rm = FALSE)
  # Minimum Score = 0 (better); Maximum Score = 3 (worse)
  # if DAYDYS_sum score is 0 give 0, if 1-2 give 1, if 3-4 give 2, if 5-6 give 3
  dataframe$PSQI_comp7 <- ifelse(DAYDYS_sum == 0, 0, 
                             ifelse(DAYDYS_sum >= 1 & DAYDYS_sum <= 2, 1, 
                                    ifelse(DAYDYS_sum > 2 & DAYDYS_sum <= 4, 2, 
                                           ifelse(DAYDYS_sum > 4 & DAYDYS_sum <= 6, 3, 666))))
  
  ##### Global PSGI Score 
  # Add the seven component scores together: 
  dataframe$PSQI_global <- rowSums(dataframe[,paste("PSQI_comp",c(1:7),sep="")], na.rm = FALSE)
  return(dataframe)
}
