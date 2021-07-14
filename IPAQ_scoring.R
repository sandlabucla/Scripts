#########################################################################################################################################################
# International Physical Activity Questionnaire (IPAQ) scoring
# Created by Aayush Patel
#########################################################################################################################################################
#this package must be installed
library(matrixStats)

IPAQ <- function(dataframe) {
#calculates the score for vigorous activity for each individual by multiplying days per week, minutes per day, and MET constant of 8; omitted if all of the data is not given
dataframe$IPAQ_score_vigorous <- ifelse(dataframe$IPAQ_1==0, dataframe$IPAQ_score_vigorous <- 0, dataframe$IPAQ_score_vigorous <- rowProds(as.matrix(cbind(dataframe[,paste("IPAQ", c(1,2), "1_TEXT", sep = "_")],8)),na.rm =F))
#calculates the score for moderate activity for each individual by multiplying days per week, minutes per day, and MET constant of 4; omitted if all of the data is not given
dataframe$IPAQ_score_moderate <- ifelse(dataframe$IPAQ_3==0, dataframe$IPAQ_score_moderate <- 0, dataframe$IPAQ_score_moderate <- rowProds(as.matrix(cbind(dataframe[,paste("IPAQ", c(3,4), "1_TEXT", sep = "_")],4)),na.rm =F))
#calculates the score for walking activity for each individual by multiplying days per week, minutes per day, and MET constant of 3.3; omitted if all of the data is not given
dataframe$IPAQ_score_walking <- ifelse(dataframe$IPAQ_5==0, dataframe$IPAQ_score_walking  <- 0, dataframe$IPAQ_score_walking <- rowProds(as.matrix(cbind(dataframe[,paste("IPAQ", c(5,6), "1_TEXT", sep = "_")],3.3)), na.rm =F))
#calculates the final score by summing the vigorous, moderate, and walking activity scores together.
dataframe$final_score <- rowSums(dataframe[,paste("IPAQ","score",c("vigorous","moderate","walking"),sep='_')],na.rm = FALSE)


#scores the categorical score of IPAQ questionnaire; takes into consideration the conditions for the different categortical scores
dataframe$categorical_score <- ifelse(dataframe$IPAQ_1_1_TEXT>= 3 & dataframe$final_score>=1500, dataframe$categorical_score <- "High",
                                      ifelse(dataframe$IPAQ_1_1_TEXT + dataframe$IPAQ_3_1_TEXT + dataframe$IPAQ_5_1_TEXT >= 7 & dataframe$final_score>= 3000, dataframe$categorical_score <- "High",
                                             ifelse(dataframe$IPAQ1_1_TEXT>=3 & dataframe$IPAQ_2_1_TEXT>=20, dataframe$categorical_score <- "Moderate",
                                                    ifelse(dataframe$IPAQ_3_1_TEXT + dataframe$IPAQ_5_1_TEXT >= 5 & dataframe$IPAQ_4_1_TEXT + dataframe$IPAQ_6_1_TEXT>= 30, dataframe$categorical_score <- "Moderate",
                                                           ifelse(dataframe$IPAQ1_1_TEXT + dataframe$IPAQ_3_1_TEXT + dataframe$IPAQ_5_1_TEXT >= 5 & dataframe$final_score >=600, dataframe$categorical_score <- "Moderate", "Low")))))
return(dataframe)
 }

