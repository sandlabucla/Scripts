#########################################################################################################################################################
# Need to Belong Scale (NtBS) scoring
# Created by Aayush Patel
#########################################################################################################################################################
#Install necessary packages
#install.packages("matrixStats")
library(matrixStats)


NtBS <- function(dataframe) {
#Reverse code NtBS for questions 1, 3, 7
  dataframe$NtBS_1R <- ifelse(dataframe$NtBS_1 == 1, dataframe$NtBS_1R <- 5,
                              ifelse(dataframe$NtBS_1 == 2, dataframe$NtBS_1R <- 4,
                                     ifelse(dataframe$NtBS_1 == 3, dataframe$NtBS_1R <- 3,
                                            ifelse(dataframe$NtBS_1 == 4, dataframe$NtBS_1R <- 2, 1))))
  dataframe$NtBS_3R <- ifelse(dataframe$NtBS_3 == 1, dataframe$NtBS_3R <- 5,
                              ifelse(dataframe$NtBS_3 == 2, dataframe$NtBS_3R <- 4,
                                     ifelse(dataframe$NtBS_3 == 3, dataframe$NtBS_3R <- 3,
                                            ifelse(dataframe$NtBS_3 == 4, dataframe$NtBS_3R <- 2,
                                                   ifelse(dataframe$NtBS_3 == 5, dataframe$NtBS_3R <- 1, 0)))))
  dataframe$NtBS_7R <- ifelse(dataframe$NtBS_7 == 1, dataframe$NtBS_7R <- 5,
                              ifelse(dataframe$NtBS_7 == 2, dataframe$NtBS_7R <- 4,
                                     ifelse(dataframe$NtBS_7 == 3, dataframe$NtBS_7R <- 3,
                                            ifelse(dataframe$NtBS_7 == 4, dataframe$NtBS_7R <- 2,
                                                   ifelse(dataframe$NtBS_7 == 5, dataframe$NtBS_7R <- 1, 0)))))
#Scores NtBS questionnaire using the sum of the correct variables
dataframe$NtBS_score_sum <- rowSums(dataframe[,paste("NtBS",c("1R", "2", "3R", "4", "5", "6", "7R", "8", "9", "10"),sep="_")], na.rm = FALSE)
dataframe$NtBS_score_mean <- rowMeans(dataframe[,paste("NtBS",c("1R", "2", "3R", "4", "5", "6", "7R", "8", "9", "10"),sep="_")], na.rm = FALSE)
  
  return(dataframe)
}



