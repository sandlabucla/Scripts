#########################################################################################################################################################
# Issues Checklist - Parent (ICp) Version scoring
# Created by Aayush Patel
#########################################################################################################################################################
#Install necessary packages
library(matrixStats)


ICp <- function(dataframe){
  #Counts the number of issues marked "yes"
  dataframe$ICp_Issues_Sum <- rowSums(dataframe[,paste("ICp",c(1:21,"Other"),sep="_")], na.rm = FALSE)
  #Calculates the mean of issues marked "yes"
  dataframe$ICp_Issues_Mean <- rowMeans(dataframe[,paste("ICp",c(1:21,"Other"),sep="_")], na.rm = TRUE)
  #For issues marked “yes,” add intensity ratings and divide by the number of issues marked “yes” to obtain mean intensity rating.
  dataframe$ICp_Mean_Intensity <- rowMeans(dataframe[,paste("ICp",c(1:21,"Other"), "Upset",sep="_")], na.rm = TRUE)
  return(dataframe)
}

