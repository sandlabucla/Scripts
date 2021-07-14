#########################################################################################################################################################
# Issues Checklist - Child (ICc) Version scoring
# Created by Yael Waizman
#########################################################################################################################################################

ICc <- function(dataframe){
  #Quantity of issues: Count the number of issues marked “yes.”
  dataframe$ICc_Issues_Sum <- rowSums(dataframe[,paste("ICc",c(1:21,"Other"),sep="_")], na.rm = FALSE)
  dataframe$ICc_Issues_Mean <- rowMeans(dataframe[,paste("ICc",c(1:21,"Other"),sep="_")], na.rm = TRUE)
  #Intensity of issues: For issues marked “yes,” add intensity ratings and divide by the number of issues marked 
  #“yes” to obtain mean intensity rating.
  dataframe$ICc_Mean_Intensity <- rowMeans(dataframe[,paste("ICc",c(1:21,"Other"), "Upset",sep="_")], na.rm = TRUE)
  return(dataframe)
}