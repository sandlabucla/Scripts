#########################################################################################################################################################
# Reappraisal Capability (RC) scoring 
# Created by Joy Forster
#########################################################################################################################################################

RC <- function(dataframe){
  #Find the mean of all 8 items
  dataframe$RC_ReappCapacity_MEAN <- rowMeans(dataframe[,paste("RC", c(1:8), sep="_")], na.rm = TRUE)
  return(dataframe)
}