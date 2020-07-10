########################################################################################## 
#Beck Depression Inventory (BDI - ages 14+, item on suicidality removed)
#Created by Yael Waizman
########################################################################################## 

###Calculate BDI total, BDI mean
BDI <- function(dataframe){
  #NOTE: Prior Tottenham SB scoring codes items 16 and 18 to include whether participant selected options a or b for answers coded 1,2, and 3, i.e. 16, 1a: I sleep somewhat more than usual, 1b: I sleep somewhat less than usual; otherwise n/a; Silvers SB scoring does not distinguish between answers a and b - to check specific response, see raw Qualtrics data
  #Prior Tottenham SB scoring calculates simple sum of BDI items. This is done below and coded as BDI_TOTAL. Further down a second sum is calculated to impute missing responses for item 9 (suicidality). This second total is stored in variable BDI_TOTAL_IMP.
  dataframe$BDI_TOTAL = rowSums(dataframe[,paste("BDI_",c(1:8,10:21),sep="")], na.rm = FALSE)
  
  ##Substitute the mean of 20 items included in BDI for the missing item 9 (suicidality)
  dataframe$BDI_9 <- rowMeans(dataframe[,paste("BDI_",c(1:8,10:21),sep="")], na.rm = TRUE)
  
  #Recalculate BDI total taking into account imputed missing value for item 9
  dataframe$BDI_TOTAL_IMP = rowSums(dataframe[,paste("BDI_",c(1:21),sep="")], na.rm = FALSE)
  
  #Calculate BDI mean taking into account imputed missing value for item 9
  dataframe$BDI_MEAN = rowMeans(dataframe[,paste("BDI_",c(1:21),sep="")], na.rm = TRUE)
  
  return(dataframe)
}