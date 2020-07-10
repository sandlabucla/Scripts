################################################################################################################## 
#Beck Depression Inventory (BDI_9_13 - ages 9-13, items on suicidality and interest in sex removed) questionnaire
#Created by Yael Waizman
################################################################################################################## 

###Calculate BDI_9_13 total, BDI_9_13 mean
#Age 9-13 version is 19 items (no item on suicidality (Q9), no item on interest in sex (Q21)), where: 
BDI_9_13 <- function(dataframe){
  #NOTE: Prior Tottenham SB scoring codes items 16 and 18 to include whether participant selected options a or b for answers coded 1, 2, and 3, i.e. 16, 1a: I sleep somewhat more than usual, 1b: I sleep somewhat less than usual; otherwise n/a; Silvers SB scoring does not distinguish between answers a and b - to check specific response, see raw Qualtrics data
  #Prior Tottenham SB scoring calculates simples sum of BDI items. This is done below and coded as BDI_9_13_TOTAL. Further down a second sum is calculated imputing missing responses for item 9 (suicidality), and item 21 (loss of interest in sex). This second total is stored in variable BDI_9_13_TOTAL_IMP.
  dataframe$BDI_9_13_TOTAL = rowSums(dataframe[,paste("BDI_9_13_",c(1:8,10:20),sep="")], na.rm = FALSE)
  
  ##Substitute the mean of 19 items included in BDI for the missing items 9 (suicidality) and 21 (loss of interest in sex) in computing BDI_9_13 total. Code as BDI_9_13_TOTAL_IMP.
  dataframe$BDI_9_13_9 <- rowMeans(dataframe[,paste("BDI_9_13_",c(1:8,10:20),sep="")], na.rm = TRUE)
  dataframe$BDI_9_13_21 <- dataframe$BDI_9_13_9
  
  #Recalculate BDI_9_13 total taking into account imputed missing values for items 9 & 21
  dataframe$BDI_9_13_TOTAL_IMP = rowSums(dataframe[,paste("BDI_9_13_",c(1:21),sep="")], na.rm = FALSE)
  
  #Calculate BDI_9_13 mean taking into account imputed missing values for items 9 & 21
  dataframe$BDI_9_13_MEAN = rowMeans(dataframe[,paste("BDI_9_13_",c(1:21),sep="")], na.rm = TRUE)
  
  return(dataframe)
}

