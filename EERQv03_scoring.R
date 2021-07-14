#########################################################################################################################################################
# E-Emotion Regulation Questionnaire (EERQ) version 03 scoring 
# Created by Joy Forster
#########################################################################################################################################################

EERQ <- function(dataframe){
  #calculate Reappraisal Mean
  dataframe$EERQ_RP_MEAN <- rowMeans(dataframe[,paste("EERQ_",c(1,5,10,17,19,22),sep="")], na.rm = TRUE)
  #calculate Reappraisal Total
  dataframe$EERQ_RP_TOTAL <- rowSums(dataframe[,paste("EERQ_",c(1,5,10,17,19,22),sep="")], na.rm = FALSE)
  
  #calculate Suppression Mean
  dataframe$EERQ_SP_MEAN <- rowMeans(dataframe[,paste("EERQ_",c(4,9,13,20),sep="")], na.rm = TRUE)
  #calculate Suppression Total
  dataframe$EERQ_SP_TOTAL <- rowSums(dataframe[,paste("EERQ_",c(4,9,13,20),sep="")], na.rm = FALSE) 
  
  #calculate Situation Selection Mean
  dataframe$EERQ_SS_MEAN <- rowMeans(dataframe[,paste("EERQ_",c(2,3,8),sep="")], na.rm = TRUE)
  #calculate Situation Selection Total
  dataframe$EERQ_SS_TOTAL <- rowSums(dataframe[,paste("EERQ_",c(2,3,8),sep="")], na.rm = FALSE)
  
  #calculate Distraction Mean
  dataframe$EERQ_DS_MEAN <- rowMeans(dataframe[,paste("EERQ_",c(6,12,15,18,21),sep="")], na.rm = TRUE)
  #calculate Distraction Total
  dataframe$EERQ_DS_TOTAL <- rowSums(dataframe[,paste("EERQ_",c(6,12,15,18,21),sep="")], na.rm = FALSE)
  
  #calculate Selective Attention Mean
  dataframe$EERQ_SA_MEAN <- rowMeans(dataframe[,paste("EERQ_",c(7,11,14,16),sep="")], na.rm = TRUE)
  #calculate Selective Attention Total
  dataframe$EERQ_SA_TOTAL <- rowSums(dataframe[,paste("EERQ_",c(7,11,14,16),sep="")], na.rm = FALSE)
  
  return(dataframe)
}