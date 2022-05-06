########################################################################################## 
#Emotion Regulation Questionnaire (ERQ) scoring
#Created by Yael Waizman
########################################################################################## 

ERQ <- function(dataframe){
  #calculate Cognitive Reappraisal Mean
  dataframe$ERQ_Reapp_MEAN <- rowMeans(dataframe[,paste("ERQ",c(1,3,5,7,8,10),sep="")], na.rm = TRUE)
  #calculate Cognitive Reappraisal Sum Total
  dataframe$ERQ_Reapp_TOTAL <- rowSums(dataframe[,paste("ERQ",c(1,3,5,7,8,10),sep="")], na.rm = FALSE)
  
  #calculate Expressive Suppression Mean
  dataframe$ERQ_Supp_MEAN <- rowMeans(dataframe[,paste("ERQ",c(2,4,6,9),sep="")], na.rm = TRUE)
  #calculate Expressive Suppression Sum Total
  dataframe$ERQ_Supp_TOTAL <- rowSums(dataframe[,paste("ERQ",c(2,4,6,9),sep="")], na.rm = FALSE) 
  
  return(dataframe)
}