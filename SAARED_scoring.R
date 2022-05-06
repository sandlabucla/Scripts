########################################################################################## 
#Screen for Adult Anxiety Related Disorders (SAARED) questionnaire scoring
#Created by Yael Waizman
########################################################################################## 

SAARED <- function(dataframe){
  ###Calculate SAARED total score sum 
  dataframe$SAARED_ANXTOTAL <- rowSums(dataframe[,paste("SAAREDa_",c(1:44),sep="")], na.rm = FALSE)
  
  ###Determine if likely presence of an Anxiety Disorder based on SAARED total score
  #A total of >= 23 may indicate presence of an Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SAARED_ANXYN <- ifelse(dataframe$SAARED_ANXTOTAL >= 23, 1, 0)
  
  ###Calculate total and determine presence of Panic Disorder / Significant Somatic Symptoms 
  dataframe$SAARED_PANIC <- rowSums(dataframe[,paste("SAAREDa_",c(1,2,6,9,11,12,15,17,18,19,22,25,28,32,36,38,40),sep="")], na.rm = FALSE)
  
  #A total of >= 5 may indicate Panic Disorder or Significant Somatic Symptoms
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SAARED_PANICYN <- ifelse(dataframe$SAARED_PANIC >= 5, 1, 0)
  
  ###Calculate total and determine presence of Generalized Anxiety Disorder 
  dataframe$SAARED_GAD <- rowSums(dataframe[,paste("SAAREDa_",c(5,7,8,14,21,23,24,29,31,35,37,39,44),sep="")], na.rm = FALSE)
  
  #A total of >= 12 may indicate Generalized Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SAARED_GADYN <- ifelse(dataframe$SAARED_GAD >= 12, 1, 0)
  
  ###Calculate total and determine presence of Separation Anxiety 
  dataframe$SAARED_SEPANX <- rowSums(dataframe[,paste("SAAREDa_",c(4,13,16,20,26,30,33),sep="")], na.rm = FALSE)
  
  #A total of >= 3 may indicate Separation Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SAARED_SEPANXYN <- ifelse(dataframe$SAARED_SEPANX >= 3, 1, 0)
  
  ###Calculate total and determine presence of Social Phobis Disorder 
  dataframe$SAARED_SOCANX <- rowSums(dataframe[,paste("SAAREDa_",c(3,10,27,34,41,42,43),sep="")], na.rm = FALSE)
  
  #A total of >= 7 may indicate Social Phobis Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SAARED_SOCANXYN <- ifelse(dataframe$SAARED_SOCANX >= 7, 1, 0)
  
  ###Calculate SAARED total score mean
  dataframe$SAARED_MEAN <- rowMeans(dataframe[,paste("SAAREDa_",c(1:44),sep="")], na.rm = TRUE)
  
  return(dataframe)
}
