########################################################################################## 
#SCAREDc (child self-report) questionnaire scoring
#Created by Yael Waizman
########################################################################################## 

SCAREDc <- function(dataframe){
  ###Calculate SCARED total score sum 
  dataframe$SCAREDc_ANXTOTAL <- rowSums(dataframe[,paste("SCAREDc_",c(1:41),sep="")], na.rm = FALSE)

  ###Determine if likely presence of an Anxiety Disorder based on SCAREDc total score
  #A total of >= 25 may indicate presence of an Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDc_ANXYN <- ifelse(dataframe$SCAREDc_ANXTOTAL >= 25, 1, 0)
  
  ###Calculate total and determine presence of Panic Disorder / Significant Somatic Symptoms 
  dataframe$SCAREDc_PANIC <- rowSums(dataframe[,paste("SCAREDc_",c(1,6,9,12,15,18,19,22,24,27,30,34,38),sep="")], na.rm = FALSE)
  
  
  #A total of >= 7 may indicate Panic Disorder or Significant Somatic Symptoms
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDc_PANICYN <- ifelse(dataframe$SCAREDc_PANIC >= 7, 1, 0)

  ###Calculate total and determine presence of Generalized Anxiety Disorder 
  dataframe$SCAREDc_GAD <- rowSums(dataframe[,paste("SCAREDc_",c(5,7,14,21,23,28,33,35,37),sep="")], na.rm = FALSE)
  
  #A total of >= 9 may indicate Generalized Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDc_GADYN <- ifelse(dataframe$SCAREDc_GAD >= 9, 1, 0)
  
  ###Calculate total and determine presence of Separation Anxiety 
  dataframe$SCAREDc_SEPANX <- rowSums(dataframe[,paste("SCAREDc_",c(4,8,13,16,20,25,29,31),sep="")], na.rm = FALSE)
  
  #A total of >= 5 may indicate Separation Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDc_SEPANXYN <- ifelse(dataframe$SCAREDc_SEPANX >= 5, 1, 0)
  
  ###Calculate total and determine presence of Social Anxiety Disorder 
  dataframe$SCAREDc_SOCANX <- rowSums(dataframe[,paste("SCAREDc_",c(3,10,26,32,39,40,41),sep="")], na.rm = FALSE)
    
  #A total of >= 8 may indicate Social Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDc_SOCANXYN <- ifelse(dataframe$SCAREDc_SOCANX >= 8, 1, 0)
  
  ###Calculate total and determine presence of Significant School Avoidance 
  dataframe$SCAREDc_SCHOANX <- rowSums(dataframe[,paste("SCAREDc_",c(2,11,17,36),sep="")], na.rm = FALSE)
  
  #A total of >= 3 may indicate Significant School Avoidance
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDc_SCHANXYN <- ifelse(dataframe$SCAREDc_SCHOANX >= 3, 1, 0)
  
  ###Calculate SCARED total score mean
  dataframe$SCAREDc_MEAN <- rowMeans(dataframe[,paste("SCAREDc_",c(1:41),sep="")], na.rm = TRUE)
  
  return(dataframe)
}
