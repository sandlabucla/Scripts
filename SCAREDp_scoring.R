########################################################################################## 
#Screen for Child Anxiety Related Disorders - Parent Version (SCAREDp) 
#(caregiver/parent report on behalf of child) questionnaire scoring
#Created by Yael Waizman
########################################################################################## 

SCAREDp <- function(dataframe){
  ###Calculate SCARED total score sum 
  dataframe$SCAREDp_ANXTOTAL <- rowSums(dataframe[,paste("SCAREDp_",c(1:41),sep="")], na.rm = FALSE)

  ###Determine if likely presence of an Anxiety Disorder based on SCAREDp total score
  #A total of >= 25 may indicate presence of an Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDp_ANXYN <- ifelse(dataframe$SCAREDp_ANXTOTAL >= 25, 1, 0)
  
  ###Calculate total and determine presence of Panic Disorder / Significant Somatic Symptoms 
  dataframe$SCAREDp_PANIC <- rowSums(dataframe[,paste("SCAREDp_",c(1,6,9,12,15,18,19,22,24,27,30,34,38),sep="")], na.rm = FALSE)
  
  #A total of >= 7 may indicate Panic Disorder or Significant Somatic Symptoms
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDp_PANICYN <- ifelse(dataframe$SCAREDp_PANIC >= 7, 1, 0)

  ###Calculate total and determine presence of Generalized Anxiety Disorder 
  dataframe$SCAREDp_GAD <- rowSums(dataframe[,paste("SCAREDp_",c(5,7,14,21,23,28,33,35,37),sep="")], na.rm = FALSE)
  
  #A total of >= 9 may indicate Generalized Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDp_GADYN <- ifelse(dataframe$SCAREDp_GAD >= 9, 1, 0)
  
  ###Calculate total and determine presence of Separation Anxiety 
  dataframe$SCAREDp_SEPANX <- rowSums(dataframe[,paste("SCAREDp_",c(4,8,13,16,20,25,29,31),sep="")], na.rm = FALSE)
  
  #A total of >= 5 may indicate Separation Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDp_SEPANXYN <- ifelse(dataframe$SCAREDp_SEPANX >= 5, 1, 0)
  
  ###Calculate total and determine presence of Social Anxiety Disorder 
  dataframe$SCAREDp_SOCANX <- rowSums(dataframe[,paste("SCAREDp_",c(3,10,26,32,39,40,41),sep="")], na.rm = FALSE)
    
  #A total of >= 8 may indicate Social Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDp_SOCANXYN <- ifelse(dataframe$SCAREDp_SOCANX >= 8, 1, 0)
  
  ###Calculate total and determine presence of Significant School Avoidance 
  dataframe$SCAREDp_SCHOANX <- rowSums(dataframe[,paste("SCAREDp_",c(2,11,17,36),sep="")], na.rm = FALSE)
  
  #A total of >= 3 may indicate Significant School Avoidance
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCAREDp_SCHANXYN <- ifelse(dataframe$SCAREDp_SCHOANX >= 3, 1, 0)
  
  ###Calculate SCARED total score mean
  dataframe$SCAREDp_MEAN <- rowMeans(dataframe[,paste("SCAREDp_",c(1:41),sep="")], na.rm = TRUE)
  
  return(dataframe)
}
