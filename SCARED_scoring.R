########################################################################################## 
# Screen for Child Anxiety Related Disorders 
# Used to score both SCAREDc - child version, and SCAREDp - parent version
# Created by Yael Waizman
########################################################################################## 

SCARED <- function(dataframe, version_tag){
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_ANXTOTAL <- paste("SCARED",version_tag,"_ANXTOTAL", sep="")
  ###Calculate SCARED total score sum 
  dataframe$SCARED_ANXTOTAL <- rowSums(dataframe[,paste("SCARED", version_tag,"_",c(1:41),sep="")], na.rm = FALSE)

  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_ANXYN <- paste("SCARED",version_tag,"_ANXYN", sep="")
  ###Determine if likely presence of an Anxiety Disorder based on SCAREDp total score
  #A total of >= 25 may indicate presence of an Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCARED_ANXYN <- ifelse(dataframe$SCAREDp_ANXTOTAL >= 25, 1, 0)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_PANIC <- paste("SCARED",version_tag,"_PANIC", sep="")
  ###Calculate total and determine presence of Panic Disorder / Significant Somatic Symptoms 
  dataframe$SCARED_PANIC <- rowSums(dataframe[,paste("SCARED",version_tag,"_",c(1,6,9,12,15,18,19,22,24,27,30,34,38),sep="")], na.rm = FALSE)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_PANICYN <- paste("SCARED",version_tag,"_PANICYN", sep="")
  #A total of >= 7 may indicate Panic Disorder or Significant Somatic Symptoms
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCARED_PANICYN <- ifelse(dataframe$SCAREDp_PANIC >= 7, 1, 0)

  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_GAD <- paste("SCARED",version_tag,"_GAD", sep="")
  ###Calculate total and determine presence of Generalized Anxiety Disorder 
  dataframe$SCARED_GAD <- rowSums(dataframe[,paste("SCARED",version_tag,"_",c(5,7,14,21,23,28,33,35,37),sep="")], na.rm = FALSE)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_GADYN <- paste("SCARED",version_tag,"_GADYN", sep="")
  #A total of >= 9 may indicate Generalized Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCARED_GADYN <- ifelse(dataframe$SCAREDp_GAD >= 9, 1, 0)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_SEPANX <- paste("SCARED",version_tag,"_SEPANX", sep="")
  ###Calculate total and determine presence of Separation Anxiety 
  dataframe$SCARED_SEPANX <- rowSums(dataframe[,paste("SCARED",version_tag,"_",c(4,8,13,16,20,25,29,31),sep="")], na.rm = FALSE)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_SEPANXYN <- paste("SCARED",version_tag,"_SEPANXYN", sep="")
  #A total of >= 5 may indicate Separation Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCARED_SEPANXYN <- ifelse(dataframe$SCAREDp_SEPANX >= 5, 1, 0)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_SOCANX <- paste("SCARED",version_tag,"_SOCANX", sep="")
  ###Calculate total and determine presence of Social Anxiety Disorder 
  dataframe$SCARED_SOCANX <- rowSums(dataframe[,paste("SCARED",version_tag,"_",c(3,10,26,32,39,40,41),sep="")], na.rm = FALSE)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_SOCANXYN <- paste("SCARED",version_tag,"_SOCANXYN", sep="")  
  #A total of >= 8 may indicate Social Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCARED_SOCANXYN <- ifelse(dataframe$SCAREDp_SOCANX >= 8, 1, 0)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_SCHOANX <- paste("SCARED",version_tag,"_SCHOANX", sep="")  
  ###Calculate total and determine presence of Significant School Avoidance 
  dataframe$SCARED_SCHOANX <- rowSums(dataframe[,paste("SCARED",version_tag,"_",c(2,11,17,36),sep="")], na.rm = FALSE)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_SCHANXYN <- paste("SCARED",version_tag,"_SCHANXYN", sep="")  
  #A total of >= 3 may indicate Significant School Avoidance
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$SCARED_SCHANXYN <- ifelse(dataframe$SCAREDp_SCHOANX >= 3, 1, 0)
  
  #combines variable name with version_tag ("c" for child version or "p" for parent version)
  SCARED_MEAN <- paste("SCARED",version_tag,"_MEAN", sep="")  
  ###Calculate SCARED total score mean
  dataframe$SCARED_MEAN <- rowMeans(dataframe[,paste("SCARED",version_tag,"_",c(1:41),sep="")], na.rm = TRUE)
  
  return(dataframe)
}
