#########################################################################################################################################################
# 2_way-SSS (2-way Social Support Scale) scoring
# Created by Lizzy Gaines
#########################################################################################################################################################

SSS <- function(dataframe){
  #Calculate Receiving Emotional Support Subscale
  dataframe$SSS_RES_MEAN <- rowMeans(dataframe[,paste("2Way_",c("1","2","3","4","5","6","7"),sep="")], na.rm = TRUE)
  dataframe$SSS_RES_SUM <- rowSums(dataframe[,paste("2Way_",c("1","2","3","4","5","6","7"),sep="")], na.rm = FALSE)
  
  #Calculate Receiving Instrumental Support Subscale
  dataframe$SSS_RIS_MEAN <- rowMeans(dataframe[,paste("2Way_",c("13","14","15","16"),sep="")], na.rm = TRUE)
  dataframe$SSS_RIS_SUM <- rowSums(dataframe[,paste("2Way_",c("13","14","15","16"),sep="")], na.rm = TRUE)
  
  #Calculate Receiving Social Support Scale
  dataframe$SSS_RSS_MEAN <- rowMeans(dataframe[,paste("2Way_",c("1","2","3","4","5","6","7","13","14","15","16"),sep="")], na.rm = TRUE)
  dataframe$SSS_RSS_SUM <- rowSums(dataframe[,paste("2Way_",c("1","2","3","4","5","6","7","13","14","15","16"),sep="")], na.rm = TRUE)
  
  #Calculate Giving Emotional Support Subscale
  dataframe$SSS_GES_MEAN <- rowMeans(dataframe[,paste("2Way_",c("8","9","10","11","12"),sep="")], na.rm = TRUE)
  dataframe$SSS_GES_SUM <- rowSums(dataframe[,paste("2Way_",c("8","9","10","11","12"),sep="")], na.rm = FALSE)
  
  #Calculate Giving Instrumental Support Subscale
  dataframe$SSS_GIS_MEAN <- rowMeans(dataframe[,paste("2Way_",c("17","18","19","20"),sep="")], na.rm = TRUE)
  dataframe$SSS_GIS_SUM <- rowSums(dataframe[,paste("2Way_",c("17","18","19","20"),sep="")], na.rm = TRUE)
  
  #Calculate Giving Social Support Scale
  dataframe$SSS_GSS_MEAN <- rowMeans(dataframe[,paste("2Way_",c("8","9","10","11","12","17","18","19","20"),sep="")], na.rm = TRUE)
  dataframe$SSS_GSS_SUM <- rowSums(dataframe[,paste("2Way_",c("8","9","10","11","12","17","18","19","20"),sep="")], na.rm = TRUE)
  
  return(dataframe)
}







