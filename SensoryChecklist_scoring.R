########################################################################################## 
#Sensory Checklist (SC) scoring
#Created by Yael Waizman
########################################################################################## 

SC <- function(dataframe){
  ###Calculate SUR Intense subscale
  dataframe$SC_SUR_INTENSE <- rowSums(dataframe[,paste("SC_SUR_",c(1:5),sep="")], na.rm = FALSE)
  
  ###Calculate SUR Notice subscale
  dataframe$SC_SUR_NOTICE <- rowSums(dataframe[,paste("SC_SUR_",c(6:10),sep="")], na.rm = FALSE)
  
  ###Calculate SUR Respond subscale
  dataframe$SC_SUR_RESPOND <- rowSums(dataframe[,paste("SC_SUR_",c(11:13),sep="")], na.rm = FALSE)
  
  ###Calculate SUR Unaware subscale
  dataframe$SC_SUR_UNAWARE <- rowSums(dataframe[,paste("SC_SUR_",c(14:17),sep="")], na.rm = FALSE)
  
  ###Calculate SUR Other subscale
  dataframe$SC_SUR_OTHER <- rowSums(dataframe[,paste("SC_SUR_",c(18:20),sep="")], na.rm = FALSE)
  
  ###Calculate SUR total
  dataframe$SC_SUR_TOTAL <- rowSums(dataframe[,paste("SC_SUR_",c(1:20),sep="")], na.rm = FALSE)
  
  ###Calculate SS Desire subscale 
  dataframe$SC_SS_DESIRE <- rowSums(dataframe[,paste("SC_SS_",c(1:7),sep="")], na.rm = FALSE)
  
  ###Calculate SS Disengage_1 subscale 
  dataframe$SC_SS_DISENGAGE_1 <- rowSums(dataframe[,paste("SC_SS_",c(8:10),sep="")], na.rm = FALSE) 
  
  ###Calculate SS Disengage_2 subscale 
  dataframe$SC_SS_DISENGAGE_2 <- rowSums(dataframe[,paste("SC_SS_",c(11:14),sep="")], na.rm = FALSE) 
  
  ###Calculate SS Disengage_3 subscale 
  dataframe$SC_SS_DISENGAGE_3 <- rowSums(dataframe[,paste("SC_SS_",c(15:16),sep="")], na.rm = FALSE) 
  
  ###Calculate SS Like subscale 
  dataframe$SC_SS_LIKE <- rowSums(dataframe[,paste("SC_SS_",c(17:20),sep="")], na.rm = FALSE) 
  
  ##Calculate SS Total
  dataframe$SC_SS_TOTAL <- rowSums(dataframe[,paste("SC_SS_",c(1:20),sep="")], na.rm = FALSE)
  
  ###Calculate tactile subscale
  dataframe$SC_TACTILE <- rowSums(dataframe[,paste("SC_tactile_",c(1:17),sep="")], na.rm = FALSE)
  
  ###Calculate visual subscale
  dataframe$SC_VISUAL <- rowSums(dataframe[,paste("SC_visual_",c(1:5),sep="")], na.rm = FALSE)
  
  ###Calculate auditory subscale
  dataframe$SC_AUD <- rowSums(dataframe[,paste("SC_aud_",c(1:12,15:22),sep="")], na.rm = FALSE)
  
  ###Calculate movement subscale
  dataframe$SC_MVMT <- rowSums(dataframe[,paste("SC_mvmt_",c(1:5),sep="")], na.rm = FALSE)
  
  ###Calculate SOR total count (aud, vis, tact subtotals)
  dataframe$SC_TOTAL <- rowSums(cbind(dataframe$SC_AUD, dataframe$SC_VISUAL, dataframe$SC_TACTILE), na.rm = FALSE)
  
  return(dataframe)
}