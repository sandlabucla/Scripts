###################################################################################################################################
#Domain Specific Risk Taking Children's Scale
#Created by Yael Waizman
###################################################################################################################################

#####DOSPERTc Child/Teen Version Scoring
DOSPERTc <- function(dataframe){
  ###Calculate Financial subscale means 
  # Financial subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTc_F_likely <- rowMeans(dataframe[,paste("DOSPERTc_L",c(3,5,9,14,18,22,35,40),sep="")], na.rm = TRUE)
  # Financial subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTc_F_risky <- rowMeans(dataframe[,paste("DOSPERTc_R", c(3,5,9,14,18,22,35,40), sep="")], na.rm = TRUE)
  # Financial subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTc_F_benefit <- rowMeans(dataframe[,paste("DOSPERTc_B", c(3,5,9,14,18,22,35,40), sep="")], na.rm = TRUE)
  
  ###Calculate Ethical subscale means 
  # Ethical subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTc_E_likely <- rowMeans(dataframe[,paste("DOSPERTc_L",c(4,7,10,11,20,26,30,36,37,38),sep="")], na.rm = TRUE)
  # Ethical subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTc_E_risky <- rowMeans(dataframe[,paste("DOSPERTc_R", c(4,7,10,11,20,26,30,36,37,38), sep="")], na.rm = TRUE)
  # Ethical subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTc_E_benefit <- rowMeans(dataframe[,paste("DOSPERTc_B", c(4,7,10,11,20,26,30,36,37,38), sep="")], na.rm = TRUE)
  
  ###Calculate Social subscale means 
  # Social subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTc_S_likely <- rowMeans(dataframe[,paste("DOSPERTc_L",c(1,8,16,25,27,32,34),sep="")], na.rm = TRUE)
  # Social subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTc_S_risky <- rowMeans(dataframe[,paste("DOSPERTc_R", c(1,8,16,25,27,32,34), sep="")], na.rm = TRUE)
  # Social subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTc_S_benefit <- rowMeans(dataframe[,paste("DOSPERTc_B", c(1,8,16,25,27,32,34), sep="")], na.rm = TRUE)
  
  ###Calculate Recreational subscale means 
  # Recreational subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTc_R_likely <- rowMeans(dataframe[,paste("DOSPERTc_L",c(2,12,15,23,29,33),sep="")], na.rm = TRUE)
  # Recreational subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTc_R_risky <- rowMeans(dataframe[,paste("DOSPERTc_R", c(2,12,15,23,29,33), sep="")], na.rm = TRUE)
  # Recreational subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTc_R_benefit <- rowMeans(dataframe[,paste("DOSPERTc_B", c(2,12,15,23,29,33), sep="")], na.rm = TRUE)
  
  ###Calculate Health subscale means 
  # Health subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTc_H_likely <- rowMeans(dataframe[,paste("DOSPERTc_L", c(6,13,17,19,21,24,28,31,39), sep="")], na.rm = TRUE)
  # Health subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTc_H_risky <- rowMeans(dataframe[,paste("DOSPERTc_R", c(6,13,17,19,21,24,28,31,39), sep="")], na.rm = TRUE)
  # Health subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTc_H_benefit <- rowMeans(dataframe[,paste("DOSPERTc_B", c(6,13,17,19,21,24,28,31,39), sep="")], na.rm = TRUE)
  
  ###Calculate overall risk-taking mean (all subscales)
  dataframe$DOSPERTc_ALL_likely <- rowMeans(dataframe[,paste("DOSPERTc_L", c(1:40), sep="")], na.rm = TRUE)
  ###Calculate overall risk perception mean (all subscales)
  dataframe$DOSPERTc_ALL_risky <- rowMeans(dataframe[,paste("DOSPERTc_R", c(1:40), sep="")], na.rm = TRUE)
  ###Calculate overall expected benefits mean (all subscales)
  dataframe$DOSPERTc_ALL_benefit <- rowMeans(dataframe[,paste("DOSPERTc_B", c(1:40), sep="")], na.rm = TRUE)
  
  return(dataframe)
}
