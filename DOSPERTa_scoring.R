###################################################################################################################################
#Domain Specific Risk Taking Adolescent's Scale
#Created by Yael Waizman
###################################################################################################################################

#####DOSPERT Adolescent Version Scoring
DOSPERTa <- function(dataframe){
  ###Calculate Financial subscale means 
  # Financial subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTa_F_likely <- rowMeans(dataframe[,paste("DOSPERTa_L",c(3,5,9,14,18,22,35),sep="")], na.rm = TRUE)
  # Financial subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTa_F_risky <- rowMeans(dataframe[,paste("DOSPERTa_R", c(3,5,9,14,18,22,35), sep="")], na.rm = TRUE)
  # Financial subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTa_F_benefit <- rowMeans(dataframe[,paste("DOSPERTa_B", c(3,5,9,14,18,22,35), sep="")], na.rm = TRUE)
  
  ###Calculate Ethical subscale means 
  # Ethical subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTa_E_likely <- rowMeans(dataframe[,paste("DOSPERTa_L",c(4,7,10,11,20,26,30,36,37),sep="")], na.rm = TRUE)
  # Ethical subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTa_E_risky <- rowMeans(dataframe[,paste("DOSPERTa_R", c(4,7,10,11,20,26,30,36,37), sep="")], na.rm = TRUE)
  # Ethical subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTa_E_benefit <- rowMeans(dataframe[,paste("DOSPERTa_B", c(4,7,10,11,20,26,30,36,37), sep="")], na.rm = TRUE)
  
  ###Calculate Social subscale means 
  # Social subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTa_S_likely <- rowMeans(dataframe[,paste("DOSPERTa_L",c(1,8,16,25,27,32,34),sep="")], na.rm = TRUE)
  # Social subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTa_S_risky <- rowMeans(dataframe[,paste("DOSPERTa_R", c(1,8,16,25,27,32,34), sep="")], na.rm = TRUE)
  # Social subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTa_S_benefit <- rowMeans(dataframe[,paste("DOSPERTa_B", c(1,8,16,25,27,32,34), sep="")], na.rm = TRUE)
  
  ###Calculate Recreational subscale means 
  # Recreational subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTa_R_likely <- rowMeans(dataframe[,paste("DOSPERTa_L",c(2,12,15,23,29,33,38,39),sep="")], na.rm = TRUE)
  # Recreational subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTa_R_risky <- rowMeans(dataframe[,paste("DOSPERTa_R", c(2,12,15,23,29,33,38,39), sep="")], na.rm = TRUE)
  # Recreational subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTa_R_benefit <- rowMeans(dataframe[,paste("DOSPERTa_B", c(2,12,15,23,29,33,38,39), sep="")], na.rm = TRUE)
  
  ###Calculate Health subscale means 
  # Health subscale risk-taking mean ("How likely...?")
  dataframe$DOSPERTa_H_likely <- rowMeans(dataframe[,paste("DOSPERTa_L", c(6,13,17,19,21,24,28,31), sep="")], na.rm = TRUE)
  # Health subscale risk perception mean ("How risky...?")
  dataframe$DOSPERTa_H_risky <- rowMeans(dataframe[,paste("DOSPERTa_R", c(6,13,17,19,21,24,28,31), sep="")], na.rm = TRUE)
  # Health subscale expected benefits mean ("How beneficial...?")
  dataframe$DOSPERTa_H_benefit <- rowMeans(dataframe[,paste("DOSPERTa_B", c(6,13,17,19,21,24,28,31), sep="")], na.rm = TRUE)
  
  ###Calculate overall risk-taking mean (all subscales)
  dataframe$DOSPERTa_ALL_likely <- rowMeans(dataframe[,paste("DOSPERTa_L", c(1:39), sep="")], na.rm = TRUE)
  ###Calculate overall risk perception mean (all subscales)
  dataframe$DOSPERTa_ALL_risky <- rowMeans(dataframe[,paste("DOSPERTa_R", c(1:39), sep="")], na.rm = TRUE)
  ###Calculate overall expected benefits mean (all subscales)
  dataframe$DOSPERTa_ALL_benefit <- rowMeans(dataframe[,paste("DOSPERTa_B", c(1:39), sep="")], na.rm = TRUE)
  
  return(dataframe)
}
