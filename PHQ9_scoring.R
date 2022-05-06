########################################################################################## 
#Patient Health Questionnaire (PHQ-9)
#Created by Yael Waizman
########################################################################################## 

###Calculate PHQ9 total and mean
PHQ9 <- function(dataframe){
  #Calculate PHQ9 total
  dataframe$PHQ9_TOTAL = rowSums(dataframe[,paste("PHQ",c(1:8, "9a"),sep="")], na.rm = TRUE)
  #Calculate PHQ9 mean
  dataframe$PHQ9_MEAN = rowMeans(dataframe[,paste("PHQ",c(1:8, "9a"),sep="")], na.rm = TRUE)
  return(dataframe)
}