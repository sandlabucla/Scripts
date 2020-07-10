########################################################################################## 
#DEBQ questionnaire scoring
#Created by Yael Waizman
##########################################################################################

DEBQ <- function(dataframe){
  #Reverse codes 5 point likert scale (from 1 to 5)
  Reverse_code_5_likert <- function(response) {
    Reverse_code_5_likert = ifelse(response == 1, 5, 
                                   ifelse(response == 2, 4,
                                          ifelse(response == 3, 3,
                                                 ifelse(response == 4, 2,1))))
    return(Reverse_code_5_likert)
  }
  
  ###First, reverse code item DEBQ_21 using function
  dataframe$DEBQ_21R <- Reverse_code_5_likert(dataframe$DEBQ_21) 
  
  ###Calculate Restrained Eating subscale mean using DEBQ_scoring function
  dataframe$DEBQ_restraint <- rowMeans(dataframe[,paste("DEBQ_",c(4,7,11,14,17,19,22,26,29,31),sep="")], na.rm = TRUE)
  
  ###Calculate Emotional Eating subscale mean using DEBQ_scoring function
  dataframe$DEBQ_emotional <- rowMeans(dataframe[,paste("DEBQ_",c(1,3,5,8,10,13,16,20,23,25,28,30,32),sep="")], na.rm = TRUE)
  
  ###Calculate External Eating subscale mean using DEBQ_scoring function
  dataframe$DEBQ_external <- rowMeans(dataframe[,paste("DEBQ_",c("2","6","9","12","15","18","21R","24","27","33"),sep="")], na.rm = TRUE)

  return(dataframe)
}






