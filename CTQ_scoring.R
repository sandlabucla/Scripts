########################################################################################## 
#Childhood Trauma Questionnaire (CTQ) Scoring
#Created by Yael Waizman
########################################################################################## 

CTQ <- function(dataframe){
  # function that will reverse 5 point likert scale codes
  Reverse_code_5_likert <- function(response) {
    
    Reverse_code_5_likert = ifelse(response == 1, 5, 
                                   ifelse(response == 2, 4,
                                          ifelse(response == 3, 3,
                                                 ifelse(response == 4, 2,1))))
    return(Reverse_code_5_likert)
  }
  
  ### First, reverse code anxiety-absent items 
  dataframe$CTQ2R <- Reverse_code_5_likert(dataframe$CTQ2)
  dataframe$CTQ5R <- Reverse_code_5_likert(dataframe$CTQ5) 
  dataframe$CTQ7R <- Reverse_code_5_likert(dataframe$CTQ7) 
  dataframe$CTQ13R <- Reverse_code_5_likert(dataframe$CTQ13)
  dataframe$CTQ19R <- Reverse_code_5_likert(dataframe$CTQ19)
  dataframe$CTQ26R <- Reverse_code_5_likert(dataframe$CTQ26)
  dataframe$CTQ28R <- Reverse_code_5_likert(dataframe$CTQ28)
  
  #Calculate emotional abuse scale
  dataframe$CTQ_Emotional_Abuse <- rowSums(dataframe[,paste("CTQ",c(3,8,14,18,25),sep="")], na.rm = FALSE)
  #Calculate physical abuse scale
  dataframe$CTQ_Physical_Abuse <- rowSums(dataframe[,paste("CTQ",c(9,11,12,15,17),sep="")], na.rm = FALSE)
  #Calculate sexual abuse scale
  dataframe$CTQ_Sexual_Abuse <- rowSums(dataframe[,paste("CTQ",c(20,21,23,24,27),sep="")], na.rm = FALSE)
  #Calculate emotional neglect scale
  dataframe$CTQ_Emotional_Neglect <- rowSums(dataframe[,paste("CTQ",c("5R","7R","13R","19R","28R"),sep="")], na.rm = FALSE)
  #Calculate physical neglect scale
  dataframe$CTQ_Physical_Neglect <- rowSums(dataframe[,paste("CTQ",c(1,"2R",4,6,"26R"),sep="")], na.rm = FALSE)
  #Calculate total CTQ score
  dataframe$CTQ_Total <- rowSums(dataframe[,paste("CTQ",c(1, "2R", 3:4,"5R",6,"7R",8:12,"13R", 14:18, "19R", 20:25, "26R", 27:28),sep="")], na.rm = FALSE)
  
  return(dataframe)
}