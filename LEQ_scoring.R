########################################################################################## 
#Life Events Questionnaire (LEQ) scoring
#Created by Yael Waizman
########################################################################################## 

LEQ <- function(dataframe){
  ###Count the number of all of the negative life events within the past 3 months and the past 12 months, separately. Scores range from 0 (no negative events in past 3 months or past 12 months) to 40 (maximum number of negative events)
  #checks all of the LEQ A items, responses coded as 1 (i.e. happened within past 3 months) remain as is, otherwise, all other responses are coded as 0
  #sums up all of the past 3 months responses for the LEQ A items
  dataframe$LEQ_3MO <- rowSums(ifelse(dataframe[,paste("LEQ_",c(1:40),"A",sep="")] == 1, 1, 0), na.rm = TRUE)
  
  #checks all of the LEQ A items, responses coded as 2 (i.e. happened within past 12 months) remain as is, otherwise, all other responses are coded as 0
  #sums up all of the past 12 months responses for the LEQ A items 
  dataframe$LEQ_12MO = rowSums(ifelse(dataframe[,paste("LEQ_",c(1:40),"A",sep="")] == 2, 1, 0), na.rm = TRUE) 
  
  ###Find maximum impact score (i.e., highest score for any b question, the parent provided in the entire questionnaire.)
  for (curr_participant in 1:nrow(SB_all_scored)){
    max_index <- which.max(dataframe[,paste("LEQ_",c(1:40),"B_IMPACT",sep="")][curr_participant,]) #finds the index for the maximum impact score in each participant's row
    ifelse(length(max_index)==0, dataframe$LEQ_IMPACT[curr_participant] <- 0, dataframe$LEQ_IMPACT[curr_participant] <- dataframe[,paste("LEQ_",c(1:40),"B_IMPACT",sep="")][curr_participant,max_index]) #inputs the particpants maximum impact score into dataframe[,paste("LEQ_",c(1:40),"B_IMPACT",sep="")], if does not have a maximum impact score, inputs NA
  }
  
  return(dataframe)
}