########################################################################################## 
#Center for Epidemiologic Studies Depression Scale (CES-D) Scoring
#Created by Yael Waizman
########################################################################################## 

###Calculate CESD total and mean
CESD <- function(dataframe){
  #function that will reverse 4 point likert scale codes
  Reverse_code_4_likert <- function(response) {
    Reverse_code_4_likert = ifelse(response == 0, 3, 
                                   ifelse(response == 1, 2,
                                          ifelse(response == 2, 1, 0)))
    
    return(Reverse_code_4_likert)
  }
  
  ###First, reverse code  
  dataframe$CESD4R <- Reverse_code_4_likert(dataframe$CESD4) 
  dataframe$CESD8R <- Reverse_code_4_likert(dataframe$CESD8)
  dataframe$CESD12R <- Reverse_code_4_likert(dataframe$CESD12)
  dataframe$CESD16R <- Reverse_code_4_likert(dataframe$CESD16)
  
  #Calculate CESD total
  dataframe$CESD_TOTAL <- rowSums(dataframe[,paste("CESD",c(1:3, "4R", 5:7, "8R", 9:11, "12R", 13:15, "16R", 17:20),sep="")], na.rm = FALSE)
  #Calculate CESD mean
  dataframe$CESD_MEAN <- rowMeans(dataframe[,paste("CESD",c(1:3, "4R", 5:7, "8R", 9:11, "12R", 13:15, "16R", 17:20),sep="")], na.rm = TRUE)
  return(dataframe)
}