########################################################################################## 
#UCLA Loneliness Scale (LS) scoring
#Created by Yael Waizman
########################################################################################## 

LS <- function(dataframe){
  #function that will reverse 4 point likert scale codes
  Reverse_code_4_likert <- function(response) {
    Reverse_code_4_likert = ifelse(response == 1, 4, 
                                   ifelse(response == 2, 3,
                                          ifelse(response == 3, 2, 1)))
    
    return(Reverse_code_4_likert)
  }
  ###First, reverse code anxiety-absent items 
  dataframe$LS1R <- Reverse_code_4_likert(dataframe$LS1) 
  dataframe$LS5R <- Reverse_code_4_likert(dataframe$LS5)
  dataframe$LS6R <- Reverse_code_4_likert(dataframe$LS6)
  dataframe$LS9R <- Reverse_code_4_likert(dataframe$LS9)
  dataframe$LS10R <- Reverse_code_4_likert(dataframe$LS10)
  dataframe$LS15R <- Reverse_code_4_likert(dataframe$LS15)
  dataframe$LS16R <- Reverse_code_4_likert(dataframe$LS16)
  dataframe$LS19R <- Reverse_code_4_likert(dataframe$LS19)
  dataframe$LS20R <- Reverse_code_4_likert(dataframe$LS20)
 
  
  ###Calculate State Anxiety total
  dataframe$LS_ANX_SUM <- rowSums(dataframe[,paste("LS",c("1R",2:4,"5R","6R",7:8,"9R","10R",11:14,"15R","16R",17:18,"19R","20R"),sep="")], na.rm = FALSE)
  ###Calculate State Anxiety mean
  dataframe$LS_ANX_MEAN <- rowMeans(dataframe[,paste("LS",c("1R",2:4,"5R","6R",7:8,"9R","10R",11:14,"15R","16R",17:18,"19R","20R"),sep="")], na.rm = TRUE)
  
  return(dataframe)
}

