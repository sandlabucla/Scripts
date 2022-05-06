########################################################################################## 
#State Trait Anxiety Inventory (STAI) questionnaire scoring
#Created by Yael Waizman
########################################################################################## 

STAI <- function(dataframe){
  #function that will reverse 4 point likert scale codes
  Reverse_code_4_likert <- function(response) {
    Reverse_code_4_likert = ifelse(response == 1, 4, 
                                   ifelse(response == 2, 3,
                                          ifelse(response == 3, 2, 1)))
    
    return(Reverse_code_4_likert)
  }
  ###First, reverse code anxiety-absent items 
  dataframe$STAI_1R <- Reverse_code_4_likert(dataframe$STAI_1) 
  dataframe$STAI_2R <- Reverse_code_4_likert(dataframe$STAI_2)
  dataframe$STAI_5R <- Reverse_code_4_likert(dataframe$STAI_5)
  dataframe$STAI_8R <- Reverse_code_4_likert(dataframe$STAI_8)
  dataframe$STAI_10R <- Reverse_code_4_likert(dataframe$STAI_10)
  dataframe$STAI_11R <- Reverse_code_4_likert(dataframe$STAI_11)
  dataframe$STAI_15R <- Reverse_code_4_likert(dataframe$STAI_15)
  dataframe$STAI_16R <- Reverse_code_4_likert(dataframe$STAI_16)
  dataframe$STAI_19R <- Reverse_code_4_likert(dataframe$STAI_19)
  dataframe$STAI_20R <- Reverse_code_4_likert(dataframe$STAI_20)
  dataframe$STAI_21R <- Reverse_code_4_likert(dataframe$STAI_21)
  dataframe$STAI_23R <- Reverse_code_4_likert(dataframe$STAI_23)
  dataframe$STAI_26R <- Reverse_code_4_likert(dataframe$STAI_26)
  dataframe$STAI_27R <- Reverse_code_4_likert(dataframe$STAI_27)
  dataframe$STAI_30R <- Reverse_code_4_likert(dataframe$STAI_30)
  dataframe$STAI_33R <- Reverse_code_4_likert(dataframe$STAI_33)
  dataframe$STAI_34R <- Reverse_code_4_likert(dataframe$STAI_34)
  dataframe$STAI_36R <- Reverse_code_4_likert(dataframe$STAI_36)
  dataframe$STAI_39R <- Reverse_code_4_likert(dataframe$STAI_39)
  
  ###Calculate State Anxiety total
  dataframe$STAI_STATE_ANX_SUM <- rowSums(dataframe[,paste("STAI_",c("1R","2R","3","4","5R","6","7","8R","9","10R","11R","12","13","14","15R","16R","17","18","19R","20R"),sep="")], na.rm = FALSE)
  ###Calculate State Anxiety mean
  dataframe$STAI_STATE_ANX_MEAN <- rowMeans(dataframe[,paste("STAI_",c("1R","2R","3","4","5R","6","7","8R","9","10R","11R","12","13","14","15R","16R","17","18","19R","20R"),sep="")], na.rm = TRUE)
  
  ###Calculate Trait Anxiety total
  dataframe$STAI_TRAIT_ANX_SUM <- rowSums(dataframe[,paste("STAI_",c("21R","22","23R","24","25","26R","27R","28","29","30R","31","32","33R","34R","35","36R","37","38","39R","40"),sep="")], na.rm = FALSE)
  ###Calculate Trait Anxiety mean
  dataframe$STAI_TRAIT_ANX_MEAN <- rowMeans(dataframe[,paste("STAI_",c("21R","22","23R","24","25","26R","27R","28","29","30R","31","32","33R","34R","35","36R","37","38","39R","40"),sep="")], na.rm = TRUE)
  
  return(dataframe)
}

