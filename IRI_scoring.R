#########################################################################################################################################################
# Interpersonal Reactivity Index (IRI) scoring
# Created by Joy Forster
#########################################################################################################################################################

IRI <- function(dataframe){
  #Reverse codes 5 point likert scale (from 1 to 5)
  Reverse_code_likert <- function(response) {
    Reverse_code_likert = ifelse(response == 0, 4, 
                                   ifelse(response == 1, 3,
                                          ifelse(response == 2, 2,
                                                 ifelse(response == 3, 1,0))))
    return(Reverse_code_likert)
  }
  
  #Reverse code certain questions (3,4,13,14,15,18,19)
  dataframe$IRI_3R <- Reverse_code_likert(dataframe$IRI_3)
  dataframe$IRI_4R <- Reverse_code_likert(dataframe$IRI_4)
  dataframe$IRI_13R <- Reverse_code_likert(dataframe$IRI_13)
  dataframe$IRI_14R <- Reverse_code_likert(dataframe$IRI_14)
  dataframe$IRI_15R <- Reverse_code_likert(dataframe$IRI_15)
  dataframe$IRI_18R <- Reverse_code_likert(dataframe$IRI_18)
  dataframe$IRI_19R <- Reverse_code_likert(dataframe$IRI_19)

  #Calculate Perspective-Taking Subscale
  dataframe$IRI_PT_MEAN <- rowMeans(dataframe[,paste("IRI_",c("3R","8","11","15R","21","25","28"),sep="")], na.rm = TRUE)
  dataframe$IRI_PT_SUM <- rowSums(dataframe[,paste("IRI_",c("3R","8","11","15R","21","25","28"),sep="")], na.rm = FALSE)
  
  #Calculate Fantasy Scale Subscale
  dataframe$IRI_FS_MEAN <- dataframe$IRI_23
  dataframe$IRI_FS_SUM <- dataframe$IRI_23
  
  #Calculate Empathetic Concern Subscale
  dataframe$IRI_EC_MEAN <- rowMeans(dataframe[,paste("IRI_",c("2","4R","9","14R","18R","20","22"),sep="")], na.rm = TRUE)
  dataframe$IRI_EC_SUM <- rowSums(dataframe[,paste("IRI_",c("2","4R","9","14R","18R","20","22"),sep="")], na.rm = FALSE)
  
  #Calculate Personal Distress Subscale
  dataframe$IRI_PD_MEAN <- rowMeans(dataframe[,paste("IRI_",c("6","10","13R","17","19R","24","27"),sep="")], na.rm = TRUE)
  dataframe$IRI_PD_SUM <- rowSums(dataframe[,paste("IRI_",c("6","10","13R","17","19R","24","27"),sep="")], na.rm = FALSE)
  
  return(dataframe)
}







