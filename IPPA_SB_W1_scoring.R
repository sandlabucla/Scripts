########################################################################################## 
#Inventory of Parents and Peer Attachment Revised (IPPA) questionnaire scoring for SB W1
########################################################################################## 
IPPA_SB_W1 <- function(dataframe){
  #Reverse codes 5 point likert scale (from 1 to 5)
  Reverse_code_5_likert <- function(response) {
    Reverse_code_5_likert = ifelse(response == 1, 5, 
                                   ifelse(response == 2, 4,
                                          ifelse(response == 3, 3,
                                                 ifelse(response == 4, 2,1))))
    return(Reverse_code_5_likert)
  }
  ###First, reverse code negatively worded items 
  # reverse code items in Part 1 (questions about mother)
  dataframe$IPPA_1_3R <- Reverse_code_5_likert(dataframe$IPPA_1_3) 
  dataframe$IPPA_1_6R <- Reverse_code_5_likert(dataframe$IPPA_1_6) 
  dataframe$IPPA_1_9R <- Reverse_code_5_likert(dataframe$IPPA_1_9) 
  dataframe$IPPA_1_14R <- Reverse_code_5_likert(dataframe$IPPA_1_14) 
  # reverse code items in Part 2 (questions about father)
  dataframe$IPPA_2_3R <- Reverse_code_5_likert(dataframe$IPPA_2_3) 
  dataframe$IPPA_2_6R <- Reverse_code_5_likert(dataframe$IPPA_2_6) 
  dataframe$IPPA_2_9R <- Reverse_code_5_likert(dataframe$IPPA_2_9) 
  dataframe$IPPA_2_14R <- Reverse_code_5_likert(dataframe$IPPA_2_14) 
  # reverse code items in Part 3 (questions about Peer)
  dataframe$IPPA_3_5R <- Reverse_code_5_likert(dataframe$IPPA_3_5) 
  
  ###Calculate Parent Trust (mother) total/mean
  dataframe$IPPA_PAR_TRUST_M_SUM = rowSums(dataframe[,paste("IPPA_1_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_TRUST_M_MEAN = rowMeans(dataframe[,paste("IPPA_1_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Trust (father) total/mean
  dataframe$IPPA_PAR_TRUST_F_SUM = rowSums(dataframe[,paste("IPPA_2_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_TRUST_F_MEAN = rowMeans(dataframe[,paste("IPPA_2_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Communication (mother) total/mean
  dataframe$IPPA_PAR_COMM_M_SUM = rowSums(dataframe[,paste("IPPA_1_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_COMM_M_MEAN = rowMeans(dataframe[,paste("IPPA_1_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Communication (father) total/mean
  dataframe$IPPA_PAR_COMM_F_SUM = rowSums(dataframe[,paste("IPPA_2_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_COMM_F_MEAN = rowMeans(dataframe[,paste("IPPA_2_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Alienation (mother) total/mean
  dataframe$IPPA_PAR_ALIEN_M_SUM = rowSums(dataframe[,paste("IPPA_1_",c(8,10,11,17,18,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_ALIEN_M_MEAN = rowMeans(dataframe[,paste("IPPA_1_",c(8,10,11,17,18,23),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Alienation (father) total/mean
  dataframe$IPPA_PAR_ALIEN_F_SUM = rowSums(dataframe[,paste("IPPA_2_",c(8,10,11,17,18,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_ALIEN_F_MEAN = rowMeans(dataframe[,paste("IPPA_2_",c(8,10,11,17,18,23),sep="")], na.rm = TRUE)
  
  ###Calculate Peer Trust total/mean
  dataframe$IPPA_PEER_TRUST_SUM = rowSums(dataframe[,paste("IPPA_3_",c("5R","6","8","12","13","14","15","19","20","21"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PEER_TRUST_MEAN = rowMeans(dataframe[,paste("IPPA_3_",c("5R","6","8","12","13","14","15","19","20","21"),sep="")], na.rm = TRUE)
  
  ###Calculate Peer Communication total/mean
  dataframe$IPPA_PEER_COMM_SUM = rowSums(dataframe[,paste("IPPA_3_",c(1,2,3,7,16,17,24,25),sep="")], na.rm = FALSE)
  dataframe$IPPA_PEER_COMM_MEAN = rowMeans(dataframe[,paste("IPPA_3_",c(1,2,3,7,16,17,24,25),sep="")], na.rm = TRUE)
  
  ###Calculate Peer Alienation total/mean
  dataframe$IPPA_PEER_ALIEN_SUM = rowSums(dataframe[,paste("IPPA_3_",c(4,9,10,11,18,22,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PEER_ALIEN_MEAN = rowMeans(dataframe[,paste("IPPA_3_",c(4,9,10,11,18,22,23),sep="")], na.rm = TRUE)

  return(dataframe)
}






