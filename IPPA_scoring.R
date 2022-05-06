########################################################################################## 
#Inventory of Parents and Peer Attachment Revised (IPPA) questionnaire scoring
#This is a modifed version, where the participant can choose who their parents are 
#(e.g., mother & father, mother & mother, father & father, just mother, or just)
########################################################################################## 
IPPA <- function(dataframe){
  #Reverse codes 5 point likert scale (from 1 to 5)
  Reverse_code_5_likert <- function(response) {
    Reverse_code_5_likert = ifelse(response == 1, 5, 
                                   ifelse(response == 2, 4,
                                          ifelse(response == 3, 3,
                                                 ifelse(response == 4, 2,1))))
    return(Reverse_code_5_likert)
  }
  ###First, reverse code negatively worded items 
  # reverse code items in Part 1&2 (questions about mother)
  dataframe$IPPA_1m_3R <- Reverse_code_5_likert(dataframe$IPPA_1m_3) 
  dataframe$IPPA_1m_6R <- Reverse_code_5_likert(dataframe$IPPA_1m_6) 
  dataframe$IPPA_1m_9R <- Reverse_code_5_likert(dataframe$IPPA_1m_9) 
  dataframe$IPPA_1m_14R <- Reverse_code_5_likert(dataframe$IPPA_1m_14)
  dataframe$IPPA_2m_3R <- Reverse_code_5_likert(dataframe$IPPA_2m_3) 
  dataframe$IPPA_2m_6R <- Reverse_code_5_likert(dataframe$IPPA_2m_6) 
  dataframe$IPPA_2m_9R <- Reverse_code_5_likert(dataframe$IPPA_2m_9) 
  dataframe$IPPA_2m_14R <- Reverse_code_5_likert(dataframe$IPPA_2m_14)
  
  # reverse code items in Part 1&2 (questions about father)
  dataframe$IPPA_1f_3R <- Reverse_code_5_likert(dataframe$IPPA_1f_3) 
  dataframe$IPPA_1f_6R <- Reverse_code_5_likert(dataframe$IPPA_1f_6) 
  dataframe$IPPA_1f_9R <- Reverse_code_5_likert(dataframe$IPPA_1f_9) 
  dataframe$IPPA_1f_14R <- Reverse_code_5_likert(dataframe$IPPA_1f_14)
  dataframe$IPPA_2f_3R <- Reverse_code_5_likert(dataframe$IPPA_2f_3) 
  dataframe$IPPA_2f_6R <- Reverse_code_5_likert(dataframe$IPPA_2f_6) 
  dataframe$IPPA_2f_9R <- Reverse_code_5_likert(dataframe$IPPA_2f_9) 
  dataframe$IPPA_2f_14R <- Reverse_code_5_likert(dataframe$IPPA_2f_14) 
  
  # reverse code items in Part 3 (questions about Peer)
  dataframe$IPPA_peer_5R <- Reverse_code_5_likert(dataframe$IPPA_peer_5) 
  
  ###Calculate Parent Trust (mother 1) total/mean
  dataframe$IPPA_PAR_TRUST_M1_SUM = rowSums(dataframe[,paste("IPPA_1m_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_TRUST_M1_MEAN = rowMeans(dataframe[,paste("IPPA_1m_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Trust (mother 2) total/mean
  dataframe$IPPA_PAR_TRUST_M2_SUM = rowSums(dataframe[,paste("IPPA_2m_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_TRUST_M2_MEAN = rowMeans(dataframe[,paste("IPPA_2m_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Trust (father 1) total/mean
  dataframe$IPPA_PAR_TRUST_F1_SUM = rowSums(dataframe[,paste("IPPA_1f_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_TRUST_F1_MEAN = rowMeans(dataframe[,paste("IPPA_1f_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Trust (father 2) total/mean
  dataframe$IPPA_PAR_TRUST_F2_SUM = rowSums(dataframe[,paste("IPPA_2f_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_TRUST_F2_MEAN = rowMeans(dataframe[,paste("IPPA_2f_",c("1","2","3R","4","9R","12","13","20","21","22"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Communication (mother 1) total/mean
  dataframe$IPPA_PAR_COMM_M1_SUM = rowSums(dataframe[,paste("IPPA_1m_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_COMM_M1_MEAN = rowMeans(dataframe[,paste("IPPA_1m_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Communication (mother 2) total/mean
  dataframe$IPPA_PAR_COMM_M2_SUM = rowSums(dataframe[,paste("IPPA_2m_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_COMM_M2_MEAN = rowMeans(dataframe[,paste("IPPA_2m_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Communication (father 1) total/mean
  dataframe$IPPA_PAR_COMM_F1_SUM = rowSums(dataframe[,paste("IPPA_1f_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_COMM_F1_MEAN = rowMeans(dataframe[,paste("IPPA_1f_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Communication (father 1) total/mean
  dataframe$IPPA_PAR_COMM_F2_SUM = rowSums(dataframe[,paste("IPPA_2f_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_COMM_F2_MEAN = rowMeans(dataframe[,paste("IPPA_2f_",c("5","6R","7","14R","15","16","19","24","25"),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Alienation (mother 1) total/mean
  dataframe$IPPA_PAR_ALIEN_M1_SUM = rowSums(dataframe[,paste("IPPA_1m_",c(8,10,11,17,18,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_ALIEN_M1_MEAN = rowMeans(dataframe[,paste("IPPA_1m_",c(8,10,11,17,18,23),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Alienation (mother 2) total/mean
  dataframe$IPPA_PAR_ALIEN_M2_SUM = rowSums(dataframe[,paste("IPPA_2m_",c(8,10,11,17,18,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_ALIEN_M2_MEAN = rowMeans(dataframe[,paste("IPPA_2m_",c(8,10,11,17,18,23),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Alienation (father 1) total/mean
  dataframe$IPPA_PAR_ALIEN_F1_SUM = rowSums(dataframe[,paste("IPPA_1f_",c(8,10,11,17,18,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_ALIEN_F1_MEAN = rowMeans(dataframe[,paste("IPPA_1f_",c(8,10,11,17,18,23),sep="")], na.rm = TRUE)
  
  ###Calculate Parent Alienation (father 2) total/mean
  dataframe$IPPA_PAR_ALIEN_F2_SUM = rowSums(dataframe[,paste("IPPA_2f_",c(8,10,11,17,18,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PAR_ALIEN_F2_MEAN = rowMeans(dataframe[,paste("IPPA_2f_",c(8,10,11,17,18,23),sep="")], na.rm = TRUE)
  
  ###Calculate Peer Trust total/mean
  dataframe$IPPA_PEER_TRUST_SUM = rowSums(dataframe[,paste("IPPA_peer_",c("5R","6","8","12","13","14","15","19","20","21"),sep="")], na.rm = FALSE)
  dataframe$IPPA_PEER_TRUST_MEAN = rowMeans(dataframe[,paste("IPPA_peer_",c("5R","6","8","12","13","14","15","19","20","21"),sep="")], na.rm = TRUE)
  
  ###Calculate Peer Communication total/mean
  dataframe$IPPA_PEER_COMM_SUM = rowSums(dataframe[,paste("IPPA_peer_",c(1,2,3,7,16,17,24,25),sep="")], na.rm = FALSE)
  dataframe$IPPA_PEER_COMM_MEAN = rowMeans(dataframe[,paste("IPPA_peer_",c(1,2,3,7,16,17,24,25),sep="")], na.rm = TRUE)
  
  ###Calculate Peer Alienation total/mean
  dataframe$IPPA_PEER_ALIEN_SUM = rowSums(dataframe[,paste("IPPA_peer_",c(4,9,10,11,18,22,23),sep="")], na.rm = FALSE)
  dataframe$IPPA_PEER_ALIEN_MEAN = rowMeans(dataframe[,paste("IPPA_peer_",c(4,9,10,11,18,22,23),sep="")], na.rm = TRUE)
  
  return(dataframe)
}






