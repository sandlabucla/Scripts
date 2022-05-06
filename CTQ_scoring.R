######################################################################################
#Childhood Trauma Questionnaire (CTQ) scoring
#Created by Lizzy Gaines
#Reverse code 5 point likert scale (from 1 to 5)
#Created by Yael Waizman
######################################################################################


CTQ <- function(dataframe){
  Reverse_code_5_likert <- function(response) {
    Reverse_code_5_likert = ifelse(response == 1, 5,
                                   ifelse(response == 2, 4,
                                          ifelse(response == 3, 3,
                                                 ifelse(response == 4, 2,1))))
    return(Reverse_code_5_likert)
  }
  
  ###reverse score CTQ 2,5,7,13,19,26 & 28
  dataframe$CTQ_2_0_5yrsR <- Reverse_code_5_likert(dataframe$CTQ_2_0_5yrs)
  dataframe$CTQ_2_6_10yrsR <- Reverse_code_5_likert(dataframe$CTQ_2_6_10yrs)
  dataframe$CTQ_2_11_14yrsR <- Reverse_code_5_likert(dataframe$CTQ_2_11_14yrs)
  dataframe$CTQ_2_15_18yrsR <- Reverse_code_5_likert(dataframe$CTQ_2_15_18yrs)
  dataframe$CTQ_5_0_5yrsR <- Reverse_code_5_likert(dataframe$CTQ_5_0_5yrs)
  dataframe$CTQ_5_6_10yrsR <- Reverse_code_5_likert(dataframe$CTQ_5_6_10yrs)
  dataframe$CTQ_5_11_14yrsR <- Reverse_code_5_likert(dataframe$CTQ_5_11_14yrs)
  dataframe$CTQ_5_15_18yrsR <- Reverse_code_5_likert(dataframe$CTQ_5_15_18yrs)
  dataframe$CTQ_7_0_5yrsR <- Reverse_code_5_likert(dataframe$CTQ_7_0_5yrs)
  dataframe$CTQ_7_6_10yrsR <- Reverse_code_5_likert(dataframe$CTQ_7_6_10yrs)
  dataframe$CTQ_7_11_14yrsR <- Reverse_code_5_likert(dataframe$CTQ_7_11_14yrs)
  dataframe$CTQ_7_15_18yrsR <- Reverse_code_5_likert(dataframe$CTQ_7_15_18yrs)
  dataframe$CTQ_13_0_5yrsR <- Reverse_code_5_likert(dataframe$CTQ_13_0_5yrs)
  dataframe$CTQ_13_6_10yrsR <- Reverse_code_5_likert(dataframe$CTQ_13_6_10yrs)
  dataframe$CTQ_13_11_14yrsR <- Reverse_code_5_likert(dataframe$CTQ_13_11_14yrs)
  dataframe$CTQ_13_15_18yrsR <- Reverse_code_5_likert(dataframe$CTQ_13_15_18yrs)
  dataframe$CTQ_19_0_5yrsR <- Reverse_code_5_likert(dataframe$CTQ_19_0_5yrs)
  dataframe$CTQ_19_6_10yrsR <- Reverse_code_5_likert(dataframe$CTQ_19_6_10yrs)
  dataframe$CTQ_19_11_14yrsR <- Reverse_code_5_likert(dataframe$CTQ_19_11_14yrs)
  dataframe$CTQ_19_15_18yrsR <- Reverse_code_5_likert(dataframe$CTQ_19_15_18yrs)
  dataframe$CTQ_26_0_5yrsR <- Reverse_code_5_likert(dataframe$CTQ_26_0_5yrs)
  dataframe$CTQ_26_6_10yrsR <- Reverse_code_5_likert(dataframe$CTQ_26_6_10yrs)
  dataframe$CTQ_26_11_14yrsR <- Reverse_code_5_likert(dataframe$CTQ_26_11_14yrs)
  dataframe$CTQ_26_15_18yrsR <- Reverse_code_5_likert(dataframe$CTQ_26_15_18yrs)
  dataframe$CTQ_28_0_5yrsR <- Reverse_code_5_likert(dataframe$CTQ_28_0_5yrs)
  dataframe$CTQ_28_6_10yrsR <- Reverse_code_5_likert(dataframe$CTQ_28_6_10yrs)
  dataframe$CTQ_28_11_14yrsR <- Reverse_code_5_likert(dataframe$CTQ_28_11_14yrs)
  dataframe$CTQ_28_15_18yrsR <- Reverse_code_5_likert(dataframe$CTQ_28_15_18yrs)
  ###at this point exactly I realized I probably could've used "CTQ_#" sooo, yeah. 
  
  ###Calculate Emotional Abuse Scale Sum for each age range
  dataframe$CTQ_EMOTIONAL_ABUSE_0_5yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c(3,8,14,18,25), "_0_5yrs",sep="")], na.rm = FALSE)
  dataframe$CTQ_EMOTIONAL_ABUSE_6_10yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("3_6_10yrs","8_6_10yrs","14_6_10yrs","18_6_10yrs","25_6_10yrs"), sep="")], na.rm = FALSE)
  dataframe$CTQ_EMOTIONAL_ABUSE_11_14yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("3_11_14yrs","8_11_14yrs","14_11_14yrs","18_11_14yrs","25_11_14yrs"),sep="")], na.rm = FALSE)
  dataframe$CTQ_EMOTIONAL_ABUSE_15_18yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("3_15_18yrs","8_15_18yrs","14_15_18yrs","18_15_18yrs","25_15_18yrs"),sep="")], na.rm = FALSE)
  
    ### Calculate Emotional Abuse Mean for each age range
   dataframe$CTQ_EMOTIONAL_ABUSE_0_5yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("3_0_5yrs","8_0_5yrs","14_0_5yrs","18_0_5yrs","25_0_5yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_ABUSE_6_10yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("3_6_10yrs","8_6_10yrs","14_6_10yrs","18_6_10yrs","25_6_10yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_ABUSE_11_14yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("3_11_14yrs","8_11_14yrs","14_11_14yrs","18_11_14yrs","25_11_14yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_ABUSE_15_18yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("3_15_18yrs","8_15_18yrs","14_15_18yrs","18_15_18yrs","25_15_18yrs"),sep="")], na.rm = FALSE)
 
 ### Calculate Physical Abuse Scale Sum for each age range
   dataframe$CTQ_PHYSICAL_ABUSE_0_5yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("9_0_5yrs","11_0_5yrs","12_0_5yrs","15_0_5yrs","17_0_5yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_ABUSE_6_10yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("9_6_10yrs","11_6_10yrs","12_6_10yrs","15_6_10yrs","17_6_10yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_ABUSE_11_14yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("9_11_14yrs","11_11_14yrs","12_11_14yrs","15_11_14yrs","17_11_14yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_ABUSE_15_18yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("9_15_18yrs","11_15_18yrs","12_15_18yrs","15_15_18yrs","17_15_18yrs"),sep="")], na.rm = FALSE)
   
  ### Calculate Physical Abuse Scale Mean for each age range 
   dataframe$CTQ_PHYSICAL_ABUSE_0_5yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("9_0_5yrs","11_0_5yrs","12_0_5yrs","15_0_5yrs","17_0_5yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_ABUSE_6_10yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("9_6_10yrs","11_6_10yrs","12_6_10yrs","15_6_10yrs","17_6_10yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_ABUSE_11_14yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("9_11_14yrs","11_11_14yrs","12_11_14yrs","15_11_14yrs","17_11_14yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_ABUSE_15_18yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("9_15_18yrs","11_15_18yrs","12_15_18yrs","15_15_18yrs","17_15_18yrs"),sep="")], na.rm = FALSE)
   
  ### Calculate Sexual Abuse Scale Sum for each age range
   dataframe$CTQ_SEXUAL_ABUSE_0_5yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("20_0_5yrs","21_0_5yrs","23_0_5yrs","24_0_5yrs","27_0_5yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_SEXUAL_ABUSE_6_10yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("20_6_10yrs","21_6_10yrs","23_6_10yrs","24_6_10yrs","27_6_10yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_SEXUAL_ABUSE_11_14yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("20_11_14yrs","21_11_14yrs","23_11_14yrs","24_11_14yrs","27_11_14yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_SEXUAL_ABUSE_15_18yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("20_15_18yrs","21_15_18yrs","23_15_18yrs","24_15_18yrs","27_15_18yrs"),sep="")], na.rm = FALSE)
   
   ### Calculate Sexual Abuse Scale Mean for each age range
   dataframe$CTQ_SEXUAL_ABUSE_0_5yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("20_0_5yrs","21_0_5yrs","23_0_5yrs","24_0_5yrs","27_0_5yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_SEXUAL_ABUSE_6_10yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("20_6_10yrs","21_6_10yrs","23_6_10yrs","24_6_10yrs","27_6_10yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_SEXUAL_ABUSE_11_14yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("20_11_14yrs","21_11_14yrs","23_11_14yrs","24_11_14yrs","27_11_14yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_SEXUAL_ABUSE_15_18yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("20_15_18yrs","21_15_18yrs","23_15_18yrs","24_15_18yrs","27_15_18yrs"),sep="")], na.rm = FALSE)
   
   ### Calculate Emotional Neglect Sum for each age range
   dataframe$CTQ_EMOTIONAL_NEGLECT_0_5yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("5_0_5yrsR","7_0_5yrsR","13_0_5yrsR","19_0_5yrsR","28_0_5yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_NEGLECT_6_10yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("5_6_10yrsR","7_6_10yrsR","13_6_10yrsR","19_6_10yrsR","28_6_10yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_NEGLECT_11_14yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("5_11_14yrsR","7_11_14yrsR","13_11_14yrsR","19_11_14yrsR","28_11_14yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_NEGLECT_15_18yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("5_15_18yrsR","7_15_18yrsR","13_15_18yrsR","19_15_18yrsR","28_15_18yrsR"),sep="")], na.rm = FALSE)
   
   ### Calculate Emotional Neglect Mean for each age range
   dataframe$CTQ_EMOTIONAL_NEGLECT_0_5yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("5_0_5yrsR","7_0_5yrsR","13_0_5yrsR","19_0_5yrsR","28_0_5yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_NEGLECT_6_10yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("5_6_10yrsR","7_6_10yrsR","13_6_10yrsR","19_6_10yrsR","28_6_10yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_NEGLECT_11_14yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("5_11_14yrsR","7_11_14yrsR","13_11_14yrsR","19_11_14yrsR","28_11_14yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_EMOTIONAL_NEGLECT_15_18yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("5_15_18yrsR","7_15_18yrsR","13_15_18yrsR","19_15_18yrsR","28_15_18yrsR"),sep="")], na.rm = FALSE)
   
   ### Calculate Physical Neglect Sum for each age range
   dataframe$CTQ_PHYSICAL_NEGLECT_0_5yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("1_0_5yrs","2_0_5yrsR","4_0_5yrs","6_0_5yrs","26_0_5yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_NEGLECT_6_10yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("1_6_10yrs","2_6_10yrsR","4_6_10yrs","6_6_10yrs","26_6_10yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_NEGLECT_11_14yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("1_11_14yrs","2_11_14yrsR","4_11_14yrs","6_11_14yrs","26_11_14yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_NEGLECT_15_18yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("1_15_18yrs","2_15_18yrsR","4_15_18yrs","6_15_18yrs","26_15_18yrsR"),sep="")], na.rm = FALSE)
   
   ### Calculate Physical Neglect Mean for each age range
   dataframe$CTQ_PHYSICAL_NEGLECT_0_5yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("1_0_5yrs","2_0_5yrsR","4_0_5yrs","6_0_5yrs","26_0_5yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_NEGLECT_6_10yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("1_6_10yrs","2_6_10yrsR","4_6_10yrs","6_6_10yrs","26_6_10yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_NEGLECT_11_14yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("1_11_14yrs","2_11_14yrsR","4_11_14yrs","6_11_14yrs","26_11_14yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_PHYSICAL_NEGLECT_15_18yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("1_15_18yrs","2_15_18yrsR","4_15_18yrs","6_15_18yrs","26_15_18yrsR"),sep="")], na.rm = FALSE)
   
   ### Calculate Denial Scale Sum for each age range
   dataframe$CTQ_DENIAL_0_5yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("10_0_5yrs","16_0_5yrs","22_0_5yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_DENIAL_6_10yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("10_6_10yrs","16_6_10yrs","22_6_10yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_DENIAL_11_14yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("10_11_14yrs","16_11_14yrs","22_11_14yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_DENIAL_15_18yrs_SUM <- rowSums(dataframe[,paste("CTQ_",c("10_15_18yrs","16_15_18yrs","22_15_18yrs"),sep="")], na.rm = FALSE)
   
   ### Calculate Denial Scale Mean for each age range
   dataframe$CTQ_DENIAL_0_5yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("10_0_5yrs","16_0_5yrs","22_0_5yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_DENIAL_6_10yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("10_6_10yrs","16_6_10yrs","22_6_10yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_DENIAL_11_14yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("10_11_14yrs","16_11_14yrs","22_11_14yrs"),sep="")], na.rm = FALSE)
   dataframe$CTQ_DENIAL_15_18yrs_MEAN <- rowMeans(dataframe[,paste("CTQ_",c("10_15_18yrs","16_15_18yrs","22_15_18yrs"),sep="")], na.rm = FALSE)
   
   ### Calculate CTQ Total Sum for each age range
   dataframe$CTQ_TOTAL_0_5yrs_SUM <-rowSums(dataframe[,paste("CTQ_", c("1_0_5yrs","2_0_5yrsR","3_0_5yrs","4_0_5yrs","5_0_5yrsR","6_0_5yrs","7_0_5yrsR","8_0_5yrs","9_0_5yrs","10_0_5yrs","11_0_5yrs","12_0_5yrs","13_0_5yrsR","14_0_5yrs","15_0_5yrs","16_0_5yrs","17_0_5yrs","18_0_5yrs","19_0_5yrsR","20_0_5yrs","21_0_5yrs","22_0_5yrs","23_0_5yrs","24_0_5yrs","25_0_5yrs","26_0_5yrsR","27_0_5yrs","28_0_5yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_TOTAL_6_10yrs_SUM <-rowSums(dataframe[,paste("CTQ_", c("1_6_10yrs","2_6_10yrsR","3_6_10yrs","4_6_10yrs","5_6_10yrsR","6_6_10yrs","7_6_10yrsR","8_6_10yrs","9_6_10yrs","10_6_10yrs","11_6_10yrs","12_6_10yrs","13_6_10yrsR","14_6_10yrs","15_6_10yrs","16_6_10yrs","17_6_10yrs","18_6_10yrs","19_6_10yrsR","20_6_10yrs","21_6_10yrs","22_6_10yrs","23_6_10yrs","24_6_10yrs","25_6_10yrs","26_6_10yrsR","27_6_10yrs","28_6_10yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_TOTAL_11_14yrs_SUM <-rowSums(dataframe[,paste("CTQ_", c("1_11_14yrs","2_11_14yrsR","3_11_14yrs","4_11_14yrs","5_11_14yrsR","6_11_14yrs","7_11_14yrsR","8_11_14yrs","9_11_14yrs","10_11_14yrs","11_11_14yrs","12_11_14yrs","13_11_14yrsR","14_11_14yrs","15_11_14yrs","16_11_14yrs","17_11_14yrs","18_11_14yrs","19_11_14yrsR","20_11_14yrs","21_11_14yrs","22_11_14yrs","23_11_14yrs","24_11_14yrs","25_11_14yrs","26_11_14yrsR","27_11_14yrs","28_11_14yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_TOTAL_15_18yrs_SUM <-rowSums(dataframe[,paste("CTQ_", c("1_15_18yrs","2_15_18yrsR","3_15_18yrs","4_15_18yrs","5_15_18yrsR","6_15_18yrs","7_15_18yrsR","8_15_18yrs","9_15_18yrs","10_15_18yrs","11_15_18yrs","12_15_18yrs","13_15_18yrsR","14_15_18yrs","15_15_18yrs","16_15_18yrs","17_15_18yrs","18_15_18yrs","19_15_18yrsR","20_15_18yrs","21_15_18yrs","22_15_18yrs","23_15_18yrs","24_15_18yrs","25_15_18yrs","26_15_18yrsR","27_15_18yrs","28_15_18yrsR"),sep="")], na.rm = FALSE)
   
   ### Calculate CTQ Total Mean for each age range
   dataframe$CTQ_TOTAL_0_5yrs_MEAN <-rowMeans(dataframe[,paste("CTQ_", c("1_0_5yrs","2_0_5yrsR","3_0_5yrs","4_0_5yrs","5_0_5yrsR","6_0_5yrs","7_0_5yrsR","8_0_5yrs","9_0_5yrs","10_0_5yrs","11_0_5yrs","12_0_5yrs","13_0_5yrsR","14_0_5yrs","15_0_5yrs","16_0_5yrs","17_0_5yrs","18_0_5yrs","19_0_5yrsR","20_0_5yrs","21_0_5yrs","22_0_5yrs","23_0_5yrs","24_0_5yrs","25_0_5yrs","26_0_5yrsR","27_0_5yrs","28_0_5yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_TOTAL_6_10yrs_MEAN <-rowMeans(dataframe[,paste("CTQ_", c("1_6_10yrs","2_6_10yrsR","3_6_10yrs","4_6_10yrs","5_6_10yrsR","6_6_10yrs","7_6_10yrsR","8_6_10yrs","9_6_10yrs","10_6_10yrs","11_6_10yrs","12_6_10yrs","13_6_10yrsR","14_6_10yrs","15_6_10yrs","16_6_10yrs","17_6_10yrs","18_6_10yrs","19_6_10yrsR","20_6_10yrs","21_6_10yrs","22_6_10yrs","23_6_10yrs","24_6_10yrs","25_6_10yrs","26_6_10yrsR","27_6_10yrs","28_6_10yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_TOTAL_11_14yrs_MEAN <-rowMeans(dataframe[,paste("CTQ_", c("1_11_14yrs","2_11_14yrsR","3_11_14yrs","4_11_14yrs","5_11_14yrsR","6_11_14yrs","7_11_14yrsR","8_11_14yrs","9_11_14yrs","10_11_14yrs","11_11_14yrs","12_11_14yrs","13_11_14yrsR","14_11_14yrs","15_11_14yrs","16_11_14yrs","17_11_14yrs","18_11_14yrs","19_11_14yrsR","20_11_14yrs","21_11_14yrs","22_11_14yrs","23_11_14yrs","24_11_14yrs","25_11_14yrs","26_11_14yrsR","27_11_14yrs","28_11_14yrsR"),sep="")], na.rm = FALSE)
   dataframe$CTQ_TOTAL_15_18yrs_MEAN <-rowMeans(dataframe[,paste("CTQ_", c("1_15_18yrs","2_15_18yrsR","3_15_18yrs","4_15_18yrs","5_15_18yrsR","6_15_18yrs","7_15_18yrsR","8_15_18yrs","9_15_18yrs","10_15_18yrs","11_15_18yrs","12_15_18yrs","13_15_18yrsR","14_15_18yrs","15_15_18yrs","16_15_18yrs","17_15_18yrs","18_15_18yrs","19_15_18yrsR","20_15_18yrs","21_15_18yrs","22_15_18yrs","23_15_18yrs","24_15_18yrs","25_15_18yrs","26_15_18yrsR","27_15_18yrs","28_15_18yrsR"),sep="")], na.rm = FALSE)
   
   return(dataframe)
}





########################################################################################




