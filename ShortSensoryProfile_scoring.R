########################################################################################## 
#Short Sensory Profile (SSP) scoring
#Created by Yael Waizman
########################################################################################## 

SSP <- function(dataframe){
  ###Calculate Tactile Sensitivity subscale
  dataframe$SSP_TACTILE <- rowSums(dataframe[,paste("SSP_tactile_",c(1:7),sep="")], na.rm = FALSE)
  
  ###Calculate Taste/Smell Sensitivity subscale
  dataframe$SSP_TASTE <- rowSums(dataframe[,paste("SSP_taste_",c(1:4),sep="")], na.rm = FALSE)
  
  ###Calculate Movement Sensitivity subscale
  dataframe$SSP_MVMT <- rowSums(dataframe[,paste("SSP_mvmt_",c(1:3),sep="")], na.rm = FALSE)
  
  ###Calculate Underresponsive/Seeks Sensation subscale
  dataframe$SSP_UNDERRESP <- rowSums(dataframe[,paste("SSP_underresp_",c(1:7),sep="")], na.rm = FALSE)
  
  ###Calculate Auditory Filtering subscale
  dataframe$SSP_AUD <- rowSums(dataframe[,paste("SSP_aud_",c(1:6),sep="")], na.rm = FALSE)
  
  ###Calculate Low Energy/Weak subscale
  dataframe$SSP_WEAK <- rowSums(dataframe[,paste("SSP_weak_",c(1:6),sep="")], na.rm = FALSE)
  
  ###Calculate Visual/Auditory Sensitivity subscale
  dataframe$SSP_VIS_AUD <- rowSums(dataframe[,paste("SSP_vis_aud_",c(1:5),sep="")], na.rm = FALSE)
  
  ###Calculate Total score
  dataframe$SSP_TOTAL <- rowSums(dataframe[,paste("SSP_",c("TACTILE","TASTE","MVMT","UNDERRESP","AUD","WEAK","VIS_AUD"),sep="")], na.rm = FALSE)
  
  ##Categorize based on deviation from a normative sample 
  #NOTE: This is based on the raw score, not a t-score, per questionnaire scoring instructions 
  
  #0 = Typical Performance
  #1 = Probable Difference (from normative sample)
  #2 = Definite Difference (from normative sample)
  
  ###Tactile subscale category
  dataframe$SSP_TACTILE_CAT <- ifelse(dataframe$SSP_TACTILE >= 7 & dataframe$SSP_TACTILE <= 26, 2, 
                                         ifelse(dataframe$SSP_TACTILE >= 27 & dataframe$SSP_TACTILE <= 29, 1,
                                                0))
  
  ###Taste/smell subscale category
  dataframe$SSP_TASTE_CAT <- ifelse(dataframe$SSP_TASTE >= 4 & dataframe$SSP_TASTE <= 11, 2, 
                                       ifelse(dataframe$SSP_TASTE >= 12 & dataframe$SSP_TASTE <= 14, 1,
                                              0))
  
  ###Movement subscale category
  dataframe$SSP_MVMT_CAT <- ifelse(dataframe$SSP_MVMT >= 3 & dataframe$SSP_MVMT <= 10, 2, 
                                      ifelse(dataframe$SSP_MVMT >= 11 & dataframe$SSP_MVMT <= 12, 1,
                                             0))
  
  ###Underresponsive subscale category
  dataframe$SSP_UNDERRESP_CAT <- ifelse(dataframe$SSP_UNDERRESP >= 7 & dataframe$SSP_UNDERRESP <= 23, 2, 
                                           ifelse(dataframe$SSP_UNDERRESP >= 24 & dataframe$SSP_UNDERRESP <= 26, 1,
                                                  0))
  
  ###Auditory subscale category
  dataframe$SSP_AUD_CAT <- ifelse(dataframe$SSP_AUD >= 6 & dataframe$SSP_AUD <= 19, 2, 
                                     ifelse(dataframe$SSP_AUD >= 20 & dataframe$SSP_AUD <= 22, 1,
                                            0))
  
  ###Low energy/weak subscale category
  dataframe$SSP_WEAK_CAT <- ifelse(dataframe$SSP_WEAK >= 6 & dataframe$SSP_WEAK <= 23, 2, 
                                      ifelse(dataframe$SSP_WEAK >= 24 & dataframe$SSP_WEAK <= 25, 1,
                                             0))
  
  ###Visual/auditory subscale category
  dataframe$SSP_VIS_AUD_CAT <- ifelse(dataframe$SSP_VIS_AUD >= 5 & dataframe$SSP_VIS_AUD <= 15, 2, 
                                         ifelse(dataframe$SSP_VIS_AUD >= 16 & dataframe$SSP_VIS_AUD <= 18, 1,
                                                0))
  
  ###Total category
  dataframe$SSP_TOTAL_CAT <- ifelse(dataframe$SSP_TOTAL >= 38 & dataframe$SSP_TOTAL <= 141, 2, 
                                       ifelse(dataframe$SSP_TOTAL >= 142 & dataframe$SSP_TOTAL <= 154, 1,
                                              0))
  return(dataframe)
}