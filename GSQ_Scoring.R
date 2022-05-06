
#########################################################################################################################################################
# Glasgow Sensory Questionnaire scoring
# Created by Joy Forster
#########################################################################################################################################################

GSQ <- function(dataframe){
  #Calculate Sums of Individual Modalities on Hyper/Hypo Sensitivity Scales: 
  #For visual, auditory, gustatory, olfactory, tactile, vestibular and proprioceptive modalities
  
  dataframe$GSQ_Hyper_Visual <- rowSums(dataframe[,paste("GSQ_",c("8_vis_hyper_1","11_vis_hyper_2","18_vis_hyper_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hypo_Visual <- rowSums(dataframe[,paste("GSQ_",c("4_vis_hypo_1","19_vis_hypo_2","42_vis_hypo_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hyper_Aud <- rowSums(dataframe[,paste("GSQ_",c("6_aud_hyper_1","25_aud_hyper_2","31_aud_hyper_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hypo_Aud <- rowSums(dataframe[,paste("GSQ_",c("9_aud_hypo_1","14_aud_hypo_2","33_aud_hypo_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hyper_Gus <- rowSums(dataframe[,paste("GSQ_",c("2_tas_hyper_1","23_tas_hyper_2","26_tas_hyper_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hypo_Gus <- rowSums(dataframe[,paste("GSQ_",c("28_tas_hypo_1","35_tas_hypo_2","39_tas_hypo_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hyper_Olf <- rowSums(dataframe[,paste("GSQ_",c("13_sme_hyper_1","21_sme_hyper_2","24_sme_hyper_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hypo_Olf <- rowSums(dataframe[,paste("GSQ_",c("7_sme_hypo_1","17_sme_hypo_2","36_sme_hypo_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hyper_Tactile <- rowSums(dataframe[,paste("GSQ_",c("1_tac_hyper_1","15_tac_hyper_2","22_tac_hyper_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hypo_Tactile <- rowSums(dataframe[,paste("GSQ_",c("16_tac_hypo_1","27_tac_hypo_2","40_tac_hypo_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hyper_Ves <- rowSums(dataframe[,paste("GSQ_",c("10_ves_hyper_1","30_ves_hyper_2","32_ves_hyper_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hypo_Ves <- rowSums(dataframe[,paste("GSQ_",c("12_ves_hypo_1","20_ves_hypo_2","34_ves_hypo_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hyper_Pro <- rowSums(dataframe[,paste("GSQ_",c("37_pro_hyper_1","38_pro_hyper_2","41_pro_hyper_3"),sep="")], na.rm = FALSE)
  dataframe$GSQ_Hypo_Pro <- rowSums(dataframe[,paste("GSQ_",c("3_pro_hypo_1","5_pro_hypo_2","29_pro_hypo_3"),sep="")], na.rm = FALSE)
  
  #Calculate Total Sums of Modalities
  dataframe$GSQ_Visual <- dataframe$GSQ_Hyper_Visual + dataframe$GSQ_Hypo_Visual
  dataframe$GSQ_Aud <- dataframe$GSQ_Hyper_Aud + dataframe$GSQ_Hypo_Aud
  dataframe$GSQ_Gus <- dataframe$GSQ_Hyper_Gus + dataframe$GSQ_Hypo_Gus
  dataframe$GSQ_Olf <- dataframe$GSQ_Hyper_Olf + dataframe$GSQ_Hypo_Olf
  dataframe$GSQ_Tactile <- dataframe$GSQ_Hyper_Tactile + dataframe$GSQ_Hypo_Tactile
  dataframe$GSQ_Ves <- dataframe$GSQ_Hyper_Ves + dataframe$GSQ_Hypo_Ves
  dataframe$GSQ_Pro <- dataframe$GSQ_Hyper_Pro + dataframe$GSQ_Hypo_Pro
  
  #Calculate Total Sums of Sensitivity Scales
  dataframe$GSQ_Total_Hyper <- dataframe$GSQ_Hyper_Visual + dataframe$GSQ_Hyper_Aud + dataframe$GSQ_Hyper_Gus + dataframe$GSQ_Hyper_Olf + dataframe$GSQ_Hyper_Tactile + dataframe$GSQ_Hyper_Ves + dataframe$GSQ_Hyper_Pro
  dataframe$GSQ_Total_Hypo <- dataframe$GSQ_Hypo_Visual + dataframe$GSQ_Hypo_Aud + dataframe$GSQ_Hypo_Gus + dataframe$GSQ_Hypo_Olf + dataframe$GSQ_Hypo_Tactile + dataframe$GSQ_Hypo_Ves + dataframe$GSQ_Hypo_Pro
  
  #Calculate Total Sum
  dataframe$GSQ_Total <- dataframe$GSQ_Total_Hyper + dataframe$GSQ_Total_Hypo
  return(dataframe)
}
