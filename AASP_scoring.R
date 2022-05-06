#########################################################################################################################################################
# Adolescent/Adult Sensory Profile (AASP) scoring
# Created by Joy Forster
#########################################################################################################################################################

AASP <- function(dataframe){
  ##Calculate raw scores:
  
  #Add up raw scores for Quadrant 1 (Low Registration)
  dataframe$AASP_RawScore_q1 <- dataframe$AASP_taste_3 + dataframe$AASP_taste_6 + dataframe$AASP_mvmt_4 + dataframe$AASP_mvmt_7 + dataframe$AASP_vis_5 + dataframe$AASP_vis_7 + dataframe$AASP_touch_10 + dataframe$AASP_touch_11 + dataframe$AASP_touch_13 + dataframe$AASP_activity_2 + dataframe$AASP_activity_5 + dataframe$AASP_activity_6 + dataframe$AASP_aud_3 + dataframe$AASP_aud_6 + dataframe$AASP_aud_10
  
  #Add up raw scores for Quadrant 2 (Sensation Seeking)
  dataframe$AASP_RawScore_q2 <- dataframe$AASP_taste_2 + dataframe$AASP_taste_4 + dataframe$AASP_taste_8 + dataframe$AASP_mvmt_2 + dataframe$AASP_mvmt_6 + dataframe$AASP_vis_1 + dataframe$AASP_vis_3 + dataframe$AASP_touch_2 + dataframe$AASP_touch_4 + dataframe$AASP_touch_6 + dataframe$AASP_activity_1 + dataframe$AASP_activity_3 + dataframe$AASP_activity_8 + dataframe$AASP_aud_1 + dataframe$AASP_aud_9
  
  #Add up raw scores for Quadrant 3 (Sensory Sensitivity)
  dataframe$AASP_RawScore_q3 <- dataframe$AASP_taste_7 + dataframe$AASP_mvmt_1 + dataframe$AASP_mvmt_5 + dataframe$AASP_mvmt_8 + dataframe$AASP_vis_4 + dataframe$AASP_vis_6 + dataframe$AASP_vis_9 + dataframe$AASP_touch_1 + dataframe$AASP_touch_5 + dataframe$AASP_touch_7 + dataframe$AASP_touch_8 + dataframe$AASP_activity_9 + dataframe$AASP_aud_2 + dataframe$AASP_aud_5 + dataframe$AASP_aud_11
  
  #Add up raw scores for Quadrant 4 (Sensation Avoiding)
  dataframe$AASP_RawScore_q4 <- dataframe$AASP_taste_1 + dataframe$AASP_taste_5 + dataframe$AASP_mvmt_3 + dataframe$AASP_vis_2 + dataframe$AASP_vis_8 + dataframe$AASP_vis_10 + dataframe$AASP_touch_3 + dataframe$AASP_touch_9 + dataframe$AASP_touch_12 + dataframe$AASP_activity_4 + dataframe$AASP_activity_7 + dataframe$AASP_activity_10 + dataframe$AASP_aud_4 + dataframe$AASP_aud_7 + dataframe$AASP_aud_8
  
  ##Quadrant Profile (based on raw scores):
  #-- = Much Less Than Most People
  #- = Less Than Most People
  #= = Similar To Most People
  #+ = More Than Most People
  #++ = Much More Than Most People
  
  #Quadrant 1 (Low Registration): Meaning of Score
  dataframe$AASP_SummaryScore_q1 <- ifelse(dataframe$AASP_RawScore_q1<=18, "--", 
                                    ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q1 >=19 & dataframe$AASP_RawScore_q1<=26, "-",
                                           ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q1 >=27 & dataframe$AASP_RawScore_q1<=40, "=",
                                                  ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q1 >=41 & dataframe$AASP_RawScore_q1<=51, "+", 
                                                         ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q1 >=52 & dataframe$AASP_RawScore_q1<=75, "++",
                                                                ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q1 >=19 & dataframe$AASP_RawScore_q1<=23, "-",
                                                                       ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q1 >=24 & dataframe$AASP_RawScore_q1<=35, "=",
                                                                              ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q1 >=36 & dataframe$AASP_RawScore_q1<=44, "+", "++"))))))))
  
  #Quadrant 2 (Sensation Seeking): Meaning of Score
  dataframe$AASP_SummaryScore_q2 <- ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q2<=27, "--", 
                                    ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q2 >=28 & dataframe$AASP_RawScore_q2<=41, "-", 
                                           ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q2 >=42 & dataframe$AASP_RawScore_q2<=58, "=", 
                                                  ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q2 >=59 & dataframe$AASP_RawScore_q2<=65, "+", 
                                                         ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q2 >=66 & dataframe$AASP_RawScore_q2<=75, "++", 
                                                                ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q2 >=35, "--", 
                                                                       ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q2 >=36 & dataframe$AASP_RawScore_q2<=42, "-", 
                                                                              ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q2 >= 43 & dataframe$AASP_RawScore_q2 <= 56, "=", 
                                                                                     ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q2 >=57 & dataframe$AASP_RawScore_q2<=62,"+", "++")))))))))
  
  #Quadrant 3 (Sensory Sensitivity): Meaning of Score
  dataframe$AASP_SummaryScore_q3 <- ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q3<=19, "--", 
                                    ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q3 >=20 & dataframe$AASP_RawScore_q3<=25, "-", 
                                           ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q3 >=26 & dataframe$AASP_RawScore_q3<=40, "=", 
                                                  ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q3 >=41 & dataframe$AASP_RawScore_q3<=48, "+", 
                                                         ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q3 >=49 & dataframe$AASP_RawScore_q3<=75, "++", 
                                                                ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q3 >=18, "--", 
                                                                       ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q3 >=19 & dataframe$AASP_RawScore_q3<=25, "-", 
                                                                              ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q3 >= 26 & dataframe$AASP_RawScore_q3 <= 41, "=", 
                                                                                     ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q3 >=42 & dataframe$AASP_RawScore_q3<=48,"+", "++")))))))))
  
  #Quadrant 4 (Sensation Avoiding): Meaning of Score
  dataframe$AASP_SummaryScore_q4 <- ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q4<=18, "--", 
                                    ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q4 >=19 & dataframe$AASP_RawScore_q4<=25, "-", 
                                           ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q4 >=26 & dataframe$AASP_RawScore_q4<=40, "=", 
                                                  ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q4 >=41 & dataframe$AASP_RawScore_q4<=48, "+", 
                                                         ifelse(dataframe$Age<=2 & dataframe$AASP_RawScore_q4 >=49 & dataframe$AASP_RawScore_q4<=75, "++", 
                                                                ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q4 >=19, "--", 
                                                                       ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q4 >=20 & dataframe$AASP_RawScore_q4<=26, "-", 
                                                                              ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q4 >= 27 & dataframe$AASP_RawScore_q4 <= 41, "=", 
                                                                                     ifelse(dataframe$Age>2 & dataframe$AASP_RawScore_q4 >=42 & dataframe$AASP_RawScore_q4<=49,"+", "++")))))))))
  
  return(dataframe)
}