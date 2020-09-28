########################################################################################## 
#Gets ASR Friends scale T-score based on participant gender and Friends scale total score 
#Created by Emilia Ninova
########################################################################################## 
ASR_FRIENDS_TSCORE <- function(gender, total_score) {
  
T_score = ifelse(gender == 1 & total_score == 0, 20, 
          ifelse(gender == 1 & total_score == 1, 23, 
          ifelse(gender == 1 & total_score == 2, 27, 
          ifelse(gender == 1 & total_score == 3, 30, 
          ifelse(gender == 1 & total_score == 4, 32, 
          ifelse(gender == 1 & total_score == 5, 34, 
          ifelse(gender == 1 & total_score == 6, 36, 
          ifelse(gender == 1 & total_score == 7, 40, 
          ifelse(gender == 1 & total_score == 8, 44, 
          ifelse(gender == 1 & total_score == 9, 48, 
          ifelse(gender == 1 & total_score == 10, 52, 
          ifelse(gender == 1 & total_score == 11, 57, 
          ifelse(gender == 1 & total_score == 12, 60, 
                                                                                                                              
          ifelse(gender == 2 & total_score == 0, 20, 
          ifelse(gender == 2 & total_score == 1, 23, 
          ifelse(gender == 2 & total_score == 2, 26, 
          ifelse(gender == 2 & total_score == 3, 29, 
          ifelse(gender == 2 & total_score == 4, 32, 
          ifelse(gender == 2 & total_score == 5, 34, 
          ifelse(gender == 2 & total_score == 6, 38, 
          ifelse(gender == 2 & total_score == 7, 41, 
          ifelse(gender == 2 & total_score == 8, 44, 
          ifelse(gender == 2 & total_score == 9, 48, 
          ifelse(gender == 2 & total_score == 10, 53, 
          ifelse(gender == 2 & total_score == 11, 58, 
          ifelse(gender == 2 & total_score == 12, 60, 666))))))))))))))))))))))))))
}


######################################################################################################## 
#Gets ASR Spouse/Partner scale T-score based on participant gender and Spouse/Partner scale total score 
########################################################################################################  
ASR_SPOUSE_PARTNER_TSCORE = function(gender, total_score) {
  
T_score = ifelse(gender == 1 & total_score == -8, 20, 
          ifelse(gender == 1 & total_score == -7, 22, 
          ifelse(gender == 1 & total_score == -6, 23, 
          ifelse(gender == 1 & total_score == -5, 25, 
          ifelse(gender == 1 & total_score == -4, 26, 
          ifelse(gender == 1 & total_score == -3, 28, 
          ifelse(gender == 1 & total_score == -2, 29, 
          ifelse(gender == 1 & total_score == -1, 31, 
          ifelse(gender == 1 & total_score == 0, 34, 
          ifelse(gender == 1 & total_score == 1, 36, 
          ifelse(gender == 1 & total_score == 2, 39, 
          ifelse(gender == 1 & total_score == 3, 42, 
          ifelse(gender == 1 & total_score == 4, 43, 
          ifelse(gender == 1 & total_score == 5, 46, 
          ifelse(gender == 1 & total_score == 6, 50, 
          ifelse(gender == 1 & total_score == 7, 56, 
          ifelse(gender == 1 & total_score == 8, 60,
                                                                                                                                                                 
          ifelse(gender == 2 & total_score == -8, 20, 
          ifelse(gender == 2 & total_score == -7, 22, 
          ifelse(gender == 2 & total_score == -6, 24, 
          ifelse(gender == 2 & total_score == -5, 26, 
          ifelse(gender == 2 & total_score == -4, 28, 
          ifelse(gender == 2 & total_score == -3, 30, 
          ifelse(gender == 2 & total_score == -2, 32, 
          ifelse(gender == 2 & total_score == -1, 33, 
          ifelse(gender == 2 & total_score == 0, 35, 
          ifelse(gender == 2 & total_score == 1, 37, 
          ifelse(gender == 2 & total_score == 2, 39, 
          ifelse(gender == 2 & total_score == 3, 42, 
          ifelse(gender == 2 & total_score == 4, 44, 
          ifelse(gender == 2 & total_score == 5, 47, 
          ifelse(gender == 2 & total_score == 6, 50, 
          ifelse(gender == 2 & total_score == 7, 55, 
          ifelse(gender == 2 & total_score == 8, 60, 666))))))))))))))))))))))))))))))))))
}



#######################################################################################  
#Gets ASR Family scale T-score based on participant gender and Family scale mean score 
####################################################################################### 
ASR_FAMILY_TSCORE <- function(gender, mean_score) {

# Then, gets t-score based on mean score 
T_score = ifelse(gender == 1 & mean_score == 0.0, 20, 
          ifelse(gender == 1 & mean_score == 0.2, 23, 
          ifelse(gender == 1 & mean_score == 0.4, 27, 
          ifelse(gender == 1 & mean_score == 0.6, 30, 
          ifelse(gender == 1 & mean_score == 0.8, 33, 
          ifelse(gender == 1 & mean_score == 1.0, 38, 
          ifelse(gender == 1 & mean_score == 1.2, 41, 
          ifelse(gender == 1 & mean_score == 1.4, 44, 
          ifelse(gender == 1 & mean_score == 1.6, 48, 
          ifelse(gender == 1 & mean_score == 1.8, 51, 
          ifelse(gender == 1 & mean_score == 2.0, 57, 
                                                                                                               
          ifelse(gender == 2 & mean_score == 0.0, 20, 
          ifelse(gender == 2 & mean_score == 0.2, 23, 
          ifelse(gender == 2 & mean_score == 0.4, 25, 
          ifelse(gender == 2 & mean_score == 0.6, 28, 
          ifelse(gender == 2 & mean_score == 0.8, 30, 
          ifelse(gender == 2 & mean_score == 1.0, 36, 
          ifelse(gender == 2 & mean_score == 1.2, 41, 
          ifelse(gender == 2 & mean_score == 1.4, 44, 
          ifelse(gender == 2 & mean_score == 1.6, 48, 
          ifelse(gender == 2 & mean_score == 1.8, 52, 
          ifelse(gender == 2 & mean_score == 2.0, 58,  666))))))))))))))))))))))

}



##################################################################################         
#Gets ASR Job scale T-score based on participant gender and Job scale total score 
################################################################################## 
ASR_JOB_TSCORE <- function(gender, total_score) {

T_score = ifelse(gender == 1 & total_score == -12, 20, 
          ifelse(gender == 1 & total_score == -11, 21, 
          ifelse(gender == 1 & total_score == -10, 23, 
          ifelse(gender == 1 & total_score == -9, 24, 
          ifelse(gender == 1 & total_score == -8, 26, 
          ifelse(gender == 1 & total_score == -7, 27, 
          ifelse(gender == 1 & total_score == -6, 29, 
          ifelse(gender == 1 & total_score == -5, 30, 
          ifelse(gender == 1 & total_score == -4, 33, 
          ifelse(gender == 1 & total_score == -3, 34, 
          ifelse(gender == 1 & total_score == -2, 36, 
          ifelse(gender == 1 & total_score == -1, 38,       
          ifelse(gender == 1 & total_score == 0, 41,       
          ifelse(gender == 1 & total_score == 1, 44,       
          ifelse(gender == 1 & total_score == 2, 48,       
          ifelse(gender == 1 & total_score == 3, 52,       
          ifelse(gender == 1 & total_score == 4, 58,       
                                                                                                                                                        
          ifelse(gender == 2 & total_score == -12, 20, 
          ifelse(gender == 2 & total_score == -11, 21, 
          ifelse(gender == 2 & total_score == -10, 23, 
          ifelse(gender == 2 & total_score == -9, 24, 
          ifelse(gender == 2 & total_score == -8, 26, 
          ifelse(gender == 2 & total_score == -7, 27, 
          ifelse(gender == 2 & total_score == -6, 29, 
          ifelse(gender == 2 & total_score == -5, 30, 
          ifelse(gender == 2 & total_score == -4, 32, 
          ifelse(gender == 2 & total_score == -3, 33, 
          ifelse(gender == 2 & total_score == -2, 35, 
          ifelse(gender == 2 & total_score == -1, 37,       
          ifelse(gender == 2 & total_score == 0, 41,     
          ifelse(gender == 2 & total_score == 1, 44,       
          ifelse(gender == 2 & total_score == 2, 47,       
          ifelse(gender == 2 & total_score == 3, 50,       
          ifelse(gender == 2 & total_score == 4, 56, 666))))))))))))))))))))))))))))))))))
}


##############################################################################################  
#Gets ASR Education scale T-score based on participant gender and Education scale total score 
##############################################################################################  
ASR_EDU_TSCORE <- function(gender, total_score) {
  
  T_score = ifelse(gender == 1 & total_score == -4, 20, 
            ifelse(gender == 1 & total_score == -3, 23, 
            ifelse(gender == 1 & total_score == -2, 25, 
            ifelse(gender == 1 & total_score == -1, 28, 
            ifelse(gender == 1 & total_score == 0, 30, 
            ifelse(gender == 1 & total_score == 1, 36, 
            ifelse(gender == 1 & total_score == 2, 41, 
            ifelse(gender == 1 & total_score == 3, 45, 
            ifelse(gender == 1 & total_score == 4, 50, 
            ifelse(gender == 1 & total_score == 5, 56, 
            ifelse(gender == 1 & total_score == 6, 60, 
            
            ifelse(gender == 2 & total_score == -4, 20, 
            ifelse(gender == 2 & total_score == -3, 23, 
            ifelse(gender == 2 & total_score == -2, 25, 
            ifelse(gender == 2 & total_score == -1, 28, 
            ifelse(gender == 2 & total_score == 0, 30, 
            ifelse(gender == 2 & total_score == 1, 34, 
            ifelse(gender == 2 & total_score == 2, 39, 
            ifelse(gender == 2 & total_score == 3, 44, 
            ifelse(gender == 2 & total_score == 4, 48, 
            ifelse(gender == 2 & total_score == 5, 54, 
            ifelse(gender == 2 & total_score == 6, 60,  666))))))))))))))))))))))
}

###################################################################################################################################
#Gets ASR Mean Adaptive T-score based on participant gender and overall Mean Adaptive score (i.e. get t-score of the mean t-score)
################################################################################################################################### 
ASR_MEAN_ADAPT_TSCORE <- function(gender, mean_score) {  
  
# Then, use mean adaptive score to get t-score 
T_score_male = ifelse(gender == 1 & mean_score >= 20 & mean_score <= 20.5, 20, 
          ifelse(gender == 1 & mean_score >= 21 & mean_score <= 22.5, 21,
          ifelse(gender == 1 & mean_score >= 23 & mean_score <= 24.5, 22,
          ifelse(gender == 1 & mean_score >= 25 & mean_score <= 26, 23,
          ifelse(gender == 1 & mean_score >= 26.5 & mean_score <= 28, 24,
          ifelse(gender == 1 & mean_score >= 28.5 & mean_score <= 30, 25,
          ifelse(gender == 1 & mean_score >= 30.5 & mean_score <= 31.5, 26,
          ifelse(gender == 1 & mean_score >= 32 & mean_score <= 33.5, 27,
          ifelse(gender == 1 & mean_score >= 34 & mean_score <= 35.5, 28,
          ifelse(gender == 1 & mean_score >= 36 & mean_score <= 36.5, 29,
          ifelse(gender == 1 & mean_score == 37, 30,
          ifelse(gender == 1 & mean_score >= 37.5 & mean_score <= 38, 31,   
          ifelse(gender == 1 & mean_score >= 38.5 & mean_score <= 39, 32,      
          ifelse(gender == 1 & mean_score >= 39.5 & mean_score <= 40, 33,       
          ifelse(gender == 1 & mean_score == 40.5, 34,       
          ifelse(gender == 1 & mean_score == 41, 35,       
          ifelse(gender == 1 & mean_score == 41.5, 36,
          ifelse(gender == 1 & mean_score == 42, 37,   
          ifelse(gender == 1 & mean_score == 42.5, 38,   
          ifelse(gender == 1 & mean_score == 43, 39,   
          ifelse(gender == 1 & mean_score == 43.5, 40,   
          ifelse(gender == 1 & mean_score == 44, 41,   
          ifelse(gender == 1 & mean_score == 44.5, 42,   
          ifelse(gender == 1 & mean_score >= 45 & mean_score <= 45.5, 43,   
          ifelse(gender == 1 & mean_score >= 46 & mean_score <= 46.5, 44,   
          ifelse(gender == 1 & mean_score == 47, 45,   
          ifelse(gender == 1 & mean_score == 47.5, 46,   
          ifelse(gender == 1 & mean_score >= 48 & mean_score <= 48.5, 47,   
          ifelse(gender == 1 & mean_score == 49, 48,   
          ifelse(gender == 1 & mean_score == 49.5, 49,   
          ifelse(gender == 1 & mean_score == 50, 50,   
          ifelse(gender == 1 & mean_score == 50.5, 51,   
          ifelse(gender == 1 & mean_score == 51, 52,   
          ifelse(gender == 1 & mean_score == 51.5, 53,   
          ifelse(gender == 1 & mean_score == 52, 54,   
          ifelse(gender == 1 & mean_score >= 52.5 & mean_score <= 53, 55,   
          ifelse(gender == 1 & mean_score == 53.5, 56,   
          ifelse(gender == 1 & mean_score == 54, 57,   
          ifelse(gender == 1 & mean_score == 54.5, 58,   
          ifelse(gender == 1 & mean_score >= 55 & mean_score <= 56, 59,   
          ifelse(gender == 1 & mean_score >= 56.5 & mean_score <= 60, 60, NA)))))))))))))))))))))))))))))))))))))))))   
                                                                                                                                 
T_score_female = ifelse(gender == 2 & mean_score >= 20 & mean_score <= 20.5, 20, 
          ifelse(gender == 2 & mean_score >= 21 & mean_score <= 22.5, 21,
          ifelse(gender == 2 & mean_score >= 23 & mean_score <= 24, 22,
          ifelse(gender == 2 & mean_score >= 24.5 & mean_score <= 26, 23,
          ifelse(gender == 2 & mean_score >= 26.5 & mean_score <= 27.5, 24,
          ifelse(gender == 2 & mean_score >= 28 & mean_score <= 29.5, 25,
          ifelse(gender == 2 & mean_score >= 30 & mean_score <= 31.5, 26,
          ifelse(gender == 2 & mean_score >= 32 & mean_score <= 33, 27,
          ifelse(gender == 2 & mean_score >= 33.5 & mean_score <= 35, 28,
          ifelse(gender == 2 & mean_score >= 35.5 & mean_score <= 36, 29,
          ifelse(gender == 2 & mean_score == 36.5, 30,
          ifelse(gender == 2 & mean_score == 37, 31,      
          ifelse(gender == 2 & mean_score >= 37.5 & mean_score <= 38, 32,    
          ifelse(gender == 2 & mean_score == 38.5, 33,      
          ifelse(gender == 2 & mean_score >= 39 & mean_score <= 39.5, 34,      
          ifelse(gender == 2 & mean_score == 40, 35,      
          ifelse(gender == 2 & mean_score == 40.5, 36,
          ifelse(gender == 2 & mean_score >= 41 & mean_score <= 41.5, 37,   
          ifelse(gender == 2 & mean_score == 42, 38,   
          ifelse(gender == 2 & mean_score == 42.5, 39,   
          ifelse(gender == 2 & mean_score >= 43 & mean_score <= 43.5, 40,   
          ifelse(gender == 2 & mean_score == 44, 41,   
          ifelse(gender == 2 & mean_score == 44.5, 42,   
          ifelse(gender == 2 & mean_score == 45, 43,   
          ifelse(gender == 2 & mean_score == 45.5, 44,   
          ifelse(gender == 2 & mean_score == 46, 45,   
          ifelse(gender == 2 & mean_score >= 46.5 & mean_score <= 47, 46,   
          ifelse(gender == 2 & mean_score == 47.5, 47,   
          ifelse(gender == 2 & mean_score == 48, 48,   
          ifelse(gender == 2 & mean_score >= 48.5 & mean_score <= 49, 49,   
          ifelse(gender == 2 & mean_score == 49.5, 50,   
          ifelse(gender == 2 & mean_score == 50, 51,   
          ifelse(gender == 2 & mean_score == 50.5, 52,   
          ifelse(gender == 2 & mean_score == 51, 53,   
          ifelse(gender == 2 & mean_score == 51.5, 54,   
          ifelse(gender == 2 & mean_score >= 52 & mean_score <= 52.5, 55,   
          ifelse(gender == 2 & mean_score == 53, 57,   
          ifelse(gender == 2 & mean_score >= 53.5 & mean_score <= 54, 58,   
          ifelse(gender == 2 & mean_score >= 54.5 & mean_score <= 55.5, 59,   
          ifelse(gender == 2 & mean_score >= 56 & mean_score <= 60, 60,
                 NA))))))))))))))))))))))))))))))))))))))))

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
# above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
return(T_score)
}


###################################################################################################################################
#Determines if participant falls in the clinical, borderline, or normal range based on ASR Mean Adaptive T-score
###################################################################################################################################

ASR_MEAN_ADAPT_RANGE <- function(t_score) {
  
  T_score_range = ifelse(t_score >= 20 & t_score <= 30, "clinical", 
                  ifelse(t_score >= 31 & t_score <= 35, "borderline", 
                  ifelse(t_score >= 36 & t_score <= 60, "normal", 666)))

return(T_score_range)
}
##############################################################################################  
#Gets ASR Anxious/Depressed scale T-score based on participant gender and total score 
##############################################################################################  

ASR_ANX_DEP_TSCORE <- function(gender, total_score) {
  
  T_score_male = ifelse(gender == 1 & total_score >= 0 & total_score <= 3, 50,
            ifelse(gender == 1 & total_score == 4, 51, 
            ifelse(gender == 1 & total_score == 5, 52, 
            ifelse(gender == 1 & total_score == 6, 53, 
            ifelse(gender == 1 & total_score == 7, 54, 
            ifelse(gender == 1 & total_score == 8, 56, 
            ifelse(gender == 1 & total_score == 9, 58, 
            ifelse(gender == 1 & total_score == 10, 60, 
            ifelse(gender == 1 & total_score == 11, 61, 
            ifelse(gender == 1 & total_score == 12, 62, 
            ifelse(gender == 1 & total_score == 13, 63, 
            ifelse(gender == 1 & total_score == 14, 64, 
            ifelse(gender == 1 & total_score == 15, 65, 
            ifelse(gender == 1 & total_score == 16, 67, 
            ifelse(gender == 1 & total_score == 17, 68, 
            ifelse(gender == 1 & total_score == 18, 69, 
            ifelse(gender == 1 & total_score == 19, 70, 
            ifelse(gender == 1 & total_score == 20, 72, 
            ifelse(gender == 1 & total_score == 21, 74, 
            ifelse(gender == 1 & total_score == 22, 75, 
            ifelse(gender == 1 & total_score == 23, 77,
            ifelse(gender == 1 & total_score == 24, 79,
            ifelse(gender == 1 & total_score == 25, 81,
            ifelse(gender == 1 & total_score == 26, 82,
            ifelse(gender == 1 & total_score == 27, 84,
            ifelse(gender == 1 & total_score == 28, 86,
            ifelse(gender == 1 & total_score == 29, 88,
            ifelse(gender == 1 & total_score == 30, 89,
            ifelse(gender == 1 & total_score == 31, 91,
            ifelse(gender == 1 & total_score == 32, 93,
            ifelse(gender == 1 & total_score == 33, 95,
            ifelse(gender == 1 & total_score == 34, 96,
            ifelse(gender == 1 & total_score == 35, 98, 
            ifelse(gender == 1 & total_score == 36, 100, NA))))))))))))))))))))))))))))))))))

    T_score_female <- ifelse(gender == 2 & total_score >= 0 & total_score <= 4, 50,
            ifelse(gender == 2 & total_score == 5, 51, 
            ifelse(gender == 2 & total_score == 6, 52, 
            ifelse(gender == 2 & total_score == 7, 53, 
            ifelse(gender == 2 & total_score == 8, 54, 
            ifelse(gender == 2 & total_score == 9, 56, 
            ifelse(gender == 2 & total_score == 10, 58, 
            ifelse(gender == 2 & total_score == 11, 59, 
            ifelse(gender == 2 & total_score == 12, 60, 
            ifelse(gender == 2 & total_score == 13, 62, 
            ifelse(gender == 2 & total_score == 14, 63, 
            ifelse(gender == 2 & total_score == 15, 64, 
            ifelse(gender == 2 & total_score == 16, 65, 
            ifelse(gender == 2 & total_score == 17, 66, 
            ifelse(gender == 2 & total_score == 18, 68, 
            ifelse(gender == 2 & total_score == 19, 69, 
            ifelse(gender == 2 & total_score == 20, 70, 
            ifelse(gender == 2 & total_score == 21, 72, 
            ifelse(gender == 2 & total_score == 22, 74, 
            ifelse(gender == 2 & total_score == 23, 76,
            ifelse(gender == 2 & total_score == 24, 78,
            ifelse(gender == 2 & total_score == 25, 79,
            ifelse(gender == 2 & total_score == 26, 81,
            ifelse(gender == 2 & total_score == 27, 83,
            ifelse(gender == 2 & total_score == 28, 85,
            ifelse(gender == 2 & total_score == 29, 87,
            ifelse(gender == 2 & total_score == 30, 89,
            ifelse(gender == 2 & total_score == 31, 91,
            ifelse(gender == 2 & total_score == 32, 93,
            ifelse(gender == 2 & total_score == 33, 94,
            ifelse(gender == 2 & total_score == 34, 96,
            ifelse(gender == 2 & total_score == 35, 98, 
            ifelse(gender == 2 & total_score == 36, 100, NA)))))))))))))))))))))))))))))))))
  
    T_score = T_score_male
    T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
    # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
    return(T_score)
}


##############################################################################################  
#Gets ASR Withdrawn scale T-score based on participant gender and total score 
##############################################################################################  


ASR_WITHDRAWN_TSCORE <- function(gender, total_score) {
  
  T_score = ifelse(gender == 1 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 1 & total_score == 2, 51,
            ifelse(gender == 1 & total_score == 3, 54,
            ifelse(gender == 1 & total_score == 4, 57, 
            ifelse(gender == 1 & total_score == 5, 60, 
            ifelse(gender == 1 & total_score == 6, 63, 
            ifelse(gender == 1 & total_score == 7, 66, 
            ifelse(gender == 1 & total_score == 8, 69, 
            ifelse(gender == 1 & total_score == 9, 70, 
            ifelse(gender == 1 & total_score == 10, 73, 
            ifelse(gender == 1 & total_score == 11, 77, 
            ifelse(gender == 1 & total_score == 12, 80, 
            ifelse(gender == 1 & total_score == 13, 83, 
            ifelse(gender == 1 & total_score == 14, 87, 
            ifelse(gender == 1 & total_score == 15, 90, 
            ifelse(gender == 1 & total_score == 16, 93, 
            ifelse(gender == 1 & total_score == 17, 97, 
            ifelse(gender == 1 & total_score == 18, 100,   

            ifelse(gender == 2 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 2 & total_score == 2, 51,
            ifelse(gender == 2 & total_score == 3, 55,
            ifelse(gender == 2 & total_score == 4, 59,
            ifelse(gender == 2 & total_score == 5, 62, 
            ifelse(gender == 2 & total_score == 6, 65, 
            ifelse(gender == 2 & total_score == 7, 67, 
            ifelse(gender == 2 & total_score == 8, 69, 
            ifelse(gender == 2 & total_score == 9, 70, 
            ifelse(gender == 2 & total_score == 10, 73, 
            ifelse(gender == 2 & total_score == 11, 77, 
            ifelse(gender == 2 & total_score == 12, 80, 
            ifelse(gender == 2 & total_score == 13, 83, 
            ifelse(gender == 2 & total_score == 14, 87, 
            ifelse(gender == 2 & total_score == 15, 90, 
            ifelse(gender == 2 & total_score == 16, 93, 
            ifelse(gender == 2 & total_score == 17, 97, 
            ifelse(gender == 2 & total_score == 18, 100,  666))))))))))))))))))))))))))))))))))))
  
  #T_score = T_score_male
  #T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
  # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
  return(T_score)
}

##############################################################################################  
#Gets ASR Somatic scale T-score based on participant gender and total score 
##############################################################################################  

ASR_SOM_TSCORE <- function(gender, total_score) {
  
  T_score_male = ifelse(gender == 1 & total_score == 0, 50,
            ifelse(gender == 1 & total_score == 1, 51,
            ifelse(gender == 1 & total_score == 2, 52,
            ifelse(gender == 1 & total_score == 3, 55,
            ifelse(gender == 1 & total_score == 4, 58, 
            ifelse(gender == 1 & total_score == 5, 60, 
            ifelse(gender == 1 & total_score == 6, 62, 
            ifelse(gender == 1 & total_score == 7, 64, 
            ifelse(gender == 1 & total_score == 8, 66, 
            ifelse(gender == 1 & total_score == 9, 69, 
            ifelse(gender == 1 & total_score == 10, 70, 
            ifelse(gender == 1 & total_score == 11, 72, 
            ifelse(gender == 1 & total_score == 12, 74, 
            ifelse(gender == 1 & total_score == 13, 76, 
            ifelse(gender == 1 & total_score == 14, 79, 
            ifelse(gender == 1 & total_score == 15, 81, 
            ifelse(gender == 1 & total_score == 16, 83, 
            ifelse(gender == 1 & total_score == 17, 85, 
            ifelse(gender == 1 & total_score == 18, 87, 
            ifelse(gender == 1 & total_score == 19, 89, 
            ifelse(gender == 1 & total_score == 20, 91, 
            ifelse(gender == 1 & total_score == 21, 94, 
            ifelse(gender == 1 & total_score == 22, 96, 
            ifelse(gender == 1 & total_score == 23, 98,
            ifelse(gender == 1 & total_score == 24, 100, NA))))))))))))))))))))))))) 

  T_score_female = ifelse(gender == 2 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 2 & total_score == 2, 51,
            ifelse(gender == 2 & total_score == 3, 54,
            ifelse(gender == 2 & total_score == 4, 56,
            ifelse(gender == 2 & total_score == 5, 58, 
            ifelse(gender == 2 & total_score == 6, 60, 
            ifelse(gender == 2 & total_score == 7, 62, 
            ifelse(gender == 2 & total_score == 8, 65, 
            ifelse(gender == 2 & total_score == 9, 67, 
            ifelse(gender == 2 & total_score == 10, 69, 
            ifelse(gender == 2 & total_score == 11, 70, 
            ifelse(gender == 2 & total_score == 12, 72, 
            ifelse(gender == 2 & total_score == 13, 75, 
            ifelse(gender == 2 & total_score == 14, 77, 
            ifelse(gender == 2 & total_score == 15, 79, 
            ifelse(gender == 2 & total_score == 16, 82, 
            ifelse(gender == 2 & total_score == 17, 84, 
            ifelse(gender == 2 & total_score == 18, 86, 
            ifelse(gender == 2 & total_score == 19, 88, 
            ifelse(gender == 2 & total_score == 20, 91, 
            ifelse(gender == 2 & total_score == 21, 93, 
            ifelse(gender == 2 & total_score == 22, 95, 
            ifelse(gender == 2 & total_score == 23, 98,
            ifelse(gender == 2 & total_score == 24, 100, NA))))))))))))))))))))))))
  
  T_score = T_score_male
  T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
  # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
  return(T_score)
}

##############################################################################################  
#Gets ASR Thought scale T-score based on participant gender and total score 
##############################################################################################  

ASR_THOUGHT_PROB_TSCORE <- function(gender, total_score) {
  
  T_score_male = ifelse(gender == 1 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 1 & total_score == 2, 52,
            ifelse(gender == 1 & total_score == 3, 55,
            ifelse(gender == 1 & total_score == 4, 59, 
            ifelse(gender == 1 & total_score == 5, 62, 
            ifelse(gender == 1 & total_score == 6, 65, 
            ifelse(gender == 1 & total_score == 7, 68, 
            ifelse(gender == 1 & total_score == 8, 70, 
            ifelse(gender == 1 & total_score == 9, 73, 
            ifelse(gender == 1 & total_score == 10, 75, 
            ifelse(gender == 1 & total_score == 11, 78, 
            ifelse(gender == 1 & total_score == 12, 80, 
            ifelse(gender == 1 & total_score == 13, 83, 
            ifelse(gender == 1 & total_score == 14, 85, 
            ifelse(gender == 1 & total_score == 15, 88, 
            ifelse(gender == 1 & total_score == 16, 90, 
            ifelse(gender == 1 & total_score == 17, 93, 
            ifelse(gender == 1 & total_score == 18, 95, 
            ifelse(gender == 1 & total_score == 19, 98, 
            ifelse(gender == 1 & total_score == 20, 100, NA))))))))))))))))))))   

  T_score_female = ifelse(gender == 2 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 2 & total_score == 2, 51,
            ifelse(gender == 2 & total_score == 3, 55,
            ifelse(gender == 2 & total_score == 4, 58,
            ifelse(gender == 2 & total_score == 5, 62, 
            ifelse(gender == 2 & total_score == 6, 65, 
            ifelse(gender == 2 & total_score == 7, 68, 
            ifelse(gender == 2 & total_score == 8, 70, 
            ifelse(gender == 2 & total_score == 9, 73, 
            ifelse(gender == 2 & total_score == 10, 75, 
            ifelse(gender == 2 & total_score == 11, 78, 
            ifelse(gender == 2 & total_score == 12, 80, 
            ifelse(gender == 2 & total_score == 13, 83, 
            ifelse(gender == 2 & total_score == 14, 85, 
            ifelse(gender == 2 & total_score == 15, 88, 
            ifelse(gender == 2 & total_score == 16, 90, 
            ifelse(gender == 2 & total_score == 17, 93, 
            ifelse(gender == 2 & total_score == 18, 95, 
            ifelse(gender == 2 & total_score == 19, 98, 
            ifelse(gender == 2 & total_score == 20, 100, NA))))))))))))))))))))
  
  T_score = T_score_male
  T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
  # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
  return(T_score)
}

##############################################################################################  
#Gets ASR Attention Problems scale T-score based on participant gender and total score 
##############################################################################################  

ASR_ATTN_PROB_TSCORE <- function(gender, total_score) {
  
  T_score_male = ifelse(gender == 1 & total_score >= 0 & total_score <= 2, 50,
            ifelse(gender == 1 & total_score == 3, 51,
            ifelse(gender == 1 & total_score == 4, 52, 
            ifelse(gender == 1 & total_score == 5, 54, 
            ifelse(gender == 1 & total_score == 6, 55, 
            ifelse(gender == 1 & total_score == 7, 56, 
            ifelse(gender == 1 & total_score == 8, 57, 
            ifelse(gender == 1 & total_score == 9, 58, 
            ifelse(gender == 1 & total_score == 10, 59, 
            ifelse(gender == 1 & total_score == 11, 60, 
            ifelse(gender == 1 & total_score == 12, 61, 
            ifelse(gender == 1 & total_score == 13, 63, 
            ifelse(gender == 1 & total_score == 14, 64, 
            ifelse(gender == 1 & total_score == 15, 66, 
            ifelse(gender == 1 & total_score == 16, 67, 
            ifelse(gender == 1 & total_score == 17, 68, 
            ifelse(gender == 1 & total_score == 18, 69, 
            ifelse(gender == 1 & total_score == 19, 70, 
            ifelse(gender == 1 & total_score == 20, 73, 
            ifelse(gender == 1 & total_score == 21, 75, 
            ifelse(gender == 1 & total_score == 22, 78, 
            ifelse(gender == 1 & total_score == 23, 81,
            ifelse(gender == 1 & total_score == 24, 84,
            ifelse(gender == 1 & total_score == 25, 86,
            ifelse(gender == 1 & total_score == 26, 89,
            ifelse(gender == 1 & total_score == 27, 92,
            ifelse(gender == 1 & total_score == 28, 95,
            ifelse(gender == 1 & total_score == 29, 97,
            ifelse(gender == 1 & total_score == 30, 100, NA))))))))))))))))))))))))))))) 

  T_score_female = ifelse(gender == 2 & total_score >= 0 & total_score <= 4, 50,
            ifelse(gender == 2 & total_score == 5, 51, 
            ifelse(gender == 2 & total_score == 6, 53, 
            ifelse(gender == 2 & total_score == 7, 56, 
            ifelse(gender == 2 & total_score == 8, 57, 
            ifelse(gender == 2 & total_score == 9, 58, 
            ifelse(gender == 2 & total_score == 10, 59, 
            ifelse(gender == 2 & total_score == 11, 60, 
            ifelse(gender == 2 & total_score == 12, 61, 
            ifelse(gender == 2 & total_score == 13, 63, 
            ifelse(gender == 2 & total_score == 14, 66, 
            ifelse(gender == 2 & total_score == 15, 69, 
            ifelse(gender == 2 & total_score == 16, 70, 
            ifelse(gender == 2 & total_score == 17, 72, 
            ifelse(gender == 2 & total_score == 18, 74, 
            ifelse(gender == 2 & total_score == 19, 76, 
            ifelse(gender == 2 & total_score == 20, 79, 
            ifelse(gender == 2 & total_score == 21, 81, 
            ifelse(gender == 2 & total_score == 22, 83, 
            ifelse(gender == 2 & total_score == 23, 85,
            ifelse(gender == 2 & total_score == 24, 87,
            ifelse(gender == 2 & total_score == 25, 89,
            ifelse(gender == 2 & total_score == 26, 91,
            ifelse(gender == 2 & total_score == 27, 94,
            ifelse(gender == 2 & total_score == 28, 96,
            ifelse(gender == 2 & total_score == 29, 98,
            ifelse(gender == 2 & total_score == 30, 100, NA)))))))))))))))))))))))))))
  
  T_score = T_score_male
  T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
  # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
  return(T_score)
}


##############################################################################################  
#Gets ASR Aggressive Behavior T-score based on participant gender and total score 
##############################################################################################  

ASR_AGGRESSIVE_TSCORE <- function(gender, total_score) {
  
  T_score_male = ifelse(gender == 1 & total_score >= 0 & total_score <= 2, 50,
            ifelse(gender == 1 & total_score == 3, 51,
            ifelse(gender == 1 & total_score == 4, 52, 
            ifelse(gender == 1 & total_score == 5, 53, 
            ifelse(gender == 1 & total_score == 6, 54, 
            ifelse(gender == 1 & total_score == 7, 57, 
            ifelse(gender == 1 & total_score == 8, 59, 
            ifelse(gender == 1 & total_score == 9, 60, 
            ifelse(gender == 1 & total_score == 10, 61, 
            ifelse(gender == 1 & total_score == 11, 62, 
            ifelse(gender == 1 & total_score == 12, 63, 
            ifelse(gender == 1 & total_score == 13, 64, 
            ifelse(gender == 1 & total_score == 14, 65, 
            ifelse(gender == 1 & total_score == 15, 67, 
            ifelse(gender == 1 & total_score == 16, 68, 
            ifelse(gender == 1 & total_score == 17, 69, 
            ifelse(gender == 1 & total_score == 18, 70, 
            ifelse(gender == 1 & total_score == 19, 73, 
            ifelse(gender == 1 & total_score == 20, 75, 
            ifelse(gender == 1 & total_score == 21, 78, 
            ifelse(gender == 1 & total_score == 22, 80, 
            ifelse(gender == 1 & total_score == 23, 83,
            ifelse(gender == 1 & total_score == 24, 85,
            ifelse(gender == 1 & total_score == 25, 88,
            ifelse(gender == 1 & total_score == 26, 90,
            ifelse(gender == 1 & total_score == 27, 93,
            ifelse(gender == 1 & total_score == 28, 95,
            ifelse(gender == 1 & total_score == 29, 98,
            ifelse(gender == 1 & total_score == 30, 100, NA)))))))))))))))))))))))))))))  
                                                                                                                                                                                                                       
  T_score_female = ifelse(gender == 2 & total_score >= 0 & total_score <= 2, 50,
            ifelse(gender == 2 & total_score == 3, 51,
            ifelse(gender == 2 & total_score == 4, 52,
            ifelse(gender == 2 & total_score == 5, 53, 
            ifelse(gender == 2 & total_score == 6, 54, 
            ifelse(gender == 2 & total_score == 7, 55, 
            ifelse(gender == 2 & total_score == 8, 58, 
            ifelse(gender == 2 & total_score == 9, 60, 
            ifelse(gender == 2 & total_score == 10, 61, 
            ifelse(gender == 2 & total_score == 11, 62, 
            ifelse(gender == 2 & total_score == 12, 63, 
            ifelse(gender == 2 & total_score == 13, 65, 
            ifelse(gender == 2 & total_score == 14, 66, 
            ifelse(gender == 2 & total_score == 15, 67, 
            ifelse(gender == 2 & total_score == 16, 68, 
            ifelse(gender == 2 & total_score == 17, 69, 
            ifelse(gender == 2 & total_score == 18, 70, 
            ifelse(gender == 2 & total_score == 19, 73, 
            ifelse(gender == 2 & total_score == 20, 75, 
            ifelse(gender == 2 & total_score == 21, 78, 
            ifelse(gender == 2 & total_score == 22, 80, 
            ifelse(gender == 2 & total_score == 23, 83,
            ifelse(gender == 2 & total_score == 24, 85,
            ifelse(gender == 2 & total_score == 25, 88,
            ifelse(gender == 2 & total_score == 26, 90,
            ifelse(gender == 2 & total_score == 27, 93,
            ifelse(gender == 2 & total_score == 28, 95,
            ifelse(gender == 2 & total_score == 29, 98,
            ifelse(gender == 2 & total_score == 30, 100, NA)))))))))))))))))))))))))))))
  
  T_score = T_score_male
  T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
  # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
  return(T_score)
}


##############################################################################################  
#Gets ASR Rule-Breaking Behavior T-score based on participant gender and total score 
##############################################################################################  

ASR_RULE_BREAKING_TSCORE <- function(gender, total_score) { 
  
  T_score_male = ifelse(gender == 1 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 1 & total_score == 2, 51,
            ifelse(gender == 1 & total_score == 3, 54,
            ifelse(gender == 1 & total_score == 4, 56, 
            ifelse(gender == 1 & total_score == 5, 57, 
            ifelse(gender == 1 & total_score == 6, 59, 
            ifelse(gender == 1 & total_score == 7, 61, 
            ifelse(gender == 1 & total_score == 8, 64, 
            ifelse(gender == 1 & total_score == 9, 65, 
            ifelse(gender == 1 & total_score == 10, 67, 
            ifelse(gender == 1 & total_score == 11, 69, 
            ifelse(gender == 1 & total_score == 12, 70, 
            ifelse(gender == 1 & total_score == 13, 72, 
            ifelse(gender == 1 & total_score == 14, 74, 
            ifelse(gender == 1 & total_score == 15, 76, 
            ifelse(gender == 1 & total_score == 16, 78, 
            ifelse(gender == 1 & total_score == 17, 79, 
            ifelse(gender == 1 & total_score == 18, 81, 
            ifelse(gender == 1 & total_score == 19, 83, 
            ifelse(gender == 1 & total_score == 20, 85, 
            ifelse(gender == 1 & total_score == 21, 87, 
            ifelse(gender == 1 & total_score == 22, 89, 
            ifelse(gender == 1 & total_score == 23, 91,
            ifelse(gender == 1 & total_score == 24, 93,
            ifelse(gender == 1 & total_score == 25, 94,
            ifelse(gender == 1 & total_score == 26, 96,
            ifelse(gender == 1 & total_score == 27, 98,
            ifelse(gender == 1 & total_score == 28, 100, NA)))))))))))))))))))))))))))) 
                   
  T_score_female = ifelse(gender == 2 & total_score == 0, 50,       
            ifelse(gender == 2 & total_score == 1, 51, 
            ifelse(gender == 2 & total_score == 2, 52, 
            ifelse(gender == 2 & total_score == 3, 56,
            ifelse(gender == 2 & total_score == 4, 58,
            ifelse(gender == 2 & total_score == 5, 60, 
            ifelse(gender == 2 & total_score == 6, 62, 
            ifelse(gender == 2 & total_score == 7, 64, 
            ifelse(gender == 2 & total_score == 8, 65, 
            ifelse(gender == 2 & total_score == 9, 67, 
            ifelse(gender == 2 & total_score == 10, 70, 
            ifelse(gender == 2 & total_score == 11, 72, 
            ifelse(gender == 2 & total_score == 12, 73, 
            ifelse(gender == 2 & total_score == 13, 75, 
            ifelse(gender == 2 & total_score == 14, 77, 
            ifelse(gender == 2 & total_score == 15, 78, 
            ifelse(gender == 2 & total_score == 16, 80, 
            ifelse(gender == 2 & total_score == 17, 82, 
            ifelse(gender == 2 & total_score == 18, 83, 
            ifelse(gender == 2 & total_score == 19, 85, 
            ifelse(gender == 2 & total_score == 20, 87, 
            ifelse(gender == 2 & total_score == 21, 88, 
            ifelse(gender == 2 & total_score == 22, 90, 
            ifelse(gender == 2 & total_score == 23, 92,
            ifelse(gender == 2 & total_score == 24, 93,
            ifelse(gender == 2 & total_score == 25, 95,
            ifelse(gender == 2 & total_score == 26, 97,
            ifelse(gender == 2 & total_score == 27, 98,
            ifelse(gender == 2 & total_score == 28, 100, NA)))))))))))))))))))))))))))))
  
  T_score = T_score_male
  T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
  # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
  return(T_score)
  }

##############################################################################################  
#Gets ASR Intrusive Behavior T-score based on participant gender and total score 
##############################################################################################  

ASR_INTRUSIVE_TSCORE <- function(gender, total_score) {
  
  T_score_male = ifelse(gender == 1 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 1 & total_score == 2, 51,
            ifelse(gender == 1 & total_score == 3, 53,
            ifelse(gender == 1 & total_score == 4, 56, 
            ifelse(gender == 1 & total_score == 5, 60, 
            ifelse(gender == 1 & total_score == 6, 63, 
            ifelse(gender == 1 & total_score == 7, 67, 
            ifelse(gender == 1 & total_score == 8, 70, 
            ifelse(gender == 1 & total_score == 9, 73, 
            ifelse(gender == 1 & total_score == 10, 75, 
            ifelse(gender == 1 & total_score == 11, 78, 
            ifelse(gender == 1 & total_score == 12, 80, NA))))))))))))
                                                                                       
  T_score_female = ifelse(gender == 2 & total_score >= 0 & total_score <= 1, 50,
            ifelse(gender == 2 & total_score == 2, 51,
            ifelse(gender == 2 & total_score == 3, 54,
            ifelse(gender == 2 & total_score == 4, 57,
            ifelse(gender == 2 & total_score == 5, 61, 
            ifelse(gender == 2 & total_score == 6, 65, 
            ifelse(gender == 2 & total_score == 7, 68, 
            ifelse(gender == 2 & total_score == 8, 70, 
            ifelse(gender == 2 & total_score == 9, 73, 
            ifelse(gender == 2 & total_score == 10, 75,
            ifelse(gender == 2 & total_score == 11, 78,
            ifelse(gender == 2 & total_score == 12, 80, NA))))))))))))
  
  T_score = T_score_male
  T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] # this and previous line merge the two variables that are storing male and female t-scores into one 
  # above two lines are used to avoid a really long series of ifelse statements that R cannot execute at once
  return(T_score)                                                          
}





###################################################################################################################################
#Gets CBCL Activities T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_ACT_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 20,
                      ifelse(gender == 1 & age == 1 & total_score == 0.5, 21, 
                      ifelse(gender == 1 & age == 1 & total_score >= 1 & total_score <= 1.5, 22,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 23,
                      ifelse(gender == 1 & age == 1 & total_score == 2.5, 24,
                      ifelse(gender == 1 & age == 1 & total_score >= 3 & total_score <= 3.5, 25,    
                      ifelse(gender == 1 & age == 1 & total_score == 4, 26,
                      ifelse(gender == 1 & age == 1 & total_score == 4.5, 27,
                      ifelse(gender == 1 & age == 1 & total_score >= 5 & total_score <= 5.5, 28,
                      ifelse(gender == 1 & age == 1 & total_score == 6, 29,
                      ifelse(gender == 1 & age == 1 & total_score == 6.5, 30,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 32,
                      ifelse(gender == 1 & age == 1 & total_score == 7.5, 33,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 35,
                      ifelse(gender == 1 & age == 1 & total_score == 8.5, 38,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 40,
                      ifelse(gender == 1 & age == 1 & total_score == 9.5, 42,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 43,
                      ifelse(gender == 1 & age == 1 & total_score == 10.5, 45,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 47,
                      ifelse(gender == 1 & age == 1 & total_score == 11.5, 49,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 53,
                      ifelse(gender == 1 & age == 1 & total_score == 12.5, 56,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 60,
                      ifelse(gender == 1 & age == 1 & total_score == 13.5, 63,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 64,
                      ifelse(gender == 1 & age == 1 & total_score >= 14.5 & total_score <= 15, 65, NA)))))))))))))))))))))))))))

T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 20, 
                       ifelse(gender == 1 & age == 2 & total_score == 0.5, 21,  
                       ifelse(gender == 1 & age == 2 & total_score == 1, 22,
                       ifelse(gender == 1 & age == 2 & total_score == 1.5, 23,
                       ifelse(gender == 1 & age == 2 & total_score == 2, 24,
                       ifelse(gender == 1 & age == 2 & total_score >= 2.5 & total_score <= 3, 25,
                       ifelse(gender == 1 & age == 2 & total_score == 3.5, 26,
                       ifelse(gender == 1 & age == 2 & total_score == 4, 27,
                       ifelse(gender == 1 & age == 2 & total_score == 4.5, 28,
                       ifelse(gender == 1 & age == 2 & total_score == 5, 29,
                       ifelse(gender == 1 & age == 2 & total_score == 5.5, 30,
                       ifelse(gender == 1 & age == 2 & total_score == 6, 31, 
                       ifelse(gender == 1 & age == 2 & total_score == 6.5, 33,
                       ifelse(gender == 1 & age == 2 & total_score == 7, 34,
                       ifelse(gender == 1 & age == 2 & total_score == 7.5, 36,
                       ifelse(gender == 1 & age == 2 & total_score == 8, 37,
                       ifelse(gender == 1 & age == 2 & total_score == 8.5, 39,
                       ifelse(gender == 1 & age == 2 & total_score == 9, 40,
                       ifelse(gender == 1 & age == 2 & total_score == 9.5, 42,
                       ifelse(gender == 1 & age == 2 & total_score == 10, 44,
                       ifelse(gender == 1 & age == 2 & total_score == 10.5, 45,
                       ifelse(gender == 1 & age == 2 & total_score == 11, 47,
                       ifelse(gender == 1 & age == 2 & total_score == 11.5, 49,
                       ifelse(gender == 1 & age == 2 & total_score == 12, 52,
                       ifelse(gender == 1 & age == 2 & total_score == 12.5, 55,
                       ifelse(gender == 1 & age == 2 & total_score == 13, 59,
                       ifelse(gender == 1 & age == 2 & total_score == 13.5, 63,
                       ifelse(gender == 1 & age == 2 & total_score == 14, 64,
                       ifelse(gender == 1 & age == 2 & total_score >= 14.5 & total_score <= 15, 65, NA)))))))))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores 


T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 20, 
                      ifelse(gender == 2 & age == 1 & total_score == .5, 21, 
                      ifelse(gender == 2 & age == 1 & total_score == 1, 22, 
                      ifelse(gender == 2 & age == 1 & total_score == 1.5, 23, 
                      ifelse(gender == 2 & age == 1 & total_score == 2, 24, 
                      ifelse(gender == 2 & age == 1 & total_score == 2.5, 25, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 26, 
                      ifelse(gender == 2 & age == 1 & total_score == 3.5, 27, 
                      ifelse(gender == 2 & age == 1 & total_score == 4, 28, 
                      ifelse(gender == 2 & age == 1 & total_score == 4.5, 29, 
                      ifelse(gender == 2 & age == 1 & total_score == 5, 30, 
                      ifelse(gender == 2 & age == 1 & total_score == 5.5, 31, 
                      ifelse(gender == 2 & age == 1 & total_score == 6, 32, 
                      ifelse(gender == 2 & age == 1 & total_score == 6.5, 33, 
                      ifelse(gender == 2 & age == 1 & total_score == 7, 34, 
                      ifelse(gender == 2 & age == 1 & total_score == 7.5, 36, 
                      ifelse(gender == 2 & age == 1 & total_score == 8, 37, 
                      ifelse(gender == 2 & age == 1 & total_score == 8.5, 39, 
                      ifelse(gender == 2 & age == 1 & total_score == 9, 40, 
                      ifelse(gender == 2 & age == 1 & total_score == 9.5, 42, 
                      ifelse(gender == 2 & age == 1 & total_score == 10, 43, 
                      ifelse(gender == 2 & age == 1 & total_score == 10.5, 45, 
                      ifelse(gender == 2 & age == 1 & total_score == 11, 47, 
                      ifelse(gender == 2 & age == 1 & total_score == 11.5, 49, 
                      ifelse(gender == 2 & age == 1 & total_score == 12, 52, 
                      ifelse(gender == 2 & age == 1 & total_score == 12.5, 56, 
                      ifelse(gender == 2 & age == 1 & total_score == 13, 60, 
                      ifelse(gender == 2 & age == 1 & total_score == 13.5, 63, 
                      ifelse(gender == 2 & age == 1 & total_score == 14, 64, 
                      ifelse(gender == 2 & age == 1 & total_score >= 14.5 & total_score <= 15, 65, NA))))))))))))))))))))))))))))))

T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 20, 
                      ifelse(gender == 2 & age == 2 & total_score == .5, 21,
                      ifelse(gender == 2 & age == 2 & total_score == 1, 22, 
                      ifelse(gender == 2 & age == 2 & total_score == 1.5, 23, 
                      ifelse(gender == 2 & age == 2 & total_score == 2, 24, 
                      ifelse(gender == 2 & age == 2 & total_score >= 2.5 & total_score <= 3, 25, 
                      ifelse(gender == 2 & age == 2 & total_score == 3.5, 26, 
                      ifelse(gender == 2 & age == 2 & total_score == 4, 27, 
                      ifelse(gender == 2 & age == 2 & total_score == 4.5, 30, 
                      ifelse(gender == 2 & age == 2 & total_score == 5, 31, 
                      ifelse(gender == 2 & age == 2 & total_score == 5.5, 32, 
                      ifelse(gender == 2 & age == 2 & total_score == 6, 33, 
                      ifelse(gender == 2 & age == 2 & total_score == 6.5, 35, 
                      ifelse(gender == 2 & age == 2 & total_score == 7, 37, 
                      ifelse(gender == 2 & age == 2 & total_score == 7.5, 39, 
                      ifelse(gender == 2 & age == 2 & total_score == 8, 40, 
                      ifelse(gender == 2 & age == 2 & total_score == 8.5, 41, 
                      ifelse(gender == 2 & age == 2 & total_score == 9, 43, 
                      ifelse(gender == 2 & age == 2 & total_score == 9.5, 44, 
                      ifelse(gender == 2 & age == 2 & total_score == 10, 46, 
                      ifelse(gender == 2 & age == 2 & total_score == 10.5, 47, 
                      ifelse(gender == 2 & age == 2 & total_score == 11, 48, 
                      ifelse(gender == 2 & age == 2 & total_score == 11.5, 51, 
                      ifelse(gender == 2 & age == 2 & total_score == 12, 54, 
                      ifelse(gender == 2 & age == 2 & total_score == 12.5, 57, 
                      ifelse(gender == 2 & age == 2 & total_score == 13, 61, 
                      ifelse(gender == 2 & age == 2 & total_score == 13.5, 64, 
                      ifelse(gender == 2 & age == 2 & total_score >= 14 & total_score <= 15, 65, NA))))))))))))))))))))))))))))

T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
}
 
 

###################################################################################################################################
#Gets CBCL Social T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_SOC_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 20,
                      ifelse(gender == 1 & age == 1 & total_score == 0.5, 21, 
                      ifelse(gender == 1 & age == 1 & total_score == 1, 23,
                      ifelse(gender == 1 & age == 1 & total_score == 1.5, 24,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 25,    
                      ifelse(gender == 1 & age == 1 & total_score == 2.5, 26,
                      ifelse(gender == 1 & age == 1 & total_score == 3, 28,
                      ifelse(gender == 1 & age == 1 & total_score == 3.5, 29,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 32,
                      ifelse(gender == 1 & age == 1 & total_score == 4.5, 34,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 37,
                      ifelse(gender == 1 & age == 1 & total_score == 5.5, 39,
                      ifelse(gender == 1 & age == 1 & total_score == 6, 41,
                      ifelse(gender == 1 & age == 1 & total_score == 6.5, 43,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 45,
                      ifelse(gender == 1 & age == 1 & total_score == 7.5, 46,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 48,
                      ifelse(gender == 1 & age == 1 & total_score == 8.5, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 51,
                      ifelse(gender == 1 & age == 1 & total_score == 9.5, 53,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 55,
                      ifelse(gender == 1 & age == 1 & total_score == 10.5, 57,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 59,
                      ifelse(gender == 1 & age == 1 & total_score == 11.5, 62,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 64,
                      ifelse(gender == 1 & age == 1 & total_score >= 12.5 & total_score <= 14, 65, NA))))))))))))))))))))))))))

T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 20,
                      ifelse(gender == 1 & age == 2 & total_score == 0.5, 21, 
                      ifelse(gender == 1 & age == 2 & total_score == 1, 23,
                      ifelse(gender == 1 & age == 2 & total_score == 1.5, 24,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 25,    
                      ifelse(gender == 1 & age == 2 & total_score == 2.5, 26,
                      ifelse(gender == 1 & age == 2 & total_score == 3, 28,
                      ifelse(gender == 1 & age == 2 & total_score == 3.5, 29,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 31,
                      ifelse(gender == 1 & age == 2 & total_score == 4.5, 33,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 35,
                      ifelse(gender == 1 & age == 2 & total_score == 5.5, 37,
                      ifelse(gender == 1 & age == 2 & total_score == 6, 39,
                      ifelse(gender == 1 & age == 2 & total_score == 6.5, 41,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 43,
                      ifelse(gender == 1 & age == 2 & total_score == 7.5, 45,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 47,
                      ifelse(gender == 1 & age == 2 & total_score == 8.5, 48,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 9.5, 52,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 53,
                      ifelse(gender == 1 & age == 2 & total_score == 10.5, 55,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 56,
                      ifelse(gender == 1 & age == 2 & total_score == 11.5, 59,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 62,
                      ifelse(gender == 1 & age == 2 & total_score == 12.5, 64,
                      ifelse(gender == 1 & age == 2 & total_score >= 13 & total_score <= 14, 65, NA)))))))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores 

T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 20, 
                      ifelse(gender == 2 & age == 1 & total_score == .5, 21, 
                      ifelse(gender == 2 & age == 1 & total_score == 1, 23, 
                      ifelse(gender == 2 & age == 1 & total_score == 1.5, 24, 
                      ifelse(gender == 2 & age == 1 & total_score == 2, 25, 
                      ifelse(gender == 2 & age == 1 & total_score == 2.5, 26, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 28, 
                      ifelse(gender == 2 & age == 1 & total_score == 3.5, 29, 
                      ifelse(gender == 2 & age == 1 & total_score == 4, 32, 
                      ifelse(gender == 2 & age == 1 & total_score == 4.5, 33, 
                      ifelse(gender == 2 & age == 1 & total_score == 5, 35, 
                      ifelse(gender == 2 & age == 1 & total_score == 5.5, 38, 
                      ifelse(gender == 2 & age == 1 & total_score == 6, 40, 
                      ifelse(gender == 2 & age == 1 & total_score == 6.5, 42, 
                      ifelse(gender == 2 & age == 1 & total_score == 7, 44, 
                      ifelse(gender == 2 & age == 1 & total_score == 7.5, 46, 
                      ifelse(gender == 2 & age == 1 & total_score == 8, 48, 
                      ifelse(gender == 2 & age == 1 & total_score == 8.5, 50, 
                      ifelse(gender == 2 & age == 1 & total_score == 9, 52, 
                      ifelse(gender == 2 & age == 1 & total_score == 9.5, 53, 
                      ifelse(gender == 2 & age == 1 & total_score == 10, 54, 
                      ifelse(gender == 2 & age == 1 & total_score == 10.5, 56, 
                      ifelse(gender == 2 & age == 1 & total_score == 11, 58, 
                      ifelse(gender == 2 & age == 1 & total_score == 11.5, 60, 
                      ifelse(gender == 2 & age == 1 & total_score == 12, 62, 
                      ifelse(gender == 2 & age == 1 & total_score == 12.5, 64,  
                      ifelse(gender == 2 & age == 1 & total_score >= 13 & total_score <= 14, 65, NA)))))))))))))))))))))))))))

T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 20, 
                      ifelse(gender == 2 & age == 2 & total_score == .5, 21, 
                      ifelse(gender == 2 & age == 2 & total_score == 1, 23, 
                      ifelse(gender == 2 & age == 2 & total_score == 1.5, 24, 
                      ifelse(gender == 2 & age == 2 & total_score == 2, 25, 
                      ifelse(gender == 2 & age == 2 & total_score == 2.5, 26, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 28, 
                      ifelse(gender == 2 & age == 2 & total_score == 3.5, 29, 
                      ifelse(gender == 2 & age == 2 & total_score == 4, 30, 
                      ifelse(gender == 2 & age == 2 & total_score == 4.5, 32, 
                      ifelse(gender == 2 & age == 2 & total_score == 5, 35, 
                      ifelse(gender == 2 & age == 2 & total_score == 5.5, 37, 
                      ifelse(gender == 2 & age == 2 & total_score == 6, 39, 
                      ifelse(gender == 2 & age == 2 & total_score == 6.5, 41, 
                      ifelse(gender == 2 & age == 2 & total_score == 7, 43, 
                      ifelse(gender == 2 & age == 2 & total_score == 7.5, 45, 
                      ifelse(gender == 2 & age == 2 & total_score == 8, 47, 
                      ifelse(gender == 2 & age == 2 & total_score == 8.5, 49, 
                      ifelse(gender == 2 & age == 2 & total_score == 9, 51, 
                      ifelse(gender == 2 & age == 2 & total_score == 9.5, 52, 
                      ifelse(gender == 2 & age == 2 & total_score == 10, 54, 
                      ifelse(gender == 2 & age == 2 & total_score == 10.5, 55, 
                      ifelse(gender == 2 & age == 2 & total_score == 11, 57, 
                      ifelse(gender == 2 & age == 2 & total_score == 11.5, 59, 
                      ifelse(gender == 2 & age == 2 & total_score == 12, 62, 
                      ifelse(gender == 2 & age == 2 & total_score == 12.5, 63,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 64,  
                      ifelse(gender == 2 & age == 2 & total_score >= 13.5 & total_score <= 14, 65, NA))))))))))))))))))))))))))))

T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
} 

###################################################################################################################################
#Gets CBCL School T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_SCH_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 20,
                      ifelse(gender == 1 & age == 1 & total_score == 0.5, 22, 
                      ifelse(gender == 1 & age == 1 & total_score == 1, 24,
                      ifelse(gender == 1 & age == 1 & total_score == 1.5, 25,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 27,    
                      ifelse(gender == 1 & age == 1 & total_score == 2.5, 29,
                      ifelse(gender == 1 & age == 1 & total_score == 3, 33,
                      ifelse(gender == 1 & age == 1 & total_score == 3.5, 36,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 40,
                      ifelse(gender == 1 & age == 1 & total_score == 4.5, 43,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 48,
                      ifelse(gender == 1 & age == 1 & total_score == 5.5, 53,
                      ifelse(gender == 1 & age == 1 & total_score == 6, 55, NA)))))))))))))

T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 20,
                      ifelse(gender == 1 & age == 2 & total_score == 0.5, 23, 
                      ifelse(gender == 1 & age == 2 & total_score == 1, 26,
                      ifelse(gender == 1 & age == 2 & total_score == 1.5, 29,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 30,    
                      ifelse(gender == 1 & age == 2 & total_score == 2.5, 33,
                      ifelse(gender == 1 & age == 2 & total_score == 3, 37,
                      ifelse(gender == 1 & age == 2 & total_score == 3.5, 39,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 43,
                      ifelse(gender == 1 & age == 2 & total_score == 4.5, 46,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 5.5, 54,
                      ifelse(gender == 1 & age == 2 & total_score == 6, 55, NA)))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores 

T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 20, 
                      ifelse(gender == 2 & age == 1 & total_score == .5, 22, 
                      ifelse(gender == 2 & age == 1 & total_score == 1, 24, 
                      ifelse(gender == 2 & age == 1 & total_score == 1.5, 25, 
                      ifelse(gender == 2 & age == 1 & total_score == 2, 27, 
                      ifelse(gender == 2 & age == 1 & total_score == 2.5, 29, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 32, 
                      ifelse(gender == 2 & age == 1 & total_score == 3.5, 34, 
                      ifelse(gender == 2 & age == 1 & total_score == 4, 38, 
                      ifelse(gender == 2 & age == 1 & total_score == 4.5, 41, 
                      ifelse(gender == 2 & age == 1 & total_score == 5, 46, 
                      ifelse(gender == 2 & age == 1 & total_score == 5.5, 52, 
                      ifelse(gender == 2 & age == 1 & total_score == 6, 55, NA)))))))))))))

T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 20, 
                      ifelse(gender == 2 & age == 2 & total_score == .5, 23, 
                      ifelse(gender == 2 & age == 2 & total_score == 1, 26, 
                      ifelse(gender == 2 & age == 2 & total_score == 1.5, 29, 
                      ifelse(gender == 2 & age == 2 & total_score == 2, 30, 
                      ifelse(gender == 2 & age == 2 & total_score == 2.5, 33, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 37, 
                      ifelse(gender == 2 & age == 2 & total_score == 3.5, 39, 
                      ifelse(gender == 2 & age == 2 & total_score == 4, 43, 
                      ifelse(gender == 2 & age == 2 & total_score == 4.5, 46, 
                      ifelse(gender == 2 & age == 2 & total_score == 5, 50,  
                      ifelse(gender == 2 & age == 2 & total_score >= 5.5 & total_score <= 6, 55, NA))))))))))))

T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
}
 
 
###################################################################################################################################
#Gets CBCL Anxious/Depressed T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_ANX_DEP_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
  T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score >= 0 & total_score <= 1, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 51, 
                      ifelse(gender == 1 & age == 1 & total_score == 3, 53,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 57,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 59,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 62,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 64,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 66,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 67,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 69,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 70,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 72,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 74,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 76,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 78,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 80,
                      ifelse(gender == 1 & age == 1 & total_score == 17, 82,
                      ifelse(gender == 1 & age == 1 & total_score == 18, 84,
                      ifelse(gender == 1 & age == 1 & total_score == 19, 86,
                      ifelse(gender == 1 & age == 1 & total_score == 20, 88,
                      ifelse(gender == 1 & age == 1 & total_score == 21, 90,
                      ifelse(gender == 1 & age == 1 & total_score == 22, 92,
                      ifelse(gender == 1 & age == 1 & total_score == 23, 94,
                      ifelse(gender == 1 & age == 1 & total_score == 24, 96,
                      ifelse(gender == 1 & age == 1 & total_score == 25, 98,
                      ifelse(gender == 1 & age == 1 & total_score == 26, 100, NA))))))))))))))))))))))))))

 T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score >= 0 & total_score <= 1, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 51, 
                      ifelse(gender == 1 & age == 2 & total_score == 3, 54,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 57,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 60,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 63,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 65,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 66,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 68,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 70,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 72,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 74,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 76,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 78,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 79,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 81,
                      ifelse(gender == 1 & age == 2 & total_score == 17, 83,
                      ifelse(gender == 1 & age == 2 & total_score == 18, 85,
                      ifelse(gender == 1 & age == 2 & total_score == 19, 87,
                      ifelse(gender == 1 & age == 2 & total_score == 20, 89,
                      ifelse(gender == 1 & age == 2 & total_score == 21, 91,
                      ifelse(gender == 1 & age == 2 & total_score == 22, 93,
                      ifelse(gender == 1 & age == 2 & total_score == 23, 94,
                      ifelse(gender == 1 & age == 2 & total_score == 24, 96,
                      ifelse(gender == 1 & age == 2 & total_score == 25, 98,
                      ifelse(gender == 1 & age == 2 & total_score == 26, 100, NA))))))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores 

T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score >= 0 & total_score <= 1, 50,
                      ifelse(gender == 2 & age == 1 & total_score == 2, 51, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 52,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 54,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 57,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 60,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 63,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 65,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 66,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 68,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 70,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 72,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 74,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 76,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 78,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 80,
                      ifelse(gender == 2 & age == 1 & total_score == 17, 82,
                      ifelse(gender == 2 & age == 1 & total_score == 18, 84,
                      ifelse(gender == 2 & age == 1 & total_score == 19, 86,
                      ifelse(gender == 2 & age == 1 & total_score == 20, 88,
                      ifelse(gender == 2 & age == 1 & total_score == 21, 90,
                      ifelse(gender == 2 & age == 1 & total_score == 22, 92,
                      ifelse(gender == 2 & age == 1 & total_score == 23, 94,
                      ifelse(gender == 2 & age == 1 & total_score == 24, 96,
                      ifelse(gender == 2 & age == 1 & total_score == 25, 98,
                      ifelse(gender == 2 & age == 1 & total_score == 26, 100, NA))))))))))))))))))))))))))

T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score >= 0 & total_score <= 1, 50,
                      ifelse(gender == 2 & age == 2 & total_score == 2, 51, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 52,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 55,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 57,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 59,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 62,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 65,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 67,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 68,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 72,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 74,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 76,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 78,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 80,
                      ifelse(gender == 2 & age == 2 & total_score == 17, 82,
                      ifelse(gender == 2 & age == 2 & total_score == 18, 84,
                      ifelse(gender == 2 & age == 2 & total_score == 19, 86,
                      ifelse(gender == 2 & age == 2 & total_score == 20, 88,
                      ifelse(gender == 2 & age == 2 & total_score == 21, 90,
                      ifelse(gender == 2 & age == 2 & total_score == 22, 92,
                      ifelse(gender == 2 & age == 2 & total_score == 23, 94,
                      ifelse(gender == 2 & age == 2 & total_score == 24, 96,
                      ifelse(gender == 2 & age == 2 & total_score == 25, 98,
                      ifelse(gender == 2 & age == 2 & total_score == 26, 100, NA))))))))))))))))))))))))))

T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
} 

###################################################################################################################################
#Gets CBCL Withdrawn T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_WITHDRAWN_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
  T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 1, 54,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 58, 
                      ifelse(gender == 1 & age == 1 & total_score == 3, 62,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 66,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 68,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 70,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 73,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 76,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 79,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 82,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 85,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 88,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 91,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 94,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 97,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 100, NA)))))))))))))))))
                      
 T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 1, 53,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 54, 
                      ifelse(gender == 1 & age == 2 & total_score == 3, 57,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 60,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 63,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 66,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 68,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 70,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 74,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 78,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 81,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 85,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 89,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 93,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 96,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 100, NA)))))))))))))))))
                      
T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores 

T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 1 & total_score == 1, 52,
                      ifelse(gender == 2 & age == 1 & total_score == 2, 56, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 60,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 64,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 66,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 68,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 70,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 73,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 77,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 80,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 83,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 87,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 90,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 93,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 97,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 100, NA)))))))))))))))))
                      
T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 2 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 2 & total_score == 2, 54, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 57,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 60,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 63,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 66,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 69,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 74,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 78,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 81,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 85,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 89,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 93,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 96,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 100, NA)))))))))))))))))
 T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
} 

###################################################################################################################################
#Gets CBCL Somatic T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_SOM_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 1, 53,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 57, 
                      ifelse(gender == 1 & age == 1 & total_score == 3, 61,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 64,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 67,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 68,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 70,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 72,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 74,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 76,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 78,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 80,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 82,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 84,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 86,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 88,    
                      ifelse(gender == 1 & age == 1 & total_score == 17, 90,
                      ifelse(gender == 1 & age == 1 & total_score == 18, 92,
                      ifelse(gender == 1 & age == 1 & total_score == 19, 94,
                      ifelse(gender == 1 & age == 1 & total_score == 20, 96,
                      ifelse(gender == 1 & age == 1 & total_score == 21, 98,
                      ifelse(gender == 1 & age == 1 & total_score == 22, 100, NA)))))))))))))))))))))))
                      
T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 1, 54,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 58, 
                      ifelse(gender == 1 & age == 2 & total_score == 3, 61,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 64,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 67,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 70,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 72,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 74,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 76,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 78,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 79,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 81,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 83,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 85,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 87,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 89,    
                      ifelse(gender == 1 & age == 2 & total_score == 17, 91,
                      ifelse(gender == 1 & age == 2 & total_score == 18, 93,
                      ifelse(gender == 1 & age == 2 & total_score == 19, 94,
                      ifelse(gender == 1 & age == 2 & total_score == 20, 96,
                      ifelse(gender == 1 & age == 2 & total_score == 21, 98,
                      ifelse(gender == 1 & age == 2 & total_score == 22, 100, NA)))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores                       
              
T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 1 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 1 & total_score == 2, 56, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 60,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 64,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 66,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 68,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 70,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 72,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 74,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 76,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 78,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 80,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 82,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 84,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 86,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 88,    
                      ifelse(gender == 2 & age == 1 & total_score == 17, 90,
                      ifelse(gender == 2 & age == 1 & total_score == 18, 92,
                      ifelse(gender == 2 & age == 1 & total_score == 19, 94,
                      ifelse(gender == 2 & age == 1 & total_score == 20, 96,
                      ifelse(gender == 2 & age == 1 & total_score == 21, 98,
                      ifelse(gender == 2 & age == 1 & total_score == 22, 100, NA)))))))))))))))))))))))    
                      
T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 2 & total_score == 1, 53,
                      ifelse(gender == 2 & age == 2 & total_score == 2, 56, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 59,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 62,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 65,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 68,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 72,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 74,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 76,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 78,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 80,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 82,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 84,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 86,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 88,    
                      ifelse(gender == 2 & age == 2 & total_score == 17, 90,
                      ifelse(gender == 2 & age == 2 & total_score == 18, 92,
                      ifelse(gender == 2 & age == 2 & total_score == 19, 94,
                      ifelse(gender == 2 & age == 2 & total_score == 20, 96,
                      ifelse(gender == 2 & age == 2 & total_score == 21, 98,
                      ifelse(gender == 2 & age == 2 & total_score == 22, 100, NA))))))))))))))))))))))) 
                                                   
T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
}       

###################################################################################################################################
#Gets CBCL Social Problems T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_SOC_PROB_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 1, 51,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 53, 
                      ifelse(gender == 1 & age == 1 & total_score == 3, 56,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 58,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 60,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 62,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 65,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 67,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 69,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 70,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 73,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 75,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 78,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 80,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 83,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 85,    
                      ifelse(gender == 1 & age == 1 & total_score == 17, 88,
                      ifelse(gender == 1 & age == 1 & total_score == 18, 90,
                      ifelse(gender == 1 & age == 1 & total_score == 19, 93,
                      ifelse(gender == 1 & age == 1 & total_score == 20, 95,
                      ifelse(gender == 1 & age == 1 & total_score == 21, 98,
                      ifelse(gender == 1 & age == 1 & total_score == 22, 100, NA)))))))))))))))))))))))
                      
T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 1, 51,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 54, 
                      ifelse(gender == 1 & age == 2 & total_score == 3, 58,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 61,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 63,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 66,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 67,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 69,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 70,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 72,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 75,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 77,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 79,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 82,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 84,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 86,    
                      ifelse(gender == 1 & age == 2 & total_score == 17, 88,
                      ifelse(gender == 1 & age == 2 & total_score == 18, 91,
                      ifelse(gender == 1 & age == 2 & total_score == 19, 93,
                      ifelse(gender == 1 & age == 2 & total_score == 20, 95,
                      ifelse(gender == 1 & age == 2 & total_score == 21, 98,
                      ifelse(gender == 1 & age == 2 & total_score == 22, 100, NA)))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores                       
              
T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 1 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 1 & total_score == 2, 52, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 54,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 57,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 59,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 62,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 64,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 67,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 70,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 72,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 75,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 77,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 79,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 82,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 84,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 86,    
                      ifelse(gender == 2 & age == 1 & total_score == 17, 88,
                      ifelse(gender == 2 & age == 1 & total_score == 18, 91,
                      ifelse(gender == 2 & age == 1 & total_score == 19, 93,
                      ifelse(gender == 2 & age == 1 & total_score == 20, 95,
                      ifelse(gender == 2 & age == 1 & total_score == 21, 98,
                      ifelse(gender == 2 & age == 1 & total_score == 22, 100, NA)))))))))))))))))))))))     
                      
T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 2 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 2 & total_score == 2, 54, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 58,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 61,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 64,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 66,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 68,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 69,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 72,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 75,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 77,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 79,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 82,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 84,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 86,    
                      ifelse(gender == 2 & age == 2 & total_score == 17, 88,
                      ifelse(gender == 2 & age == 2 & total_score == 18, 91,
                      ifelse(gender == 2 & age == 2 & total_score == 19, 93,
                      ifelse(gender == 2 & age == 2 & total_score == 20, 95,
                      ifelse(gender == 2 & age == 2 & total_score == 21, 98,
                      ifelse(gender == 2 & age == 2 & total_score == 22, 100, NA)))))))))))))))))))))))
                                                   
T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
}       

###################################################################################################################################
#Gets CBCL Thought Problems T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_THOUGHT_PROB_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 1, 51,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 54, 
                      ifelse(gender == 1 & age == 1 & total_score == 3, 58,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 61,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 64,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 67,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 70,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 71,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 73,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 74,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 75,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 77,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 78,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 79,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 80,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 82,    
                      ifelse(gender == 1 & age == 1 & total_score == 17, 83,
                      ifelse(gender == 1 & age == 1 & total_score == 18, 84,
                      ifelse(gender == 1 & age == 1 & total_score == 19, 86,
                      ifelse(gender == 1 & age == 1 & total_score == 20, 87,
                      ifelse(gender == 1 & age == 1 & total_score == 21, 88,
                      ifelse(gender == 1 & age == 1 & total_score == 22, 90,
                      ifelse(gender == 1 & age == 1 & total_score == 23, 91,
                      ifelse(gender == 1 & age == 1 & total_score == 24, 92,    
                      ifelse(gender == 1 & age == 1 & total_score == 25, 93,
                      ifelse(gender == 1 & age == 1 & total_score == 26, 95,
                      ifelse(gender == 1 & age == 1 & total_score == 27, 96,
                      ifelse(gender == 1 & age == 1 & total_score == 28, 97,
                      ifelse(gender == 1 & age == 1 & total_score == 29, 99,
                      ifelse(gender == 1 & age == 1 & total_score == 30, 100, NA)))))))))))))))))))))))))))))))
                      
T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 1, 51,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 55, 
                      ifelse(gender == 1 & age == 2 & total_score == 3, 59,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 63,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 66,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 67,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 69,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 70,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 71,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 73,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 74,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 75,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 77,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 78,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 80,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 81,    
                      ifelse(gender == 1 & age == 2 & total_score == 17, 82,
                      ifelse(gender == 1 & age == 2 & total_score == 18, 84,
                      ifelse(gender == 1 & age == 2 & total_score == 19, 85,
                      ifelse(gender == 1 & age == 2 & total_score == 20, 86,
                      ifelse(gender == 1 & age == 2 & total_score == 21, 88,
                      ifelse(gender == 1 & age == 2 & total_score == 22, 89,
                      ifelse(gender == 1 & age == 2 & total_score == 23, 90,
                      ifelse(gender == 1 & age == 2 & total_score == 24, 92,    
                      ifelse(gender == 1 & age == 2 & total_score == 25, 93,
                      ifelse(gender == 1 & age == 2 & total_score == 26, 95,
                      ifelse(gender == 1 & age == 2 & total_score == 27, 96,
                      ifelse(gender == 1 & age == 2 & total_score == 28, 97,
                      ifelse(gender == 1 & age == 2 & total_score == 29, 99,
                      ifelse(gender == 1 & age == 2 & total_score == 30, 100, NA)))))))))))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores                       
              
T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 1 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 1 & total_score == 2, 54, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 58,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 62,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 66,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 68,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 70,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 71,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 73,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 74,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 75,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 77,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 78,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 79,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 80,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 82,    
                      ifelse(gender == 2 & age == 1 & total_score == 17, 83,
                      ifelse(gender == 2 & age == 1 & total_score == 18, 84,
                      ifelse(gender == 2 & age == 1 & total_score == 19, 86,
                      ifelse(gender == 2 & age == 1 & total_score == 20, 87,
                      ifelse(gender == 2 & age == 1 & total_score == 21, 88,
                      ifelse(gender == 2 & age == 1 & total_score == 22, 90,
                      ifelse(gender == 2 & age == 1 & total_score == 23, 91,
                      ifelse(gender == 2 & age == 1 & total_score == 24, 92,    
                      ifelse(gender == 2 & age == 1 & total_score == 25, 93,
                      ifelse(gender == 2 & age == 1 & total_score == 26, 95,
                      ifelse(gender == 2 & age == 1 & total_score == 27, 96,
                      ifelse(gender == 2 & age == 1 & total_score == 28, 97,
                      ifelse(gender == 2 & age == 1 & total_score == 29, 99,
                      ifelse(gender == 2 & age == 1 & total_score == 30, 100, NA)))))))))))))))))))))))))))))))      
                      
T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 2 & total_score == 1, 52,
                      ifelse(gender == 2 & age == 2 & total_score == 2, 56, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 60,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 64,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 67,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 69,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 71,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 73,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 74,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 75,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 77,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 78,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 79,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 80,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 82,    
                      ifelse(gender == 2 & age == 2 & total_score == 17, 83,
                      ifelse(gender == 2 & age == 2 & total_score == 18, 84,
                      ifelse(gender == 2 & age == 2 & total_score == 19, 86,
                      ifelse(gender == 2 & age == 2 & total_score == 20, 87,
                      ifelse(gender == 2 & age == 2 & total_score == 21, 88,
                      ifelse(gender == 2 & age == 2 & total_score == 22, 90,
                      ifelse(gender == 2 & age == 2 & total_score == 23, 91,
                      ifelse(gender == 2 & age == 2 & total_score == 24, 92,    
                      ifelse(gender == 2 & age == 2 & total_score == 25, 93,
                      ifelse(gender == 2 & age == 2 & total_score == 26, 95,
                      ifelse(gender == 2 & age == 2 & total_score == 27, 96,
                      ifelse(gender == 2 & age == 2 & total_score == 28, 97,
                      ifelse(gender == 2 & age == 2 & total_score == 29, 99,
                      ifelse(gender == 2 & age == 2 & total_score == 30, 100, NA))))))))))))))))))))))))))))))) 
                                                   
T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
} 

###################################################################################################################################
#Gets CBCL Attention Problems T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_ATTN_PROB_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 =   ifelse(gender == 1 & age == 1 & total_score >= 0 & total_score <= 1, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 51,
                      ifelse(gender == 1 & age == 1 & total_score == 3, 52,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 53,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 55,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 57,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 59,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 61,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 64,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 66,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 67,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 69,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 71,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 75,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 79,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 83,    
                      ifelse(gender == 1 & age == 1 & total_score == 17, 88,
                      ifelse(gender == 1 & age == 1 & total_score == 18, 92,
                      ifelse(gender == 1 & age == 1 & total_score == 19, 96,
                      ifelse(gender == 1 & age == 1 & total_score == 20, 100, NA))))))))))))))))))))
                      
T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score >= 0 & total_score <= 1, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 51,
                      ifelse(gender == 1 & age == 2 & total_score == 3, 52,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 53,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 55,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 57,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 59,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 61,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 62,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 64,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 65,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 67,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 69,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 71,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 76,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 81,    
                      ifelse(gender == 1 & age == 2 & total_score == 17, 86,
                      ifelse(gender == 1 & age == 2 & total_score == 18, 90,
                      ifelse(gender == 1 & age == 2 & total_score == 19, 95,
                      ifelse(gender == 1 & age == 2 & total_score == 20, 100, NA))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores                       
              
T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 1 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 1 & total_score == 2, 52,
                      ifelse(gender == 2 & age == 1 & total_score == 3, 53,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 55,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 57,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 59,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 62,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 64,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 66,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 68,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 70,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 73,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 77,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 80,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 83,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 87,    
                      ifelse(gender == 2 & age == 1 & total_score == 17, 90,
                      ifelse(gender == 2 & age == 1 & total_score == 18, 93,
                      ifelse(gender == 2 & age == 1 & total_score == 19, 97,
                      ifelse(gender == 2 & age == 1 & total_score == 20, 100, NA)))))))))))))))))))))      
                      
T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 2 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 2 & total_score == 2, 52,
                      ifelse(gender == 2 & age == 2 & total_score == 3, 54,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 56,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 59,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 61,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 63,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 65,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 66,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 68,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 73,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 77,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 80,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 83,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 87,    
                      ifelse(gender == 2 & age == 2 & total_score == 17, 90,
                      ifelse(gender == 2 & age == 2 & total_score == 18, 93,
                      ifelse(gender == 2 & age == 2 & total_score == 19, 97,
                      ifelse(gender == 2 & age == 2 & total_score == 20, 100, NA))))))))))))))))))))) 
                                                   
T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
} 

###################################################################################################################################
#Gets CBCL Rule-Breaking Behavior T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_RULE_BREAKING_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 1 & total_score == 1, 51,
                      ifelse(gender == 1 & age == 1 & total_score == 2, 53, 
                      ifelse(gender == 1 & age == 1 & total_score == 3, 57,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 60,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 64,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 67,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 70,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 71,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 72,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 73,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 74,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 76,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 77,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 78,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 79,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 80,    
                      ifelse(gender == 1 & age == 1 & total_score == 17, 81,
                      ifelse(gender == 1 & age == 1 & total_score == 18, 82,
                      ifelse(gender == 1 & age == 1 & total_score == 19, 83,
                      ifelse(gender == 1 & age == 1 & total_score == 20, 84,
                      ifelse(gender == 1 & age == 1 & total_score == 21, 86,
                      ifelse(gender == 1 & age == 1 & total_score == 22, 87,
                      ifelse(gender == 1 & age == 1 & total_score == 23, 88,
                      ifelse(gender == 1 & age == 1 & total_score == 24, 89,    
                      ifelse(gender == 1 & age == 1 & total_score == 25, 90,
                      ifelse(gender == 1 & age == 1 & total_score == 26, 91,
                      ifelse(gender == 1 & age == 1 & total_score == 27, 92,
                      ifelse(gender == 1 & age == 1 & total_score == 28, 93,
                      ifelse(gender == 1 & age == 1 & total_score == 29, 94,
                      ifelse(gender == 1 & age == 1 & total_score == 30, 96,
                      ifelse(gender == 1 & age == 1 & total_score == 31, 97,
                      ifelse(gender == 1 & age == 1 & total_score == 32, 98,
                      ifelse(gender == 1 & age == 1 & total_score == 33, 99,
                      ifelse(gender == 1 & age == 1 & total_score == 34, 100, NA)))))))))))))))))))))))))))))))))))
                      
T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 1 & age == 2 & total_score == 1, 51,
                      ifelse(gender == 1 & age == 2 & total_score == 2, 52, 
                      ifelse(gender == 1 & age == 2 & total_score == 3, 54,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 57,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 60,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 62,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 63,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 64,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 66,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 67,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 68,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 69,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 70,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 71,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 73,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 74,    
                      ifelse(gender == 1 & age == 2 & total_score == 17, 76,
                      ifelse(gender == 1 & age == 2 & total_score == 18, 77,
                      ifelse(gender == 1 & age == 2 & total_score == 19, 79,
                      ifelse(gender == 1 & age == 2 & total_score == 20, 80,
                      ifelse(gender == 1 & age == 2 & total_score == 21, 81,
                      ifelse(gender == 1 & age == 2 & total_score == 22, 83,
                      ifelse(gender == 1 & age == 2 & total_score == 23, 84,
                      ifelse(gender == 1 & age == 2 & total_score == 24, 86,    
                      ifelse(gender == 1 & age == 2 & total_score == 25, 87,
                      ifelse(gender == 1 & age == 2 & total_score == 26, 89,
                      ifelse(gender == 1 & age == 2 & total_score == 27, 90,
                      ifelse(gender == 1 & age == 2 & total_score == 28, 91,
                      ifelse(gender == 1 & age == 2 & total_score == 29, 93,
                      ifelse(gender == 1 & age == 2 & total_score == 30, 94,
                      ifelse(gender == 1 & age == 2 & total_score == 31, 96,
                      ifelse(gender == 1 & age == 2 & total_score == 32, 97,
                      ifelse(gender == 1 & age == 2 & total_score == 33, 99,
                      ifelse(gender == 1 & age == 2 & total_score == 34, 100, NA)))))))))))))))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores                       
              
T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 1 & total_score == 1, 52,
                      ifelse(gender == 2 & age == 1 & total_score == 2, 55, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 59,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 63,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 66,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 68,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 70,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 71,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 72,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 73,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 74,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 76,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 77,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 78,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 79,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 80,    
                      ifelse(gender == 2 & age == 1 & total_score == 17, 81,
                      ifelse(gender == 2 & age == 1 & total_score == 18, 82,
                      ifelse(gender == 2 & age == 1 & total_score == 19, 83,
                      ifelse(gender == 2 & age == 1 & total_score == 20, 84,
                      ifelse(gender == 2 & age == 1 & total_score == 21, 86,
                      ifelse(gender == 2 & age == 1 & total_score == 22, 87,
                      ifelse(gender == 2 & age == 1 & total_score == 23, 88,
                      ifelse(gender == 2 & age == 1 & total_score == 24, 89,    
                      ifelse(gender == 2 & age == 1 & total_score == 25, 90,
                      ifelse(gender == 2 & age == 1 & total_score == 26, 91,
                      ifelse(gender == 2 & age == 1 & total_score == 27, 92,
                      ifelse(gender == 2 & age == 1 & total_score == 28, 93,
                      ifelse(gender == 2 & age == 1 & total_score == 29, 94,
                      ifelse(gender == 2 & age == 1 & total_score == 30, 96,    
                      ifelse(gender == 2 & age == 1 & total_score == 31, 97,
                      ifelse(gender == 2 & age == 1 & total_score == 32, 98,
                      ifelse(gender == 2 & age == 1 & total_score == 33, 99,
                      ifelse(gender == 2 & age == 1 & total_score == 34, 100, NA)))))))))))))))))))))))))))))))))))      
                      
T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score == 0, 50,
                      ifelse(gender == 2 & age == 2 & total_score == 1, 51,
                      ifelse(gender == 2 & age == 2 & total_score == 2, 54, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 57,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 60,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 62,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 64,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 65,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 67,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 68,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 69,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 71,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 73,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 74,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 75,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 77,    
                      ifelse(gender == 2 & age == 2 & total_score == 17, 78,
                      ifelse(gender == 2 & age == 2 & total_score == 18, 79,
                      ifelse(gender == 2 & age == 2 & total_score == 19, 80,
                      ifelse(gender == 2 & age == 2 & total_score == 20, 82,
                      ifelse(gender == 2 & age == 2 & total_score == 21, 83,
                      ifelse(gender == 2 & age == 2 & total_score == 22, 84,
                      ifelse(gender == 2 & age == 2 & total_score == 23, 86,
                      ifelse(gender == 2 & age == 2 & total_score == 24, 87,    
                      ifelse(gender == 2 & age == 2 & total_score == 25, 88,
                      ifelse(gender == 2 & age == 2 & total_score == 26, 90,
                      ifelse(gender == 2 & age == 2 & total_score == 27, 91,
                      ifelse(gender == 2 & age == 2 & total_score == 28, 92,
                      ifelse(gender == 2 & age == 2 & total_score == 29, 93,
                      ifelse(gender == 2 & age == 2 & total_score == 30, 95,    
                      ifelse(gender == 2 & age == 2 & total_score == 31, 96,
                      ifelse(gender == 2 & age == 2 & total_score == 32, 97,
                      ifelse(gender == 2 & age == 2 & total_score == 33, 99,
                      ifelse(gender == 2 & age == 2 & total_score == 34, 100, NA))))))))))))))))))))))))))))))))))) 
                                                   
T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
} 

###################################################################################################################################
#Gets CBCL Aggressive Behavior T-score based on gender, age, and total score 
###################################################################################################################################

CBCL_AGGRESSIVE_TSCORE <- function(gender, age, total_score) {

# Use total score to get t-score for each gender and age group
T_score_male_6_11 = ifelse(gender == 1 & age == 1 & total_score >= 0 & total_score <= 2, 50, 
                      ifelse(gender == 1 & age == 1 & total_score == 3, 51,
                      ifelse(gender == 1 & age == 1 & total_score == 4, 52,
                      ifelse(gender == 1 & age == 1 & total_score == 5, 53,    
                      ifelse(gender == 1 & age == 1 & total_score == 6, 55,
                      ifelse(gender == 1 & age == 1 & total_score == 7, 57,
                      ifelse(gender == 1 & age == 1 & total_score == 8, 59,
                      ifelse(gender == 1 & age == 1 & total_score == 9, 61,
                      ifelse(gender == 1 & age == 1 & total_score == 10, 62,
                      ifelse(gender == 1 & age == 1 & total_score == 11, 64,
                      ifelse(gender == 1 & age == 1 & total_score == 12, 65,
                      ifelse(gender == 1 & age == 1 & total_score == 13, 66,
                      ifelse(gender == 1 & age == 1 & total_score == 14, 67,
                      ifelse(gender == 1 & age == 1 & total_score == 15, 68,
                      ifelse(gender == 1 & age == 1 & total_score == 16, 69,    
                      ifelse(gender == 1 & age == 1 & total_score == 17, 70,
                      ifelse(gender == 1 & age == 1 & total_score == 18, 72,
                      ifelse(gender == 1 & age == 1 & total_score == 19, 73,
                      ifelse(gender == 1 & age == 1 & total_score == 20, 75,
                      ifelse(gender == 1 & age == 1 & total_score == 21, 76,
                      ifelse(gender == 1 & age == 1 & total_score == 22, 78,
                      ifelse(gender == 1 & age == 1 & total_score == 23, 79,
                      ifelse(gender == 1 & age == 1 & total_score == 24, 81,    
                      ifelse(gender == 1 & age == 1 & total_score == 25, 83,
                      ifelse(gender == 1 & age == 1 & total_score == 26, 84,
                      ifelse(gender == 1 & age == 1 & total_score == 27, 86,
                      ifelse(gender == 1 & age == 1 & total_score == 28, 87,
                      ifelse(gender == 1 & age == 1 & total_score == 29, 89,
                      ifelse(gender == 1 & age == 1 & total_score == 30, 91,
                      ifelse(gender == 1 & age == 1 & total_score == 31, 92,
                      ifelse(gender == 1 & age == 1 & total_score == 32, 94,
                      ifelse(gender == 1 & age == 1 & total_score == 33, 95,
                      ifelse(gender == 1 & age == 1 & total_score == 34, 97,
                      ifelse(gender == 1 & age == 1 & total_score == 35, 98,
                      ifelse(gender == 1 & age == 1 & total_score == 36, 100, NA)))))))))))))))))))))))))))))))))))
                      
T_score_male_12_17 = ifelse(gender == 1 & age == 2 & total_score >= 0 & total_score <= 2, 50, 
                      ifelse(gender == 1 & age == 2 & total_score == 3, 51,
                      ifelse(gender == 1 & age == 2 & total_score == 4, 52,
                      ifelse(gender == 1 & age == 2 & total_score == 5, 54,    
                      ifelse(gender == 1 & age == 2 & total_score == 6, 55,
                      ifelse(gender == 1 & age == 2 & total_score == 7, 57,
                      ifelse(gender == 1 & age == 2 & total_score == 8, 58,
                      ifelse(gender == 1 & age == 2 & total_score == 9, 60,
                      ifelse(gender == 1 & age == 2 & total_score == 10, 61,
                      ifelse(gender == 1 & age == 2 & total_score == 11, 63,
                      ifelse(gender == 1 & age == 2 & total_score == 12, 64,
                      ifelse(gender == 1 & age == 2 & total_score == 13, 65,
                      ifelse(gender == 1 & age == 2 & total_score == 14, 66,
                      ifelse(gender == 1 & age == 2 & total_score == 15, 68,
                      ifelse(gender == 1 & age == 2 & total_score == 16, 69,    
                      ifelse(gender == 1 & age == 2 & total_score == 17, 70,
                      ifelse(gender == 1 & age == 2 & total_score == 18, 72,
                      ifelse(gender == 1 & age == 2 & total_score == 19, 73,
                      ifelse(gender == 1 & age == 2 & total_score == 20, 75,
                      ifelse(gender == 1 & age == 2 & total_score == 21, 76,
                      ifelse(gender == 1 & age == 2 & total_score == 22, 78,
                      ifelse(gender == 1 & age == 2 & total_score == 23, 79,
                      ifelse(gender == 1 & age == 2 & total_score == 24, 81,    
                      ifelse(gender == 1 & age == 2 & total_score == 25, 83,
                      ifelse(gender == 1 & age == 2 & total_score == 26, 84,
                      ifelse(gender == 1 & age == 2 & total_score == 27, 86,
                      ifelse(gender == 1 & age == 2 & total_score == 28, 87,
                      ifelse(gender == 1 & age == 2 & total_score == 29, 89,
                      ifelse(gender == 1 & age == 2 & total_score == 30, 91,
                      ifelse(gender == 1 & age == 2 & total_score == 31, 92,
                      ifelse(gender == 1 & age == 2 & total_score == 32, 94,
                      ifelse(gender == 1 & age == 2 & total_score == 33, 95,
                      ifelse(gender == 1 & age == 2 & total_score == 34, 97,
                      ifelse(gender == 1 & age == 2 & total_score == 35, 98,
                      ifelse(gender == 1 & age == 2 & total_score == 36, 100, NA)))))))))))))))))))))))))))))))))))

T_score_male = T_score_male_6_11
T_score_male[!is.na(T_score_male_12_17)] = T_score_male_12_17[!is.na(T_score_male_12_17)]
# merges the two variables that are storing male t-scores                       
              
T_score_female_6_11 = ifelse(gender == 2 & age == 1 & total_score >= 0 & total_score <= 2, 50, 
                      ifelse(gender == 2 & age == 1 & total_score == 3, 51,
                      ifelse(gender == 2 & age == 1 & total_score == 4, 52,
                      ifelse(gender == 2 & age == 1 & total_score == 5, 54,    
                      ifelse(gender == 2 & age == 1 & total_score == 6, 56,
                      ifelse(gender == 2 & age == 1 & total_score == 7, 57,
                      ifelse(gender == 2 & age == 1 & total_score == 8, 59,
                      ifelse(gender == 2 & age == 1 & total_score == 9, 60,
                      ifelse(gender == 2 & age == 1 & total_score == 10, 61,
                      ifelse(gender == 2 & age == 1 & total_score == 11, 63,
                      ifelse(gender == 2 & age == 1 & total_score == 12, 65,
                      ifelse(gender == 2 & age == 1 & total_score == 13, 66,
                      ifelse(gender == 2 & age == 1 & total_score == 14, 68,
                      ifelse(gender == 2 & age == 1 & total_score == 15, 69,
                      ifelse(gender == 2 & age == 1 & total_score == 16, 70,    
                      ifelse(gender == 2 & age == 1 & total_score == 17, 72,
                      ifelse(gender == 2 & age == 1 & total_score == 18, 73,
                      ifelse(gender == 2 & age == 1 & total_score == 19, 75,
                      ifelse(gender == 2 & age == 1 & total_score == 20, 76,
                      ifelse(gender == 2 & age == 1 & total_score == 21, 78,
                      ifelse(gender == 2 & age == 1 & total_score == 22, 79,
                      ifelse(gender == 2 & age == 1 & total_score == 23, 81,
                      ifelse(gender == 2 & age == 1 & total_score == 24, 82,    
                      ifelse(gender == 2 & age == 1 & total_score == 25, 84,
                      ifelse(gender == 2 & age == 1 & total_score == 26, 85,
                      ifelse(gender == 2 & age == 1 & total_score == 27, 87,
                      ifelse(gender == 2 & age == 1 & total_score == 28, 88,
                      ifelse(gender == 2 & age == 1 & total_score == 29, 90,
                      ifelse(gender == 2 & age == 1 & total_score == 30, 91,
                      ifelse(gender == 2 & age == 1 & total_score == 31, 93,
                      ifelse(gender == 2 & age == 1 & total_score == 32, 94,
                      ifelse(gender == 2 & age == 1 & total_score == 33, 96,
                      ifelse(gender == 2 & age == 1 & total_score == 34, 97,
                      ifelse(gender == 2 & age == 1 & total_score == 35, 99,
                      ifelse(gender == 2 & age == 1 & total_score == 36, 100, NA)))))))))))))))))))))))))))))))))))      
                      
T_score_female_12_17 = ifelse(gender == 2 & age == 2 & total_score >= 0 & total_score <= 2, 50, 
                      ifelse(gender == 2 & age == 2 & total_score == 3, 51,
                      ifelse(gender == 2 & age == 2 & total_score == 4, 52,
                      ifelse(gender == 2 & age == 2 & total_score == 5, 54,    
                      ifelse(gender == 2 & age == 2 & total_score == 6, 56,
                      ifelse(gender == 2 & age == 2 & total_score == 7, 58,
                      ifelse(gender == 2 & age == 2 & total_score == 8, 59,
                      ifelse(gender == 2 & age == 2 & total_score == 9, 61,
                      ifelse(gender == 2 & age == 2 & total_score == 10, 63,
                      ifelse(gender == 2 & age == 2 & total_score == 11, 64,
                      ifelse(gender == 2 & age == 2 & total_score == 12, 65,
                      ifelse(gender == 2 & age == 2 & total_score == 13, 66,
                      ifelse(gender == 2 & age == 2 & total_score == 14, 67,
                      ifelse(gender == 2 & age == 2 & total_score == 15, 68,
                      ifelse(gender == 2 & age == 2 & total_score == 16, 69,    
                      ifelse(gender == 2 & age == 2 & total_score >= 17 & total_score <= 18, 70,
                      ifelse(gender == 2 & age == 2 & total_score == 19, 72,
                      ifelse(gender == 2 & age == 2 & total_score == 20, 73,
                      ifelse(gender == 2 & age == 2 & total_score == 21, 75,
                      ifelse(gender == 2 & age == 2 & total_score == 22, 77,
                      ifelse(gender == 2 & age == 2 & total_score == 23, 78,
                      ifelse(gender == 2 & age == 2 & total_score == 24, 80,    
                      ifelse(gender == 2 & age == 2 & total_score == 25, 82,
                      ifelse(gender == 2 & age == 2 & total_score == 26, 83,
                      ifelse(gender == 2 & age == 2 & total_score == 27, 85,
                      ifelse(gender == 2 & age == 2 & total_score == 28, 87,
                      ifelse(gender == 2 & age == 2 & total_score == 29, 88,
                      ifelse(gender == 2 & age == 2 & total_score == 30, 90,
                      ifelse(gender == 2 & age == 2 & total_score == 31, 92,
                      ifelse(gender == 2 & age == 2 & total_score == 32, 93,
                      ifelse(gender == 2 & age == 2 & total_score == 33, 95,
                      ifelse(gender == 2 & age == 2 & total_score == 34, 97,
                      ifelse(gender == 2 & age == 2 & total_score == 35, 98,
                      ifelse(gender == 2 & age == 2 & total_score == 36, 100, NA)))))))))))))))))))))))))))))))))) 
                                                   
T_score_female = T_score_female_6_11
T_score_female[!is.na(T_score_female_12_17)] = T_score_female_12_17[!is.na(T_score_female_12_17)] 
# merges the two variables that are storing female t-scores 

T_score = T_score_male
T_score[!is.na(T_score_female)] = T_score_female[!is.na(T_score_female)] 
# merges male and female tscore variables into one 

return(T_score)
} 

