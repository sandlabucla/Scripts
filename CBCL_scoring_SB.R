########################################################################################## 
#Child Behavior Checklist for Ages 6-18 (CBCL/6-18) scoring
#Created by Adriana Mendez Leal and Yael Waizman
########################################################################################## 

CBCL_scoring_SB <- function(dataframe,t_score_table){
  
  dataframe$CBCL_2_TEXT <- as.character(dataframe$CBCL_2_TEXT)
  dataframe$CBCL_40_TEXT <- as.character(dataframe$CBCL_40_TEXT)
  dataframe$CBCL_56_D_TEXT <- as.integer(dataframe$CBCL_56_D_TEXT)
  dataframe$CBCL_66_TEXT <- as.character(dataframe$CBCL_66_TEXT)
  dataframe$CBCL_85_TEXT <- as.character(dataframe$CBCL_85_TEXT)
  dataframe$CBCL_113_B_TEXT <- as.character(dataframe$CBCL_113_B_TEXT)
  
 # helper function to catch missing values in text responses
 # returns 1 if a none NA response is in the text field,otherwise NA
  check_col_for_any_response_open_ended<- function (col_to_check){
    cols_no_nas <- ifelse((tolower(col_to_check) == "none" | tolower(col_to_check) == "n/a" | tolower(col_to_check) == "na" | is.na(col_to_check) | col_to_check == "N/A") == TRUE,NA,1)
    return(cols_no_nas)
  }
  
  # attach prefix for questionnaire to item names
  attach_q_prefix<- function(q_prefix,item_names){
       final_item_names <- paste(q_prefix,'_',item_names,sep='')
       return(final_item_names)
  }
  
  # helper function to calculate CBCL ACTIVITES and SOCIAL competence scales
  calculate_CBCL_total_competence_scales<-function(curr_row,inds_cols_w_means){
    if (sum(is.na(curr_row))==0){
      # if there are no missing scores, calculate the sum of all scores and round it to the nearest .5 and set it as the total competence score
      tot_score <- round_half_up(sum(curr_row)/0.5)*.5
    } else if (sum(is.na(curr_row))==1){
      #  if there is 1 missing score that is in the inds_cols_w_means, take the mean of the other five scores. If this mean is greater than 2, round it down to 2, when replacing the missing score
      if (mean(curr_row, na.rm = TRUE) > 2 && length(intersect(which(is.na(curr_row)),inds_cols_w_means))==1){
        curr_row[which(is.na(curr_row))] <- 2
        # take the mean of all scores and round to nearest .5 and set it as the total competence score
        tot_score <- round_half_up(sum(curr_row,na.rm = T)/0.5)*.5
      } else {
        # if there is 1 missing score that is not in the inds_cols_w_means, take the mean of the other five scores. If this mean is less than 2, replace it with the missing score
        curr_row[which(is.na(curr_row))] <- mean(curr_row, na.rm = TRUE)
        # take the sum of all scores and round to nearest .5 and set it as the total competence score
        tot_score <- round_half_up(sum(curr_row,na.rm = T)/0.5)*.5
      }
    } else if (sum(is.na(curr_row))>1) {
      # if more than one missing value, total competence score should not be calculated and set to NA
      tot_score <- NA
    }
    return (tot_score)
  }
  
  # get the CBCL raw data column names
  CBCL_raw_data_colnames <- colnames(dataframe)[grep('CBCL',colnames(dataframe))]
  
  ###Calculate total score on Activities scale based on sections I - sports,II - other activities,and IV - jobs
  # I-A. Number of sports. Sum up number of sports with anything written in (score 0 to 3). Don't count None,N/A,etc. 
  dataframe$CBCL_NUM_SPORTS <- rowSums(check_col_for_any_response_open_ended(dataframe[,attach_q_prefix('CBCL',c('I_1','I_2','I_3'))]),na.rm = TRUE) 
  
  # I-B. Mean of participation and skill in sports, excluding NAs for sports not selected (confirm this is right)
  dataframe$CBCL_SPORTS_MEAN <- rowMeans(dataframe[,attach_q_prefix('CBCL',c('I_TIME_A','I_TIME_B','I_TIME_C','I_WELL_A','I_WELL_B','I_WELL_C'))],na.rm =TRUE)
  
  #II-A. Number of other activities 
  # Count non-blanks and everything other than "none", "None", and not applicable as 1
  dataframe$CBCL_NUM_OTH_ACT <- rowSums(check_col_for_any_response_open_ended(dataframe[,attach_q_prefix('CBCL',c('II_1','II_2','II_3'))]),na.rm = TRUE) 
    
  #II-B. Mean of participation and skill in other activities 
  dataframe$CBCL_OTH_ACT_MEAN<- rowMeans(dataframe[,attach_q_prefix('CBCL',c('II_TIME_A','II_TIME_B','II_TIME_C','II_WELL_A','II_WELL_B','II_WELL_C'))],na.rm =TRUE)
  
  #IV-A. Number of jobs: Count non-blanks and everything other than "none", "None", and not applicable as 1
  dataframe$CBCL_NUM_JOBS <- rowSums(check_col_for_any_response_open_ended(dataframe[,attach_q_prefix('CBCL',c('IV_1','IV_2','IV_3'))]),na.rm = TRUE) 
  
  #IV-B. Mean job quality
  dataframe$CBCL_JOBS_MEAN <- rowMeans(dataframe[,attach_q_prefix('CBCL',c('IV_WELL_A','IV_WELL_B','IV_WELL_C'))],na.rm = TRUE)
  
  #Calculate Activities total by adding up the 6 sports, hobbies/other activites, and jobs scores calculated above
  dataframe$CBCL_ACT_TOT<- apply(dataframe[,attach_q_prefix('CBCL',c('NUM_SPORTS','SPORTS_MEAN','NUM_OTH_ACT','OTH_ACT_MEAN',
                                                        'NUM_JOBS','JOBS_MEAN'))],1,function(x) calculate_CBCL_total_competence_scales(x,c(2,4,6)))

  ###Get CBCL Activities scale T-score
  dataframe$CBCL_CHILD_AGE_CAT = ifelse(dataframe$CBCL_CHILD_AGE <= 11,1,2)
  dataframe$CBCL_ACT_TSCORE <- CBCL_ACT_TSCORE(dataframe$CBCL_CHILD_GENDER,dataframe$CBCL_CHILD_AGE_CAT,dataframe$CBCL_ACT_TOT) #uses CBCL_ASR_subscale_tscores_funcs
 
  ###Calculate total score on Social scale based on sections III - organizations, V - friends, and VI - behavior
  #III-A. Number of organizations
  dataframe$CBCL_NUM_ORGS <- rowSums(check_col_for_any_response_open_ended(dataframe[,attach_q_prefix('CBCL',c('III_1','III_2','III_3'))]),na.rm = TRUE) 
  
  #III-B. Mean of participation in organizations 
  dataframe$CBCL_ORGS_MEAN <- rowMeans(dataframe[,attach_q_prefix('CBCL',c('III_ACTIVE_A','III_ACTIVE_B','III_ACTIVE_C'))],na.rm = TRUE)
  
  #VI-A. Behavior with other siblings (turn 3s into Nas because 3= no siblings)
  dataframe$CBCL_VI_A <- ifelse(dataframe$CBCL_VI_A==3,NA,dataframe$CBCL_VI_A) #ignore responses that are 3's to match codebook
  dataframe$CBCL_BEHAV_OTH_MEAN <- rowMeans(dataframe[,attach_q_prefix('CBCL',c('VI_A','VI_B','VI_C'))],na.rm = TRUE)
  
  #Calculate Social total by adding up 6 scores for organizations, friends, and behavior scores calculated above
  # if only one NA, replace with mean of other 5 values (if NA is for CBCL_ORGS_MEAN, CBCL_V_2,CBCL_BEHAV_OTH_MEAN, or CBCL_VI_D and mean>2, replace NA with 2)
  # take mean of all 6 values once NAs are replaced, ignoring participants with greater than 2 NA values
  dataframe$CBCL_SOC_TOT<- apply(dataframe[,attach_q_prefix('CBCL',c('NUM_ORGS','ORGS_MEAN','V_1','V_2','BEHAV_OTH_MEAN','VI_D'))],
                                     1,function(x) calculate_CBCL_total_competence_scales(x,c(2,4,5,6)))

  ###Get CBCL Social scale T-score
  dataframe$CBCL_CHILD_AGE_CAT = ifelse(dataframe$CBCL_CHILD_AGE <= 11, 1, 2)
  dataframe$CBCL_SOC_TSCORE <- CBCL_SOC_TSCORE(dataframe$CBCL_CHILD_GENDER, dataframe$CBCL_CHILD_AGE_CAT, dataframe$CBCL_SOC_TOT) #uses CBCL_ASR_subscale_tscores_funcs
 
  ###Calculate total score on School scale based on section VII - academic performance
  ##replaces non-academic items (looking for common non-academic keywords)
  common_non_academic_keywords<- c('music','art','pe\\>','p\\.e\\>','p\\.e\\.\\>','drama','religion','bible','theat','video','photo','band','choir','ball','dance')
  extra_academic_sub_fields<- c('CBCL_VII_1_E','CBCL_VII_1_F','CBCL_VII_1_G')
  
  #this print function below is to check that the correct responses are being removed
  #print('removing non-academic subjects, please check to confirm')
  for (curr_column in  extra_academic_sub_fields){
    non_acad_inds <- grepl(paste(common_non_academic_keywords, collapse = "|"), lapply(dataframe[,c(paste(curr_column,'_TEXT',sep = ''))],function (x) tolower(x)))
    #print(dataframe[non_acad_inds,c(paste(curr_column,'_TEXT',sep = ''))])
    dataframe[non_acad_inds,c(paste(curr_column,'_TEXT',sep = ''),curr_column)]<-NA
  }
  
  #VII-1. School Mean performance - mean of ratings for each subject (including write-ins E-G that are only academic)
  dataframe$CBCL_ACAD_MEAN <- rowMeans(dataframe[,attach_q_prefix('CBCL',c('VII_1_A','VII_1_B','VII_1_C','VII_1_D',
                                                            'VII_1_E','VII_1_F','VII_1_G'))],na.rm = T)
  
  #Calculate School total by adding the 4 school scores, unless any are NAs, round to nearest .5
  dataframe$CBCL_SCH_TOT<-round_half_up(rowSums(dataframe[,attach_q_prefix('CBCL',c('ACAD_MEAN','VII_2','VII_3',
                                                                                            'VII_4'))],na.rm = F)/.5)*.5
  
  ###Get CBCL School scale T-score
  dataframe$CBCL_CHILD_AGE_CAT = ifelse(dataframe$CBCL_CHILD_AGE <= 11, 1, 2)
  dataframe$CBCL_SCH_TSCORE <- CBCL_SCH_TSCORE(dataframe$CBCL_CHILD_GENDER, dataframe$CBCL_CHILD_AGE_CAT, 
                                                   dataframe$CBCL_SCH_TOT) #uses CBCL_ASR_subscale_tscores_funcs
 
  ###Calculate total competence score based on activities, social, and school scales

  #Do not compute a total competence score if any of the three scores above is missing 
  #creates column for final total (either sum of all 3 scores or NA if any are missing)
  dataframe$CBCL_TOTAL_COMP <-rowSums(dataframe[,attach_q_prefix('CBCL',c('ACT_TOT','SOC_TOT','SCH_TOT'))],na.rm = F)
  
  ###Calculate (1) Internalizing and (2) Externalizing total scores (CBCL_1 - CBCL_113) 

  # First, check if any participants are missing data on more than 8 items in the range CBCL_1 - CBCL_113 (NOT including open-ended items 56h and 113); If yes, do NOT calculate total scores 
  dataframe$CBCL_more_8_missing_items = rowSums(is.na(dataframe[,attach_q_prefix('CBCL',
                                            c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,
                                              26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
                                              '56_A','56_B','56_C','56_D','56_E','56_F','56_G',57,58,59,60,61,62,63,64,65,66,67,68,69,70,
                                              71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,98,99,100,
                                              101,102,103,104,105,106,107,108,109,110,111,112))]))>8
  
  compute_tot_scores_for_valid_entries<- function(subscale_df,missing_item_col){
    tot_score <- ifelse(missing_item_col == TRUE,NA,rowSums(subscale_df,na.rm = TRUE))
    return(tot_score)
  }
  
  ##Substitute the mean of CBCL items 9, 40, 46, 58, 59, 60, 66, 70, 76, 83, 84, 85, 92, 100 for the missing item 18 (suicidality).
  dataframe$CBCL_18 <- floor(rowMeans(dataframe[,attach_q_prefix('CBCL',c(9,40,46,58,59,60,66,70,76,83,84,85,92,100))],na.rm = TRUE))
  
  ##Substitute the mean of CBCL items 14, 29, 30, 31, 32, 33, 35, 45, 50, 52, 71, and 112 for the missing items 91 (suicidality).
  dataframe$CBCL_91 <- floor(rowMeans(dataframe[,attach_q_prefix('CBCL',c(14,29,30,31,32,33,35,45,50,52,71,112))],na.rm =TRUE))

  # (1) Internalizing (Total of Anxious/Depressed, Withdrawn/Depressed, and Somatic Complaints items)
  CBCL_anxious_all <- attach_q_prefix('CBCL',c(14,29,30,31,32,33,35,45,50,52,71,91,112))
  dataframe$CBCL_ANX_DEP <- rowSums(dataframe[,CBCL_anxious_all],na.rm = TRUE)
  CBCL_withdrawn_all <-  attach_q_prefix('CBCL',c(5,42,65,69,75,102,103,111))
  dataframe$CBCL_WITHDRAWN <- rowSums(dataframe[,CBCL_withdrawn_all],na.rm = TRUE)
  CBCL_somatic_all <- attach_q_prefix('CBCL',c(47,49,51,54,'56_A','56_B','56_C','56_D','56_E','56_F','56_G'))
  dataframe$CBCL_SOM <- rowSums(dataframe[,CBCL_somatic_all],na.rm = TRUE)
  
  # calculates internalizing total score only if no more than 8 items are missing 
  dataframe$CBCL_INT_TOTAL <- compute_tot_scores_for_valid_entries (dataframe[,c(CBCL_anxious_all,CBCL_withdrawn_all,CBCL_somatic_all)],
                                                                        dataframe$CBCL_more_8_missing_items)

  # (2) Externalizing (Total of Rule-Breaking Behavior, Aggressive Behavior)
  CBCL_rule_breaking_all = attach_q_prefix('CBCL',c(2,26,28,39,43,63,67,72,73,81,82,90,96,99,101,105,106))
  dataframe$CBCL_RULE_BREAKING <- rowSums(dataframe[,CBCL_rule_breaking_all],na.rm = TRUE)
  CBCL_aggressive_all = attach_q_prefix('CBCL',c(3,16,19,20,21,22,23,37,57,68,86,87,88,89,94,95,97,104))
  dataframe$CBCL_AGGRESSIVE <- rowSums(dataframe[,CBCL_aggressive_all],na.rm = TRUE)
  
  
  # calculates only if no more than 8 items are missing 
  dataframe$CBCL_EXT_TOTAL <- compute_tot_scores_for_valid_entries (dataframe[,c(CBCL_rule_breaking_all, CBCL_aggressive_all)],
                                                                        dataframe$CBCL_more_8_missing_items)
  
  # Other Problems, i.e. those that do not fit into internalizing/externalizing categories (Includes Social Problems, Thought Problems, and Attention Problems items); necessary to calculate Total Problems
  CBCL_social_probs_all <- attach_q_prefix('CBCL',c(11,12,25,27,34,36,38,48,62,64,79))
  dataframe$CBCL_SOC_PROB <- rowSums(dataframe[,CBCL_social_probs_all],na.rm = TRUE)
  CBCL_thought_probs_all <- attach_q_prefix('CBCL',c(9,18,40,46,58,59,60,66,70,76,83,84,85,92,100))
  dataframe$CBCL_THOUGHT_PROB <- rowSums(dataframe[,CBCL_thought_probs_all],na.rm = TRUE)
  CBCL_attention_probs_all <- attach_q_prefix('CBCL',c(1,4,8,10,13,17,41,61,78,80))
  dataframe$CBCL_ATTN_PROB <- rowSums(dataframe[,CBCL_attention_probs_all],na.rm = TRUE)
  CBCL_other_problems_all <- attach_q_prefix('CBCL',c(6,7,15,24,44,53,55,'56_H',74,77,93,98,107,108,109,110,'113_A','113_B','113_C'))
  dataframe$CBCL_OTHER_PROBLEMS <- rowSums(dataframe[,CBCL_other_problems_all],na.rm = TRUE)
  
  # calculates only if no more than 8 items are missing
  dataframe$CBCL_OTHER_PROBS_TOTAL <- compute_tot_scores_for_valid_entries (dataframe[,c(CBCL_thought_probs_all, CBCL_attention_probs_all,
                                                                                                 CBCL_social_probs_all, CBCL_other_problems_all)],
                                                                                dataframe$CBCL_more_8_missing_items)
  
  ###Calculate Total Problems score based on internalizing, externalizing, and other problems total scores
  #Does not compute a total problems score if any of the three scores above is missing 
  dataframe$CBCL_TOT_TOTAL <- rowSums(dataframe[c('CBCL_INT_TOTAL','CBCL_EXT_TOTAL','CBCL_OTHER_PROBS_TOTAL')],na.rm = F) 
  
  # Create an empty column for internalizing T-score
  dataframe$CBCL_INT_TSCORE  <- NA
  # Create an empty column for externalizing T-score
  dataframe$CBCL_EXT_TSCORE  <- NA
  # Create an empty column for total competance T-score
  dataframe$CBCL_TOT_TSCORE  <- NA
  
  get_t_score_from_table <- function(age_min, age_max, scale_tag, curr_gender, total_score, tscores_table){
    # get the column name for the desired tscore from tscore table
    min_colname <- paste(curr_gender, scale_tag, age_min, age_max, "Min", sep = "_")
    max_colname <- paste(curr_gender, scale_tag, age_min, age_max, "Max", sep = "_")
    # check the Total score & gender & age
    min_rows <- which(tscores_table[,which(colnames(tscores_table)==max_colname)] >= total_score)
    max_rows <- which(tscores_table[,which(colnames(tscores_table)==min_colname)] <= total_score)
    row_ind <- min_rows[min_rows %in% max_rows]
    #row_ind <- which(which(tscores_table[,which(colnames(tscores_table)==max_colname)] >= total_score) %in% which(tscores_table[,which(colnames(tscores_table)==min_colname)] <= total_score))    # if total score is greater than min and lower than max (based on gender & age group), give it the corresponding T score
    if(!identical(row_ind, integer(0))){
      T_score <- tscores_table$T[row_ind]
    } else {
      T_score <- NA
      #print(paste("Total score value of ", total_score, " does not exsit for this subject, please check", sep=""))
      #print(paste(scale_tag, T_score, sep = " "))
      #print(curr_gender)
      #print(total_score)
    }
    return(T_score)
  }
  
  # get the CBCL scored data column names
  CBCL_scored_data_colnames <- colnames(dataframe)[grep('CBCL',colnames(dataframe))][which(!(colnames(dataframe)[grep('CBCL',colnames(dataframe))]%in%CBCL_raw_data_colnames))]
  # scale tag names to be used to get their corresponding t scores in the for loop below
  scale_tag_list <- c("Int", "Ext", "Tot")
  
  # use function to get the T scores for Internalizing, Externalizing, and Total Probs scores from the t score table depending on the participant's age and gender
  for(curr_scale_tag in scale_tag_list){
    # any participant that did not complete the CBCL, change all of their scored values to NA
    for (each_row in 1:nrow(dataframe)) { 
      #print(paste("each row ",each_row,sep=""))
      if (dataframe$Age[each_row] < 18) {
        if (dataframe$Age[each_row] >= 6 && dataframe$Age[each_row] < 12){
          curr_min_age<- 6
          curr_max_age<-11
        } else if (dataframe$Age[each_row] >= 12 && dataframe$Age[each_row] < 18){
          curr_min_age<-12
          curr_max_age<-18
        } 
        dataframe[each_row,paste("CBCL", toupper(curr_scale_tag),"TSCORE", sep="_")] <- get_t_score_from_table(curr_min_age, curr_max_age, 
                                                                                                                  curr_scale_tag, dataframe$Gender[each_row], 
                                                                                                                  dataframe[each_row,paste("CBCL", toupper(curr_scale_tag),"TOTAL", sep="_")], 
                                                                                                                  CBCL_t_score_table)
      }
        # if the participant had NAs for the entire CBCL survey, change all of their scored values to NA as well
        if (all(is.na(dataframe[each_row,CBCL_raw_data_colnames]))==TRUE) {
          dataframe[each_row,CBCL_scored_data_colnames] <- NA
        }
    }  
  }
  return(dataframe)
}
