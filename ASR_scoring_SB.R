###################################################################################################################################
#Adult Self-Report (ASR) For Ages 18-59
#Created by Adriana Mendez Leal and Yael Waizman
###################################################################################################################################

ASR_scoring_SB <- function(dataframe,t_score_table){
  library(expss)
  library(janitor)
  
  # helper function to catch missing values in text responses
  check_col_for_none_or_nas_asr<- function (col_to_check){
    cols_no_nas <- ifelse((tolower(col_to_check) == "none" | tolower(col_to_check) == "n/a" | tolower(col_to_check) == "na" | is.na(col_to_check) | col_to_check == "N/A") == TRUE, NA, col_to_check)
    return(cols_no_nas)
  }
  
  # get the ASR raw data column names
  ASR_raw_data_colnames <- colnames(dataframe)[grep('ASR',colnames(dataframe))]
  
  ###Calculate total score on Friends scale (total of items ASR_FRIENDS_A - ASR_FRIENDS_D), don't score if any items are missed
  dataframe$ASR_FRIENDS_TOTAL<- rowSums(dataframe[,paste("ASR","FRIENDS",c("A","B","C","D"),sep='_')], na.rm = FALSE) 
  dataframe$ASR_FRIENDS_TSCORE <- ASR_FRIENDS_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_FRIENDS_TOTAL) # uses CBCL_ASR_subscale_tscores_func 
  
  ###Calculate total score on Spouse/Partner scale (items ASR_RELATION_A - ASR_RELATION_H)
  dataframe$ASR_SP_PART_TOTAL <- (rowSums(dataframe[,paste("ASR","RELATION",c("A","C","D","G"),sep='_')],na.rm = FALSE)) - (rowSums(dataframe[,paste("ASR","RELATION",c("B","E","F","H"),sep='_')],na.rm = FALSE))
  dataframe$ASR_SP_PART_TSCORE <- ASR_SPOUSE_PARTNER_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_SP_PART_TOTAL) # uses CBCL_ASR_subscale_tscores_func 
  
  ###Calculate total score on Family scale (total of items ASR_FAMILY_A - ASR_FAMILY_F)
  fam_var_list = c('ASR_FAMILY_A','ASR_FAMILY_B','ASR_FAMILY_C','ASR_FAMILY_D','ASR_FAMILY_E1','ASR_FAMILY_E2','ASR_FAMILY_E3','ASR_FAMILY_E4','ASR_FAMILY_F')
  
  # replace all 3s ("not in contact") with NAs
  dataframe[,fam_var_list][dataframe[, fam_var_list] == 3] <- NA
  dataframe$ASR_FAMILY_MEAN = round(round(rowMeans(dataframe[,fam_var_list],na.rm = TRUE)/.2)*.2, digits = 1) #rounds mean score to the nearest .2
  dataframe$ASR_FAMILY_TSCORE <- ASR_FAMILY_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_FAMILY_MEAN) # uses CBCL_ASR_subscale_tscores_func 
  
  ###Calculate total score on Job scale (total of items ASR_WORK_A - ASR_WORK_I)
  dataframe$ASR_JOB_TOTAL = rowSums(dataframe[,paste("ASR","WORK",c("A","C"),sep='_')],na.rm = FALSE) +
    - rowSums(dataframe[,paste("ASR","WORK",c("B","D","F","G","H","I"),sep='_')],na.rm = FALSE) 
  dataframe$ASR_JOB_TSCORE <- ASR_JOB_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_JOB_TOTAL) # uses CBCL_ASR_subscale_tscores_func 
  
  ###Calculate total score on Education scale (total of items ASR_EDU_A - ASR_EDU_E)
  dataframe$ASR_EDU_TOTAL = rowSums(dataframe[,paste("ASR","EDU",c("A","B","D"),sep='_')],na.rm = FALSE) +
    - rowSums(dataframe[,paste("ASR","EDU",c("C","E"),sep='_')],na.rm = FALSE) 
  dataframe$ASR_EDU_TSCORE <- ASR_EDU_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_EDU_TOTAL) # uses CBCL_ASR_subscale_tscores_func 
  
  ###Calculate mean of all subscale T-scores (Friends, Spouse/Partner, Family, Job & Education)
  dataframe$ASR_MEAN_ADAPTIVE_SCORE <- round_half_up(rowMeans(dataframe[,paste("ASR",c("FRIENDS_TSCORE", "SP_PART_TSCORE", 
                                                                                               "FAMILY_TSCORE", "JOB_TSCORE", 
                                                                                               "EDU_TSCORE"),sep='_')], na.rm=TRUE)/.5)*.5
  
  ###Get Mean Adaptive T-score from mean of all subscale T-scores calculated above 
  dataframe$ASR_MEAN_ADAPT_TSCORE <- ASR_MEAN_ADAPT_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_MEAN_ADAPTIVE_SCORE) # uses CBCL_ASR_subscale_tscores_func to round mean and get corresponding t-score
  
  ###Determine if participant falls in the clinical, borderline, or normal range based on Mean Adaptive T-score
  dataframe$ASR_MEAN_ADAPT_RANGE <- ASR_MEAN_ADAPT_RANGE(dataframe$ASR_MEAN_ADAPT_TSCORE)
  
  ###Calculate (1) Internalizing, (2) Externalizing and (3) Total Problems total scores (ASR_1 - ASR_123)
  
  # First, check if any participants are missing data on more than 8 items in the range ASR_1 - ASR_123 (NOT including items 2, 4, 15, 49, 73, 80,88,98,106,109 and 123,
  # ignoring 56 since empty values there count as 0, and 18 and 91 since removed); If yes, do NOT calculate total scores 
  ASR_items_all<- 1:123
  ASR_items_no_sd<-ASR_items_all[!ASR_items_all %in% c(2,4,15,18,49,73,80,88,91,98,106,109,123,56)]
  dataframe$ASR_more_8_missing_items <- rowSums(is.na(dataframe[,paste("ASR_",ASR_items_no_sd,sep='')])) > 8
  
  ##Substitute the mean of ASR items 9, 36, 40, 46, 63, 66, 70, 84, and 85 for the missing item 18 (suicidality).
  dataframe$ASR_18 <- floor(rowMeans(dataframe[,paste("ASR_",c(9,36,40,46,63,66,70,84,85),sep='')],na.rm = TRUE))
  
  ##Substitute the mean of ASR items 12, 13, 14, 22, 31, 33, 34, 35, 45, 47, 50, 52, 71, 103, 107, 112, and 113 for the missing items 91 (suicidality).
  dataframe$ASR_91 <- floor(rowMeans(dataframe[,paste("ASR_",c(12,13,14,22,31,33,34,35,45,47,50,52,71,103,107,112,113),sep='')],na.rm = TRUE))
  
  # (1) Internalizing (Total of Anxious/Depressed, Withdrawn, and Somatic Complaints items)
  ASR_anxious_depressed_all = dataframe[,paste("ASR_",c(12,13,14,22,31,33,34,35,45,47,50,52,71,91,103,107,112,113),sep='')]
  dataframe$ASR_ANX_DEP_TOT <- rowSums(ASR_anxious_depressed_all)
  ###Get ASR Anxious/Depressed scale T-score
  dataframe$ASR_ANX_DEP_TSCORE <- ASR_ANX_DEP_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_ANX_DEP_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_withdrawn_all = dataframe[,paste("ASR_",c(25,30,42,48,60,65,67,69,111),sep='')]
  dataframe$ASR_WITHDRAWN_TOT <- rowSums(ASR_withdrawn_all)
  ###Get ASR Withdrawn scale T-score
  dataframe$ASR_WITHDRAWN_TSCORE <- ASR_WITHDRAWN_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_WITHDRAWN_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_somatic_all = dataframe[,paste("ASR_",c(51,54,paste("56_",c("A","B","C","D","E","F","G","H","I"),sep=''),100),sep='')]
  dataframe$ASR_SOM_TOT =rowSums(ASR_somatic_all)
  ###Get ASR Somatic Complaints scale T-score
  dataframe$ASR_SOM_TSCORE <- ASR_SOM_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_SOM_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_Internalizing_all = cbind(ASR_anxious_depressed_all, ASR_withdrawn_all, ASR_somatic_all)
  dataframe$ASR_INT_TOTAL = ifelse(dataframe$ASR_more_8_missing_items == TRUE, NA, rowSums(ASR_Internalizing_all, na.rm = TRUE)) # calculates only if no more than 8 items are missing                                      
  
  # (2) Externalizing (Total of Agressive Behavior, Rule-breaking Behavior, and Intrusive items)
  ASR_aggressive_all = dataframe[,paste("ASR_",c(3,5,16,28,37,55,57,68,81,86,87,95,97,116,118),sep='')]
  dataframe$ASR_AGGRESSIVE_TOT <- rowSums(ASR_aggressive_all)
  ###Get ASR Aggressive Behavior scale T-score
  dataframe$ASR_AGGRESSIVE_TSCORE <- ASR_AGGRESSIVE_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_AGGRESSIVE_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_rule_breaking_all = dataframe[,paste("ASR_",c(6,20,23,26,39,41,43,76,82,90,92,114,117,122),sep='')]
  dataframe$ASR_RULE_BREAKING_TOT <- rowSums(ASR_rule_breaking_all)
  ###Get ASR Rule Breaking scale T-score
  dataframe$ASR_RULE_BREAKING_TSCORE <- ASR_RULE_BREAKING_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_RULE_BREAKING_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_intrusive_all = dataframe[,paste("ASR_",c(7,19,74,93,94,104),sep='')]
  dataframe$ASR_INTRUSIVE_TOT <- rowSums(ASR_intrusive_all)
  ###Get ASR Intrusive scale T-score
  dataframe$ASR_INTRUSIVE_TSCORE <- ASR_INTRUSIVE_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_INTRUSIVE_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_Externalizing_all = cbind(ASR_aggressive_all, ASR_rule_breaking_all, ASR_intrusive_all)
  dataframe$ASR_EXT_TOTAL = ifelse(dataframe$ASR_more_8_missing_items == TRUE, NA, rowSums(ASR_Externalizing_all, na.rm = TRUE)) # calculates only if no more than 8 items are missing 
  
  # Other Problems, i.e. those that do not fit into internalizing/externalizing categories (Includes Thought Problems and Attention Problems items); necessary to calculate Total Problems
  ASR_thought_probs_all = dataframe[,paste("ASR_",c(9,18,36,40,46,63,66,70,84,85),sep='')]
  dataframe$ASR_THOUGHT_PROB_TOT <- rowSums(ASR_thought_probs_all)
  ###Get ASR Thought Problems scale T-score
  dataframe$ASR_THOUGHT_PROB_TSCORE <- ASR_THOUGHT_PROB_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_THOUGHT_PROB_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_attention_probs_all= dataframe[,paste("ASR_",c(1,8,11,17,53,59,61,64,78,101,102,105,108,119,121),sep='')]
  dataframe$ASR_ATTN_PROB_TOT <- rowSums(ASR_attention_probs_all)
  ###Get ASR Attention Problems scale T-score
  dataframe$ASR_ATTN_PROB_TSCORE <- ASR_ATTN_PROB_TSCORE(dataframe$ASR_GENDER, dataframe$ASR_ATTN_PROB_TOT) #uses CBCL_ASR_subscale_tscores_funcs
  ASR_other_problems_all = dataframe[,paste("ASR_",c(10,21,24,27,29,32,38,44,58,62,72,75,77,79,83,89,96,99,110,115,120),sep='')]
  dataframe$ASR_OTHER_PROBLEMS <- rowSums(ASR_other_problems_all)
  ASR_other_probs_all = cbind(ASR_thought_probs_all, ASR_attention_probs_all, ASR_other_problems_all)
  dataframe$ASR_OTHER_PROBS_TOTAL = ifelse(dataframe$ASR_more_8_missing_items == TRUE, NA, rowSums(ASR_other_probs_all, na.rm = TRUE)) # calculates only if no more than 8 items are missing                                             
  
  # (3) Total Problems (Total of Internalizing problems, Externalizing problems, and Other problems, calculated above)
  dataframe$ASR_TOT_TOTAL = rowSums(dataframe[,c('ASR_INT_TOTAL','ASR_EXT_TOTAL','ASR_OTHER_PROBS_TOTAL')], na.rm = FALSE)
  
  # Create an empty column for internalizing T-score
  dataframe$ASR_INT_TSCORE  <- NA
  # Create an empty column for externalizing T-score
  dataframe$ASR_EXT_TSCORE  <- NA
  # Create an empty column for total competance T-score
  dataframe$ASR_TOT_TSCORE  <- NA
  
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
    }
    return(T_score)
  }
  
  # get the ASR scored data column names
  ASR_scored_data_colnames <- colnames(dataframe)[grep('ASR',colnames(dataframe))][which(!(colnames(dataframe)[grep('ASR',colnames(dataframe))]%in%ASR_raw_data_colnames))]
  # scale tag names to be used to get their corresponding t scores in the for loop below
  scale_tag_list <- c("Int", "Ext", "Tot")
  
  # use function to get the T scores for Internalizing, Externalizing, and Total Probs scores from the t score table depending on the participant's age and gender
  for(curr_scale_tag in scale_tag_list){
    # any participant that did not complete the ASR, change all of their scored values to NA
    for (each_row in 1:nrow(dataframe)) { 
      if (dataframe$Age[each_row] >= 18) {
        if (dataframe$Age[each_row] >= 18 && dataframe$Age[each_row] < 36){
          curr_min_age<- 18
          curr_max_age<-35
        } else if (dataframe$Age[each_row] >= 36 && dataframe$Age[each_row] < 60){
          curr_min_age<-36
          curr_max_age<-59
        } 
        dataframe[each_row,paste("ASR", toupper(curr_scale_tag),"TSCORE", sep="_")] <- get_t_score_from_table(curr_min_age, 
                                                                                                                  curr_max_age, curr_scale_tag, 
                                                                                                                  dataframe$Gender[each_row], 
                                                                                                                  dataframe[each_row,paste("ASR", toupper(curr_scale_tag),"TOTAL", sep="_")], ASR_t_score_table)
      }
    }  
  }
  return(dataframe)
}
