############################################################################################################################
#SCOFF (Useful Eating Disorder screening questions)
#Created by Yael Waizman
############################################################################################################################

###Calculate total number of "yes" responses
SCOFF <- function(dataframe){
  dataframe$SCOFF_total <- rowSums(dataframe[,paste("SCOFF_", c(1:5), sep="")], na.rm = TRUE)
  ###Determine if likely case of anorexia nervosa or bulimia, i.e. 2 or more "yes" answers (1 = yes, 0 = no)
  dataframe$SCOFF_disorder = ifelse(dataframe$SCOFF_total >= 2, 1, 0) #assigns 1 for scores >=2, 0 for scores <2
  return(dataframe)
}