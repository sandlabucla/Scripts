########################################################################################## 
#Revised Child Anxiety and Depression Scale (RCADS)
#Created by Yael Waizman
########################################################################################## 

#The script below creates a new dataframe with all of the participants scored RCADS data. It loops through each individual participant's RCADS data file, grabs the RCADS raw and t scores for each individual, and places them into the RCADS dataframe.  
RCADS <- function(dataframe, sub_RCADS_file, sub_id_length, RCADS_file_name_length){
  num_rows <- nrow(dataframe)
  RCADS_col_names <- c("RCADS_SAD_RAW", "RCADS_GA_RAW", "RCADS_PD_RAW", "RCADS_SOC_RAW", "RCADS_OCD_RAW", "RCADS_MDD_RAW", "RCADS_ANXIETY_TOTAL_RAW",   "RCADS_TOTAL_SCORE_RAW", "RCADS_SAD_T", "RCADS_GA_T", "RCADS_PD_T", "RCADS_SOC_T", "RCADS_OCD_T", "RCADS_MDD_T", "RCADS_ANXIETY_TOTAL_T", "RCADS_TOTAL_SCORE_T")
  #creates new dataframe that contains 16 columns and the number of rows as the whole dataframe That way, as we get more participants in the dataframe, this RCADS_dataframe will automatically add new rows. 
  RCADS_dataframe <- data.frame(matrix(ncol=length(RCADS_col_names), nrow=num_rows))
  #adds column names to the RCADS_dataframe 
  colnames(RCADS_dataframe) <- RCADS_col_names
  
  for (curr_file in 1:length(sub_RCADS_file)){
    #import individual RCADS data from each participants' RCADS files
    RCADS_data <- read.csv(sub_RCADS_file[curr_file])
    
    #finds the subject ID that is written in the individual RCADS data file
    pos <- regexpr(".csv", sub_RCADS_file[curr_file])
    #add 1 to this pos so it will be the first time that sub ID appears
    sub_id_pos <- pos[1]-RCADS_file_name_length
    #get sub IDs for all the raw data which starts at sub_id_pos and ends at the length of the sub ID characters (i.e., for SB Study this is 5 since SBXXX has the length of 5 characters).
    SUB_ID <- substr(sub_RCADS_file[curr_file],sub_id_pos,sub_id_pos+sub_id_length-1)
    
    #grabs all of the RCADS data
    RCADS_data <- RCADS_data[c(7:15), c(7, 9)]
    #delete the empty row 
    RCADS_data <- RCADS_data[-c(7),]
    #convert the dataframe to vectors
    RCADS_raw_scores <- as.vector(RCADS_data$"boy")
    RCADS_t_scores <- as.vector(RCADS_data$"X.6")
    #combine the two vectors to create one vector for all of the RCADS scores
    RCADS_data <- as.matrix(t(c(RCADS_raw_scores, RCADS_t_scores)))
    
    #find the row that the subject's RCADS_data is located in the dataframe dataframe
    find_SUB_row <- which(dataframe$SUB_ID==SUB_ID)
    #input all of the RCADS scored data into the RCADS dataframe matching it to the subject ID's row
    RCADS_dataframe[find_SUB_row,] <- RCADS_data
    
  }
  
  #append the RCADS_dataframe to the end of the dataframe dataframe
  dataframe <- cbind(dataframe,RCADS_dataframe)
  
  return(dataframe)
}