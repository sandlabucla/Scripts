# Adapted from CTS_T1_T2_T3_T4_T5_survey_scoring script and CTQ_scoring script.

# Change this to be the path to wherever the data are saved on your computer.
data_path <- '/Users/nataliesaragosa-harris/Box/College_transition_study_(CTS)/Data/marie_natalie_project/data'
# script_path <- getwd()
# Change this to be the path to wherever the CTS_scoring script is on your computer.
# source(file.path(script_path,"CTQ_scoring.R"))

library(readr)
library(tidyr)
library(dplyr)

# Define function for reverse scoring.
Reverse_code_5_likert <- function(response) {
  
  Reverse_code_5_likert = ifelse(response == 1, 5, 
                                 ifelse(response == 2, 4,
                                        ifelse(response == 3, 3,
                                               ifelse(response == 4, 2,1))))
  return(Reverse_code_5_likert)
}


# Read in unscored survey responses.
CTS_T1_data <- read.csv(file.path(data_path,"CTS_T1_RawMasterCopy.csv"),header=TRUE)
View(CTS_T1_data)

# Clean the raw CTS_T1 datafile by getting rid of the rows and columns that are unecessary.
# Save StartDate, and then save from CTS_ID on.
CTS_T1_data <- CTS_T1_data[c(1:nrow(CTS_T1_data)), c(which(colnames(CTS_T1_data)=="StartDate"),
                                                     which(colnames(CTS_T1_data)=="CTS_ID"):ncol(CTS_T1_data))] 
# Make CTS_ID the first column.
CTS_T1_data <- CTS_T1_data %>% select(CTS_ID, everything())
CTS_T1_data <- CTS_T1_data[-c(1:2),] # Remove unnecessary rows from Qualtrics file.

# Get day when they filled out the questionnaires.
dates_T1 <- select(CTS_T1_data, CTS_ID, StartDate)

# Note: This will give a warning about not knowing which time zone to use, but can ignore since we are not looking at time of day.
dates_T1$date <- as.POSIXct(strptime(dates_T1$StartDate, "%m/%d/%y %H:%M"))
dates_T1$date_standard<- as.Date(dates_T1$date, "%m/%d/%y") #standardize date format.
dates_T1$timepoint = "T1"

# Keep only the participant ID and the CTQ columns.
CTS_T1_data <- select(CTS_T1_data,c("CTS_ID",contains("CTQ")))
# Make the CTQ columns (but not the CTS_ID column) numeric.
CTS_T1_data  <- CTS_T1_data  %>%
  mutate(across(!CTS_ID,as.numeric))

# Convert all Refused (777), Don't Know (888), and N/A (999) responses to NAs to exclude from calculations.
CTS_T1_data[CTS_T1_data == 777] <- NA
CTS_T1_data[CTS_T1_data == 888] <- NA
CTS_T1_data[CTS_T1_data == 999] <- NA

## Uses function to score the Childhood Trauma Questionnaire (CTQ) 
# CTS_T1_data <- CTQ(CTS_T1_data)

### First, reverse code anxiety-absent items 
CTS_T1_data$CTQ2R <- Reverse_code_5_likert(CTS_T1_data$CTQ2)
CTS_T1_data$CTQ5R <- Reverse_code_5_likert(CTS_T1_data$CTQ5) 
CTS_T1_data$CTQ7R <- Reverse_code_5_likert(CTS_T1_data$CTQ7) 
CTS_T1_data$CTQ13R <- Reverse_code_5_likert(CTS_T1_data$CTQ13)
CTS_T1_data$CTQ19R <- Reverse_code_5_likert(CTS_T1_data$CTQ19)
CTS_T1_data$CTQ26R <- Reverse_code_5_likert(CTS_T1_data$CTQ26)
CTS_T1_data$CTQ28R <- Reverse_code_5_likert(CTS_T1_data$CTQ28)

# Calculate emotional abuse scale.
CTS_T1_data$CTQ_Emotional_Abuse <- rowSums(CTS_T1_data[,paste("CTQ",c(3,8,14,18,25),sep="")], na.rm = FALSE)
# Calculate physical abuse scale.
CTS_T1_data$CTQ_Physical_Abuse <- rowSums(CTS_T1_data[,paste("CTQ",c(9,11,12,15,17),sep="")], na.rm = FALSE)
# Calculate sexual abuse scale.
CTS_T1_data$CTQ_Sexual_Abuse <- rowSums(CTS_T1_data[,paste("CTQ",c(20,21,23,24,27),sep="")], na.rm = FALSE)
# Calculate emotional neglect scale.
CTS_T1_data$CTQ_Emotional_Neglect <- rowSums(CTS_T1_data[,paste("CTQ",c("5R","7R","13R","19R","28R"),sep="")], na.rm = FALSE)
# Calculate physical neglect scale.
CTS_T1_data$CTQ_Physical_Neglect <- rowSums(CTS_T1_data[,paste("CTQ",c(1,"2R",4,6,"26R"),sep="")], na.rm = FALSE)
# Calculate total CTQ score.
CTS_T1_data$CTQ_Total <- rowSums(CTS_T1_data[,paste("CTQ",c(1, "2R", 3:4,"5R",6,"7R",8:9, 11:12,"13R", 14:15, 17:18, "19R", 20:21, 23:25, "26R", 27, "28R"),sep="")], na.rm = FALSE)

# Order dataframe by participant ID value.
CTS_T1_data <- CTS_T1_data[order(CTS_T1_data$CTS_ID),]

# Save the data to the data folder.
write.csv(CTS_T1_data,file.path(data_path,"CTS_CTQ_T1_Scored.csv"), row.names=FALSE)