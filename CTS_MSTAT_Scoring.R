# This script scores the MSTAT-II (Ambiguity Tolerance Scale).
# Written by Natalie Saragosa-Harris.
# October 2021.

# There should be thirteen items. Check to make sure they are in the correct order (i.e., same order as McClain 2009).
# In this scale, greater scores indicate greater tolerance of ambiguity.
# The items are labeled based on five "stimulus types": 
# (G) ambiguous stimuli in general.
# (C) complex stimuli.
# (U) uncertain stimuli.
# (N) new/unfamiliar/novel stimuli.
# (I) insoluble/illogical/irreducible/internally inconsistent stimuli.
# Participants responded to each item using a five point Likert scale
# (1) Strongly disagree (2) Disagree (3) Neutral (4) Agree (5) Strongly agree.

library(readr)
library(tidyr)
library(dplyr)
library(glue)

# Define function for reverse scoring.
Reverse_code_5_likert <- function(response) {
  
  Reverse_code_5_likert = ifelse(response == 1, 5, 
                                 ifelse(response == 2, 4,
                                        ifelse(response == 3, 3,
                                               ifelse(response == 4, 2,1))))
  return(Reverse_code_5_likert)
}



# Read in data.
data_path <- '/Users/nataliesaragosa-harris/Box/Natalie./UCLA./Lab./Studies/CollegeTransitionStudy/data/questionnaires/mstat'
mstat_data <- read.csv(file.path(data_path,'CTS_T1_MSTAT_Responses.csv'),header=TRUE)
# View(mstat_data)

# Clean the datafile by getting rid of the rows and columns that are unecessary.
mstat_data <- mstat_data %>% select(StartDate,CTS_ID:colnames(mstat_data)[length(colnames(mstat_data))]) # Save StartDate, and then save from CTS_ID on.
mstat_data <- mstat_data %>% select(CTS_ID, everything()) # Make CTS_ID the first column.
mstat_data <- mstat_data[-1,] # Once you have verified that the questions are in the correct order (e.g., that MSTAT_II_1 is the first question on the scoring guide), remove the row with question text.

# Get day when they filled out the questionnaires.
# Note: This will give a warning about not knowing which time zone to use, but can ignore since we are not looking at time of day.
mstat_data$date <- as.POSIXct(strptime(mstat_data$StartDate, "%m/%d/%y %H:%M"))
mstat_data$date_standard<- as.Date(mstat_data$date, "%m/%d/%y") #standardize date format.
mstat_data$timepoint = "T1"

# Change the order of the columns.
mstat_data <- mstat_data %>% select(CTS_ID, StartDate,date,date_standard,timepoint, everything())

# Make the MSTAT columns (and none of the others) numeric.
mstat_data  <- mstat_data  %>% mutate(across(contains("MSTAT"),as.numeric))

### Before adding the items, reverse code the ones that need to be reverse coded.
# Items to be reverse coded: 1, 2, 3, 4, 5, 6, 9, 11, 12.
reverse_items <- c("MSTAT_II_1","MSTAT_II_2","MSTAT_II_3","MSTAT_II_4",
                   "MSTAT_II_5","MSTAT_II_6","MSTAT_II_9","MSTAT_II_11","MSTAT_II_12")

for(item in reverse_items){
  # Create a new column with 'R' appended to the name (to indicate reverse scoring) and save reverse coded response to it.
  mstat_data[,glue('{item}_R')] <- Reverse_code_5_likert(mstat_data[,item])
  mstat_data[,item] <- NULL # To avoid unintentionally using the old (unreversed scored) version, remove it from the data frame.
}

# Using the scoring guide, calculate the totals for the five different stimulus types.
mstat_data$mstat_general_ambiguous_stimuli <- rowSums(mstat_data[,paste('MSTAT_II_',c('1_R','3_R','7','11_R','13'),sep="")], na.rm = FALSE) # items 1, 3, 7, 11, and 13.
mstat_data$mstat_complex_stimuli <- rowSums(mstat_data[,paste('MSTAT_II_',c('6_R','8'),sep="")], na.rm = FALSE) # items 6 and 8.
mstat_data$mstat_uncertain_stimuli <- mstat_data$MSTAT_II_12_R # item 12 (no adding necessary because it is a single item).
mstat_data$mstat_new_unfamiliar_novel_stimuli <- rowSums(mstat_data[,paste('MSTAT_II_',c('4_R','10'),sep="")], na.rm = FALSE) # items 4 and 10.
mstat_data$mstat_insoluble_illogical_irreducible_internally_inconsistent_stimuli <- rowSums(mstat_data[,paste('MSTAT_II_',c('2_R','5_R','9_R'),sep="")], na.rm = FALSE)  # items 2, 5, and 9.

# Calculate total scores.
mstat_data$mstat_total_score <- rowSums(mstat_data[,c('mstat_general_ambiguous_stimuli',
                                                      'mstat_complex_stimuli',
                                                      'mstat_uncertain_stimuli',
                                                      'mstat_new_unfamiliar_novel_stimuli',
                                                      'mstat_insoluble_illogical_irreducible_internally_inconsistent_stimuli')], na.rm = FALSE)

# Save the data to the data folder.
write.csv(mstat_data,file.path(data_path,"CTS_MSTAT_Scored.csv"), row.names=FALSE)
