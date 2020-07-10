###################################################################################################################################
#Reverse 4 point likert scale (from 1 to 4)
#Created by Yael Waizman
###################################################################################################################################
Reverse_code_4_likert <- function(response) {
  
  Reverse_code_4_likert = ifelse(response == 1, 4, 
                         ifelse(response == 2, 3,
                                ifelse(response == 3, 2, 1)))
  
  return(Reverse_code_4_likert)
}