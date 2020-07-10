###################################################################################################################################
#Reverse codes 5 point likert scale (from 1 to 5)
#Created by Yael Waizman
###################################################################################################################################
Reverse_code_5_likert <- function(response) {
  
  Reverse_code_5_likert = ifelse(response == 1, 5, 
                                 ifelse(response == 2, 4,
                                        ifelse(response == 3, 3,
                                               ifelse(response == 4, 2,1))))
  return(Reverse_code_5_likert)
}