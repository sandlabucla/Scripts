###################################################################################################################################
#Physical Activity scale (PA)
#Created by Yael Waizman
###################################################################################################################################

###Calculate mean for all PA items
PA <- function(dataframe){
  dataframe$PA_mean <- rowMeans(dataframe[,paste("PA_", c(1:3), sep="")], na.rm = TRUE)
  return(dataframe)
}