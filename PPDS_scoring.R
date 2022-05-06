########################################################################################## 
#Petersen Physical Development Scale (PPDS) Female and Male questionnaire scoring
#Created by Yael Waizman
########################################################################################## 

#NOTE: Calculations are based on Tottenham SB study SPSS syntax (i.e. previous waves of this study)
PPDS <- function(dataframe){
  ###Calculate Female pubertal stage 
  #NOTE: PPDS_F_6_MENSTRU is coded as NO = 1 (i.e. pre-menarche), YES = 4 (i.e. post-menarche) (not the typical No = 0, Yes = 1), in order to correctly calculate a mean on the scale 1-4. 
  dataframe$PPDS_F_STAGE <- rowMeans(dataframe[,paste("PPDS_F_", c("1","2","3","4","6_MENSTRU"),sep="")], na.rm = TRUE)
  
  ###Calculate Male pubertal stage
  dataframe$PPDS_M_STAGE <- rowMeans(dataframe[,paste("PPDS_M_", c(1:5),sep="")], na.rm = TRUE)
  
  return(dataframe)
}