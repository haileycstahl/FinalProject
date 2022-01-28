library(tidyverse)

#getting rid of variables not wanting to use for model
datam<-subset(data,select=-c(academicyear,instname))

#Renaming for an ID column 
#

View(datam)

