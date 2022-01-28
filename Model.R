library(tidyverse)

data<- read_csv("finaldataclean.csv", 
                          +     col_types = cols(X1 = col_skip()))
#getting rid of variables not wanting to use for model
datam<-subset(data,select=-c(academicyear,instname))

#Renaming for an ID column 
#

View(datam)

