library(ggplot2)
library(corrplot)
library(tidyverse)

data<- read_csv("finaldataclean.csv", 
                +     col_types = cols(X1 = col_skip()))
#getting rid of variables not wanting to use for model
datam<-subset(data,select=-c(academicyear,instname))



histogram= ggplot(data, aes(x=ftretention_rate)) +

  geom_histogram(binwidth=.05,color="white",fill ="#DFA1A1")+
  labs( x = "Retention Rate",y = "Count",size = 30)

print(histogram)

