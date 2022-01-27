
#Other libraries

library(ggfortify)
library(readxl)
library(dplyr)     
#viz libraries
library(ggplot2)
library(corrplot)
data <- read_excel("C:/Users/haile/OneDrive - Texas Lutheran University/Desktop/GitProjectsTLU/Project1/CLEANEDDATASET.xlsx")
View(data)
# Median and Mean together, meaning no outliers
# Spread across
# y outcome trying to measure
summary(data$ftretention_rate)

#Vizulize y outcome
histogram= ggplot(data, aes(x=ftretention_rate)) +    
  
geom_histogram(binwidth=.05,color="white",fill ="#DFA1A1")+
  labs( x = "Retention Rate",y = "Count",size = 30)

print(histogram)





cor(data$tuition_reliance_a1,data$tuition_reliance_a2)
