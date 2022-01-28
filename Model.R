
library(corrplot)
data<- read_csv("finaldata.csv", col_types = cols(X1 = col_skip()))




#only data wanna see for correlation matrix to double check numerical non dummy variables not cooreated
#Can only do a few amount at a time.
data_num <- data[,3:20]
data_num.cor = cor(data_num, method = c("spearman"))

corrplot(data_num.cor)

print(cor(data$nettuition01,data$fte_count))
print(cor(data$nettuition01,data$total03_revenue))
