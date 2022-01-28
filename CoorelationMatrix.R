
library(corrplot)
data<- read_csv("finaldata.csv", col_types = cols(X1 = col_skip()))

View(data)


#only data wanna see for correlation matrix to double check numerical non dummy variables not cooreated
#Can only do a few amount at a time.
#same variable columns are next to each other in the dataset
data_num <- data[,3:6]
data_num.cor = cor(data_num, method = c("spearman"))

corrplot(data_num.cor)


data_num1 <- data[,7:20]
data_num1.cor = cor(data_num1, method = c("spearman"))

corrplot(data_num1.cor)

print(cor(data$nettuition01,data$fte_count))
print(cor(data$nettuition01,data$total03_revenue))
data_num2<-data[,21:40]
data_num2.cor = cor(data_num2, method = c("spearman"))
corrplot(data_num2.cor)

print(cor(data$instruction_share,data$admin_share))

#getting rid of admin share,other_ed_relatedcost
#


data_num3<-data[,41:55]
data_num3.cor = cor(data_num3, method = c("spearman"))
corrplot(data_num3.cor)


data_num4<-data[,56:65]
data_num4.cor = cor(data_num4, method = c("spearman"))
corrplot(data_num4.cor)

data_num5<-data[,65:72]
data_num5.cor = cor(data_num5, method = c("spearman"))
corrplot(data_num5.cor)



#Removing Varialbes that have high coorelation with others. 
#
#