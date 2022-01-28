
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

data_num2<-data[,21:40]
data_num2.cor = cor(data_num2, method = c("spearman"))
corrplot(data_num2.cor)


data_num3<-data[,41:55]
data_num3.cor = cor(data_num3, method = c("spearman"))
corrplot(data_num3.cor)


data_num4<-data[,56:65]
data_num4.cor = cor(data_num4, method = c("spearman"))
corrplot(data_num4.cor)

data_num5<-data[,65:72]
data_num5.cor = cor(data_num5, method = c("spearman"))
corrplot(data_num5.cor)



#checking correlation with variables to remove them

#removing fte_count
print(cor(data$nettuition01,data$fte_count))
#removing total revenue3
print(cor(data$nettuition01,data$total03_revenue))

#removing adminshare
print(cor(data$instruction_share,data$admin_share))

#removing all_employees
print(cor(data$all_employees,data$salarytotal))

#removing all tuition reliance except one

print(cor(data$tuition_reliance_a1,data$tuition_reliance_c2))

#removing grant 7
print(cor(data$grant07,data$appliedaid01))

#removing returning to total undergraduate
print(cor(data$returning_to_total_undergraduate,data$fall_cohort_pct))


#removing depreciation
print(cor(data$depreciation01,data$acadinststud01))

#removing student serv01 and stud servshare

print(cor(data$acadinststud01,data$studserv01))

#removing total 01
print(cor(data$acadinststud01,data$total01))

#removing eandg01
print(cor(data$acadinststud01,data$eandg01))
# removing other related cost
# #removing acadinstud01
print(cor(data$acadinststud01,data$other_ed_related_cost))
print(cor(data$acadinststud01,data$nettuition01))

#after removing going to look at the matrix again.


data2<-subset(data,select=-c(fte_count,total03_revenue,admin_share,
all_employees,tuition_reliance_c2,tuition_reliance_c1,
grant07,returning_to_total_undergraduate,depreciation01,studserv01,
total01,eandg01,grscohort,total_full_time_undergraduates,total_full_time,
totaldegrees,acadsupp01,liabilities07,total_enrollment,acadinststud01,
other_ed_related_cost,salarytotal))


#_________________________________
#Looking at new correlation matrix cleaned up
#Want only diagnol line of dark blue/Red

data_num1 <- data2[,7:20]
data_num1.cor = cor(data_num1, method = c("spearman"))
corrplot(data_num1.cor)



data_num2<-data2[,21:40]
data_num2.cor = cor(data_num2, method = c("spearman"))
corrplot(data_num2.cor)


data_num3<-data2[,41:50]
data_num3.cor = cor(data_num3, method = c("spearman"))
corrplot(data_num3.cor)

write.csv(data2, "finaldataclean.csv")

