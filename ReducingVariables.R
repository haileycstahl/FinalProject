
#Other libraries
library(dummies)
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





#Suspected variables highly correlated to remove one and keep the other

#Category tuition 
print(cor(data$tuition_reliance_a1,data$tuition_reliance_a2))
print(cor(data$tuition_reliance_a1,data$tuition_reliance_b1))
print(cor(data$tuition_reliance_a1,data$tuition_reliance_b2))
print(cor(data$tuition_reliance_a1,data$tuition_reliance_c2))
print(cor(data$tuition01_tf,data$tuitionfee01_tf))
print(cor(data$nettuition01,data$net_student_tuition))
#support
print(cor(data$acadinststud01,data$acadinstsupp01))
print(cor(data$acadinststud01,data$acadsupp02))
print(cor(data$acadinststud01,data$instruction01))
print(cor(data$acadinststud01,data$instsupp01))
print(cor(data$acadinststud01,data$instsupp02))

#total
print(cor(data$total01,data$total03_expenses))
print(cor(data$total01,data$total02))
#fee
print(cor(data$tuition02_tf,data$tuition02_tf))

#edag

print(cor(data$eandg01,data$eandg01_sum))
print(cor(data$eandg01,data$eandg02))
print(cor(data$eandg01,data$eandg03))

#ear
print(cor(data$eandg01,data$eandr))
print(cor(data$eandr_degree,data$eandr_completion))

print(cor(data$sticker_subsidy,data$average_subsidy))

#fringe

print(cor(data$fringe_benefit_play,data$fringe_benefit_play_imp))

#assets

print(cor(data$assets06,data$assets15))
print(cor(data$assets06,data$assets11))
print(cor(data$assets06,data$liabilities07))

#degrees
print(cor(data$totaldegrees,data$totalcompletions))
print(cor(data$totaldegrees,data$grad_rate_150_n))

#cohort

print(cor(data$fall_total_undergrad,data$fall_cohort_num))
print(cor(data$ft_first_time_first_yr_deg_seek,data$fall_cohort_num))
print(cor(data$ft_first_time_first_yr_deg_seek,data$other_full_time))

#partime
print(cor(data$total_part_time,data$total_part_time_undergraduates))


print(cor(data$instr_sal_as_pct_instrtot,data$labor_share_of_instructcost))
print(cor(data$acadsupp_sal_as_pct_acadsupptot,data$labor_share_of_acadsuppcost))
#Subletting again to just to make it easier to view

print(cor(data$total_full_time,data$total_part_time))
print(cor(data$total_enrollment,data$total_undergraduates))
print(cor(data$total_enrollment,data$total_undergraduates))

print(cor(data$applcn,data$applcnm))
print(cor(data$applcn,data$applcnw))

#adm

print(cor(data$admssn,data$admssnm))
print(cor(data$admssn,data$admssnw))
print(cor(data$admssn,data$admitcount))

print(cor(data$admitcount,data$enrlm))
print(cor(data$admitcount,data$applicantcount))
print(cor(data$enrlm,data$enrlt))
print(cor(data$enrlm,data$enrlw))
print(cor(data$full_time_employee_100fte,data$ft_faculty_per_100fte))
print(cor(data$ugentering,data$grscohort))

#Subsetting to new data by reducing variables
datar<-subset(data,select=-c(tuition_reliance_b1,tuition_reliance_a2,tuition_reliance_b2,
tuitionfee01_tf,tuitionfee02_tf,tuitionfee03_tf,net_student_tuition,tuition03,
acadinstsupp01,acadsupp02,instruction01,instsupp01,instsupp02,
total02,total04,total05,total03_expenses,
tuition02_tf,tuition03_tf,sticker_subsidy,sticker_price_share,
eandg01_sum,eandg02,eandg03,eandg05,eandg08,eandr,eandr_completion,eandg01_w_auxother_sum,
fringe_benefit_play_imp,assets11,assets15,assets11,
totalcompletions,totalcompletions_100fte,grad_rate_150_n,
fall_total_undergrad,ft_first_time_first_yr_deg_seek,other_full_time,
total_part_time_undergraduates,
oberegion,any_aid_pct,loan_pct,inst_grant_pct,
fee02_tf,fee03_tf,average_subsidy_share,labor_share_of_instructcost,labor_share_of_acadsuppcost,
total_part_time,total_undergraduates,applcnm,applcnw,admssnm,admssnw,admssn,admitcount,
enrlt,enrlw,enrlm,applicantcount,ft_faculty_per_100fte,actcm25,actcm75,acten25,
acten75,actmt25,actmt75,actnum,satmt25,satmt75,satnum,
satvr25,satvr75,associatedegrees,ugentering))


#1 means a public university
datar$control<-ifelse(data$control==2,1,0)

#1 is hbu school

datar$hbcu<-ifelse(data$hbcu==2,0,1)

#1 has a hospital in it

datar$medical<-ifelse(data$medical==2,0,1)

# Making state dummy variable for each state
table(data$state)


datar$AK<-ifelse(datar$state =="AK",1,0)
datar$AL<-ifelse(datar$state =="AL",1,0)
datar$AR<-ifelse(datar$state =="AR",1,0)
datar$AZ<-ifelse(datar$state =="AZ",1,0)
datar$CA<-ifelse(datar$state =="CA",1,0)                             
datar$CO<-ifelse(datar$state =="CO",1,0) 
datar$CT<-ifelse(datar$state =="CT",1,0) 
datar$DC<-ifelse(datar$state =="DC",1,0)
datar$DE<-ifelse(datar$state =="DE",1,0)
datar$FL<-ifelse(datar$state =="FL",1,0)
datar$GA<-ifelse(datar$state =="GA",1,0)
datar$HI<-ifelse(datar$state =="HI",1,0)
datar$IA<-ifelse(datar$state =="IA",1,0)
datar$ID<-ifelse(datar$state =="ID",1,0)
datar$IL<-ifelse(datar$state =="IL",1,0)
datar$IN<-ifelse(datar$state =="IN",1,0)
datar$KS<-ifelse(datar$state =="KS",1,0)
datar$KY<-ifelse(datar$state =="KY",1,0)
datar$LA<-ifelse(datar$state =="LA",1,0)
datar$MA<-ifelse(datar$state =="MA",1,0)
datar$MD<-ifelse(datar$state =="MD",1,0)
datar$MI<-ifelse(datar$state =="MI",1,0)
datar$MN<-ifelse(datar$state =="MN",1,0)
datar$MO<-ifelse(datar$state =="MO",1,0)
datar$MS<-ifelse(datar$state =="MS",1,0)
datar$MT<-ifelse(datar$state =="MT",1,0)
datar$NC<-ifelse(datar$state =="NC",1,0)
datar$ND<-ifelse(datar$state =="ND",1,0)
datar$NE<-ifelse(datar$state =="NE",1,0)
datar$NH<-ifelse(datar$state =="NH",1,0)
datar$NJ<-ifelse(datar$state =="NJ",1,0)
datar$NV<-ifelse(datar$state =="NV",1,0)
datar$NY<-ifelse(datar$state =="NY",1,0)
datar$OH<-ifelse(datar$state =="OH",1,0)
datar$OK<-ifelse(datar$state =="OK",1,0)
datar$OR<-ifelse(datar$state =="OR",1,0)
datar$PA<-ifelse(datar$state =="PA",1,0)
datar$PR<-ifelse(datar$state =="PR",1,0)
datar$RI<-ifelse(datar$state =="RI",1,0)
datar$SC<-ifelse(datar$state =="SC",1,0)
datar$SD<-ifelse(datar$state =="SD",1,0)
datar$TN<-ifelse(datar$state =="TN",1,0)
datar$TX<-ifelse(datar$state =="TX",1,0)
datar$UT<-ifelse(datar$state =="UT",1,0)
datar$VA<-ifelse(datar$state =="VA",1,0)
datar$VI<-ifelse(datar$state =="VI",1,0)
datar$VT<-ifelse(datar$state =="VT",1,0)
datar$WA<-ifelse(datar$state =="WA",1,0)
datar$WI<-ifelse(datar$state =="WI",1,0)
datar$WV<-ifelse(datar$state =="WV",1,0)
datar$WY<-ifelse(datar$state =="WY",1,0)

datar<-subset(datar,select=-c(state))

write.csv(datar, "finaldata.csv")