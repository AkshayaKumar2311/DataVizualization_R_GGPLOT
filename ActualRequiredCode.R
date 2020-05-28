FulldataSet <- read.csv("XYZCorp_LendingData.csv")



#Package installation for EDA
#install.packages("Hmisc")
library(Hmisc)
#install.packages("psych")
library(psych)
#install.packages("dplyr")
library(dplyr)
#install.packages("magrittr")
library(magrittr)
#install.packages("corrplot")
library(corrplot)
#install.packages("quantreg")
library(quantreg)
#install.packages("naniar")
library(naniar)
#install.packages("hrbrthemes")
library(hrbrthemes)
#install.packages("wordcloud")# word-cloud generator 
library(wordcloud)
#install.packages("RColorBrewer") # color palettes
library(RColorBrewer)
#install.packages("tm")
library(tm)
library(ggplot2)
library(scales)

#perfoming EDA
vis_miss(FulldataSet,warn_large_data = FALSE)
describe(FulldataSet)
summary(FulldataSet)
str(FulldataSet)
names(FulldataSet)
sum(is.null(FulldataSet))
sapply(FulldataSet, function(x) sum(is.na(x)))


df <- FulldataSet %>% select('annual_inc','int_rate','application_type' , 'collection_recovery_fee', 'last_pymnt_amnt' ,'dti','loan_amnt' ,'funded_amnt','out_prncp' ,'funded_amnt_inv','revol_bal' , 'installment','revol_util','total_pymnt','delinq_2yrs','issue_d','earliest_cr_line','last_credit_pull_d','emp_length' , 'last_pymnt_d', 'home_ownership','next_pymnt_d' ,'initial_list_status','open_acc' , 'inq_last_6mths','sub_grade' , 'pub_rec','term','verification_status','default_ind')
sapply(df,function(x) sum(is.na(x)))
str(df)
describe(df)
#write.csv(df,"G://MY Monash//Semester 1//Data Visualization and exploration//R DE Assignment//myDataSet.csv")
onlyNumber <- select_if(df,is.numeric)
unique(df$pub_rec)
onlyNumber <- onlyNumber %>% select(-"revol_util")
corrvalue <- cor(onlyNumber)
corrplot(round(corrvalue,2), method="number", type = "lower",title = "Correlation b/w Numerical Variables")


df <- FulldataSet %>% select('annual_inc','application_type','loan_amnt' , 'emp_length', 'home_ownership' ,'sub_grade','verification_status','purpose','title' ,'emp_title','addr_state' , 'default_ind')

#Verification Status :
unique(df$verification_status)
df$verification_status <- as.character(df$verification_status)
unique(df$verification_status)
df$verification_status <- ifelse(df$verification_status=="Verified","Source Verified",df$verification_status)
df$verification_status <- as.factor(df$verification_status)

#Employee Length
unique(df$emp_length)
df$emp_length = sub("n/a",names(table(df$emp_length))[table(df$emp_length)==max(table(df$emp_length))],df$emp_length)

#Defaulters
df$default_ind <- as.factor(df$default_ind)
#Annual Income
df$annual_inc <- as.integer(df$annual_inc)


#Home_ownership
df$home_ownership <- as.character(df$home_ownership)
df$home_ownership <- ifelse(df$home_ownership=='NONE',"OTHER",df$home_ownership)
df$home_ownership <- ifelse(df$home_ownership=='ANY',"OTHER",df$home_ownership)
unique(df$home_ownership)
df$home_ownership <- as.factor(df$home_ownership)

finalData <- df

#Annual Income
ggplot(finalData,aes(y=annual_inc)) + geom_boxplot(color='red') + facet_grid(~default_ind) + ggtitle("Annual Income VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income") + scale_x_continuous(breaks = 0)
salless10 <- subset(finalData,finalData$annual_inc < 1000000)
ggplot(subset(salless10,salless10$annual_inc<250000),aes(y=annual_inc)) + geom_boxplot(aes(colour=home_ownership)) + facet_grid(default_ind~home_ownership) + ggtitle("Annual Income & Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income") + scale_x_continuous(breaks = 0)

outlier_defaulter1 <- subset(finalData,finalData$default_ind==1)#and [finalData$annual_inc>5000000]]
outlier_defaulter <- subset(outlier_defaulter1,outlier_defaulter1$annual_inc > 5000000)

#Application Type
table(finalData$application_type) / sum(table(finalData$application_type))
table(finalData$application_type)

#Application Type

# Application Type With Only Joint
ggplot(subset(finalData,finalData$application_type=="JOINT"),aes(x=application_type)) + geom_bar(aes(application_type)) + facet_grid(~default_ind) + ggtitle("Application Type(Joint) VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Count") + geom_text(stat='count', aes(label=..count..), vjust=2,colour="white")
# Application Type With Only INDIVIDUAL
ggplot(subset(finalData,finalData$application_type=="INDIVIDUAL"),aes(x=application_type)) + geom_bar(aes(application_type)) + facet_grid(~default_ind)

ggplot(subset(salless10,salless10$application_type=="INDIVIDUAL"),aes(x=application_type)) + geom_bar(aes(application_type)) + facet_grid(~default_ind) + ggtitle("Application Type(Individual) VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Count") + geom_text(stat='count', aes(label=..count..), vjust=1,colour="white")


#Employee Length
ggplot(salless10,aes(emp_length)) + geom_bar(aes(colour=home_ownership))+ geom_text(stat='count', aes(label=..count..), vjust=0) + facet_grid(home_ownership~default_ind)

ggplot(finalData,aes(emp_length)) + geom_bar(aes(colour=purpose))+ geom_text(stat='count', aes(label=..count..), vjust=0) + facet_grid(home_ownership~default_ind)

ggplot(finalData,aes(emp_length)) + geom_bar(aes(colour=purpose))+ geom_text(stat='count', aes(label=..count..), vjust=0) + facet_grid(~default_ind)
ggplot(finalData,aes(emp_length)) + geom_bar(aes(colour=default_ind))+ geom_text(stat='count', aes(label=..count..), vjust=0) + ggtitle("Employment Length Vs Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Count") #+ facet_grid(~default_ind)
ggplot(subset(finalData,finalData$emp_length=="10+ years"),aes(emp_length)) + geom_bar(aes(colour=purpose))+ geom_text(stat='count', aes(label=..count..), vjust=0) + ggtitle("10+Years Vs Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Count") + facet_grid(default_ind~purpose)


# Plot
# salless10 %>%
#   ggplot( aes(emp_length),count(emp_length)) +
#   geom_line( color="grey") +
#   geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
#   theme_ipsum() +
#   ggtitle("XXX")






#Loan Amount

#Better to confuse them
ggplot(salless10,aes(loan_amnt,annual_inc)) + geom_point(aes(colour=home_ownership)) + facet_grid(~default_ind) + ggtitle("House Ownership Vs Defaulters")+theme(plot.title = element_text(hjust = 0.5))

#purpose
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
ggplot((finalData),aes(x="",y=purpose)) + geom_bar(stat="identity") + coord_polar("y", start=0) +
  scale_fill_brewer(palette="Dark2") + 
  blank_theme +theme(axis.text.x=element_blank())+ ggtitle("Loan Puropse")+theme(plot.title = element_text(hjust = 0.5))


