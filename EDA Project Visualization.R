dataSet <- read.csv("XYZCorp_LendingData.csv")
backUp_dataset <- read.csv("XYZCorp_LendingData.csv")
df <- data.frame(dataSet$verification_status)
df$verification_status <- dataSet$verification_status
df$home_ownership <- dataSet$home_ownership

unique(df$home_ownership)
unique(df$verification_status)

unique(df$verification_status)
df$verification_status <- as.character(df$verification_status)
unique(df$verification_status)
df$verification_status <- ifelse(df$verification_status=="Verified","Source Verified",df$verification_status)
df$verification_status <- as.factor(df$verification_status)

df$home_ownership <- as.character(df$home_ownership)
df$home_ownership <- ifelse(df$home_ownership=='NONE',"OTHER",df$home_ownership)
df$home_ownership <- ifelse(df$home_ownership=='ANY',"OTHER",df$home_ownership)
unique(df$home_ownership)
df$home_ownership <- as.factor(df$home_ownership)


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
library(quantreg)
#install.packages("naniar")
library(naniar)
#install.packages("wordcloud")# word-cloud generator 
library(wordcloud)
#install.packages("RColorBrewer") # color palettes
library(RColorBrewer)
#install.packages("tm")
library(tm)

#perfoming EDA
vis_miss(dataSet,warn_large_data = FALSE)
describe(dataSet)
summary(dataSet)
str(dataSet)
names(dataSet)
sum(is.null(dataSet))
sapply(dataSet, function(x) sum(is.na(x)))

#Trimming unwanted columns
#t <- (dataSet %>% select("loan_amnt","term","grade","sub_grade","emp_length","home_ownership","annual_inc","verification_status","pymnt_plan","purpose","dti","delinq_2yrs","inq_last_6mths","open_acc","pub_rec","revol_bal","revol_util","total_acc","initial_list_status","out_prncp","total_pymnt","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_d","last_pymnt_amnt","next_pymnt_d","last_credit_pull_d","collections_12_mths_ex_med","policy_code","application_type","annual_inc_joint","dti_joint","verification_status_joint","acc_now_delinq","default_ind"))
#dataSet_trimmed <- t
#str(t)

df <- dataSet %>% select('annual_inc','int_rate','application_type' , 'collection_recovery_fee', 'last_pymnt_amnt' ,'dti','loan_amnt' ,'funded_amnt','out_prncp' ,'funded_amnt_inv','revol_bal' , 'installment','revol_util','total_pymnt','delinq_2yrs','issue_d','earliest_cr_line','last_credit_pull_d','emp_length' , 'last_pymnt_d', 'home_ownership','next_pymnt_d' ,'initial_list_status','open_acc' , 'inq_last_6mths','sub_grade' , 'pub_rec','term','verification_status','default_ind')
sapply(df,function(x) sum(is.na(x)))
str(df)
describe(df)
#write.csv(df,"G://MY Monash//Semester 1//Data Visualization and exploration//R DE Assignment//myDataSet.csv")
onlyNumber <- select_if(dataSet,is.numeric)
unique(df$pub_rec)
onlyNumber <- onlyNumber %>% select(-"revol_util")
corrvalue <- cor(onlyNumber)
na.exclude(corrvalue)

corrplot(round((corrvalue),2), method="number", type = "lower",title = "Correlation b/w Numerical Variables")
var.test(finalData$annual_inc,finalData$loan_amnt)
chisq.test(finalData$application_type,finalData$emp_length)
chisq.test(finalData$application_type,finalData$default_ind)
chisq.test(finalData$home_ownership,finalData$default_ind,correct = T)


#Leaving All the columns that has correlation 
finalData <- df %>% select('annual_inc','application_type','sub_grade','verification_status','loan_amnt','emp_length' , 'home_ownership')
finalData$defaulters <- dataSet$default_ind


#performing Analysis on final data Set
str(finalData)
describe(finalData)
sapply(finalData,function(x) sum(is.na(x)))
table(finalData$emp_length)
table(finalData$home_ownership)
table(finalData$verification_status)

#Missing value imputation in Employee length Column
finalData$emp_length = sub("n/a",names(table(finalData$emp_length))[table(finalData$emp_length)==max(table(finalData$emp_length))],finalData$emp_length)

#Inconsistency eleminiation in Verification Status coulmn
finalData$verification_status <- as.character(finalData$verification_status)
unique(finalData$verification_status)
finalData$verification_status <- ifelse(finalData$verification_status=="Verified","Source Verified",finalData$verification_status)
finalData$verification_status <- as.factor(finalData$verification_status)

#Changing default to factor
finalData$defaulters <- as.factor(finalData$defaulters)
finalData$annual_inc <- as.integer(finalData$annual_inc)

#Elimination ANY and NONE in house OWNERSHIP
finalData$home_ownership <- as.character(finalData$home_ownership)
finalData$home_ownership <- ifelse(finalData$home_ownership=='NONE',"OTHER",finalData$home_ownership)
finalData$home_ownership <- ifelse(finalData$home_ownership=='ANY',"OTHER",finalData$home_ownership)
unique(finalData$home_ownership)
finalData$home_ownership <- as.factor(finalData$home_ownership)

boxplot(finalData$annual_inc,horizontal = T)

outlier_defaulter1 <- subset(finalData,finalData$defaulters==1)#and [finalData$annual_inc>5000000]]
outlier_defaulter <- subset(outlier_defaulter,outlier_defaulter$annual_inc > 5000000)

#Analyse in depth
salless25 <- subset(finalData,finalData$annual_inc < 2500000)

salless10 <- subset(finalData,finalData$annual_inc < 1000000)

#Performing Visualization
#Based on Annual Income
ggplot(finalData,aes(y=annual_inc)) + geom_boxplot(color='red') + facet_grid(~defaulters) + ggtitle("Annual Income VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income") + scale_x_continuous(breaks = 0)
#less than 2500000
ggplot(salless,aes(y=annual_inc)) + geom_boxplot(color='red') + facet_grid(~defaulters) + ggtitle("Annual Income VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income") + scale_x_continuous(breaks = 0)
#less than 1000000
ggplot(salless10,aes(y=annual_inc)) + geom_boxplot(color='red') + facet_grid(defaulters~home_ownership) + ggtitle("Annual Income VS Defaulters Vs Ownership")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income") + scale_x_continuous(breaks = 0)

ggplot(finalData,aes(y=annual_inc,x=defaulters)) + geom_col() 
ggplot(finalData,aes(annual_inc)) + geom_dotplot(colour='red') + facet_grid(home_ownership~defaulters)
ggplot(finalData,aes(annual_inc)) + geom_dotplot(colour='red') + facet_grid(defaulters~home_ownership)

ggplot(finalData,aes(annual_inc)) + geom_histogram() + facet_grid(defaulters~home_ownership)

ggplot(salless10,aes(annual_inc)) + geom_histogram() + facet_grid(defaulters~home_ownership)

#Application Type

# Application Type With Only Joint
ggplot(subset(finalData,finalData$application_type=="JOINT"),aes(x=application_type)) + geom_bar(aes(application_type)) + facet_grid(~defaulters) + ggtitle("Application Type(Joint) VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Count")
# Application Type With Only INDIVIDUAL
ggplot(subset(finalData,finalData$application_type=="INDIVIDUAL"),aes(x=application_type)) + geom_bar(aes(application_type)) + facet_grid(~default_ind)

ggplot(subset(salless10,salless10$application_type=="INDIVIDUAL"),aes(x=application_type)) + geom_bar(aes(application_type)) + facet_grid(~default_ind) + ggtitle("Application Type(Individual) VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Count")


#SUB_GRADE
ggplot(finalData,aes(sub_grade,annual_inc)) + geom_count() + facet_grid(~defaulters)
ggplot(finalData,aes(annual_inc,sub_grade)) + geom_col(aes(colour=home_ownership)) + facet_grid(~defaulters)
ggplot(salless10,aes(sub_grade,annual_inc)) + geom_col(aes(colour=home_ownership)) + facet_grid(~defaulters) + ggtitle("SUB_GRADE VS Defaulters")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Count Based on Sub-Grade")
ggplot(finalData,aes(annual_inc,sub_grade)) + geom_col(aes(colour=home_ownership)) + facet_grid(~defaulters)

ggplot(finalData,aes(sub_grade,annual_inc)) + geom_col() + facet_grid(home_ownership~defaulters)
ggplot(finalData,aes(sub_grade,annual_inc)) + geom_dotplot(binaxis = "y",stackdir = "center") + facet_grid(home_ownership~defaulters)
ggplot(finalData,aes(sub_grade,annual_inc)) + geom_violin(scale="area") + facet_grid(~defaulters)


#Sub grade does not have impact.there are both paid and not paid alomst count equal.
ggplot(salless10,aes(annual_inc,sub_grade)) + geom_violin(scale="area") + facet_grid(~defaulters)

#Verification Status
ggplot(finalData,aes(verification_status)) + geom_bar(aes(colour=defaulters)) + facet_grid(~defaulters)

#Loan Amount

#Better to confuse them
ggplot(salless10,aes(annual_inc,loan_amnt)) + geom_point(aes(colour=home_ownership)) + facet_grid(~defaulters)
#Difficult to confuse
ggplot(salless10,aes(loan_amnt,annual_inc)) + geom_point(aes(colour=home_ownership)) + facet_grid(default_ind~home_ownership) + ggtitle("Home Ownership & Defaulters")+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text.x=element_text(size=8, angle=90))
#Same as that of Geom Point. 
#ggplot(salless10,aes(annual_inc,loan_amnt)) + geom_jitter(aes(colour=home_ownership)) + facet_grid(~defaulters)

#Employee Length


theTable <- within(finalData, 
                   Position <- factor(finalData$emp_length, 
                                      levels=names(sort(table(finalData$emp_length), 
                                                        decreasing=TRUE))))
theTable$emp_length <- factor(theTable$emp_length,levels = c("< 1 year","1 year","2 years","3 years","4 years","5 years","6 years","7 years","8 years","9 years","10+ years"),ordered = T)
ggplot(theTable,aes(x=(theTable$emp_length)))+geom_bar()

ggplot(theTable,aes(emp_length)) + geom_bar(aes(colour=default_ind)) + geom_text(stat='count', aes(label=..count..), vjust=0) +ggtitle("Employee Length VS Defaulters")+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text.x=element_text(size=10, angle=45))
#Choose this plot
ggplot(theTable,aes(emp_length)) + geom_bar(aes(colour=home_ownership))+ geom_text(stat='count', aes(label=..count..), vjust=0) + facet_grid(home_ownership~default_ind)+ theme(axis.text.x=element_text(size=15, angle=90)) +ggtitle("Employee Length VS Defaulters")+theme(plot.title = element_text(hjust = 0.5))


#house Ownership
ggplot(finalData,aes(home_ownership)) + geom_bar(colour="blue") + facet_grid(~defaulters) + scale_colour_brewer(palette = "Set2")
ggplot(finalData,aes(home_ownership)) + geom_bar(aes(colour=verification_status)) + facet_grid(defaulters~emp_length)


treeMap <- (subset(finalData,(finalData$purpose!="debt_consolidation"))) #or finalData$purpose!="credit_card"))
treeMap <- subset(treeMap,treeMap$purpose!="credit_card")
treeMap$purpose <- factor(treeMap$purpose,levels = (unique(treeMap$purpose)))

treeMapCount <- count(treeMap$purpose)

install.packages("treemap")
library(treemap)
#ggplot(treeMap,aes(purpose)) + geom_tree
treemap(treeMapCount,index = "x",vSize = "freq",type = "index")


#Generating Word Cloud
dummy <- as.character(finalData$emp_title)
dtm <- TermDocumentMatrix(dummy)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

forWordCloud <- count(finalData$emp_title)
forWordCloud_1 <- subset(forWordCloud,forWordCloud$freq>200)

forpieChart <- count(finalData$purpose)
install.packages("plotrix")
library(plotrix)
pie3D(forpieChart$freq,labels=forpieChart$x,explode=0.1,
      main="Pie Chart of Purpose")

pie(forpieChart$freq,labels=forpieChart$x, col=rainbow(length(forpieChart$x)),
    main="Pie Chart of Purpose")

# Create a basic bar
pie = ggplot(df, aes(x="", y=share, fill=brand)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Phones - Market Share")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))

pie

# Compute the position of labels
pie_data <- df %>% 
  arrange(desc(df$labels)) %>%
  mutate(prop = slices / sum(df$slices) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(pie_data, aes(x="", y=prop, fill=labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +ggtitle("Pie chart of Purpose")+theme(plot.title = element_text(hjust = 0.5))
lbls <- paste(lbls,"%",sep="") # ad % to labels
  geom_text(aes(y = ypos, label = pie_data$labels), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")



slices <- forpieChart$freq
lbls <- forpieChart$x
pct <- round(forpieChart$freq/sum(forpieChart$freq)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
df = data.frame(slices = slices,labels =  lbls)
ggplot(df,aes(x = factor(1),fill = labels))+
  geom_bar(width = 1)+
  coord_polar(theta = "y")+
  theme(axis.title = element_blank())


library(plyr)
library(wordcloud2)
wordcloud2(forWordCloud_1,size = 5)
set.seed(1234)
wordcloud(words = finalData$emp_title, freq = count(finalData$emp_title), min.freq = 200,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))