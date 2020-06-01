#####################   Exploratory Data Analysis ##############################################
# Data Exploratory
# Data Preparation
# Spliting Data into Train nd Test data using Random sampling
 
setwd("C:\\Users\\Home\\Desktop\\Dataset\\data sheets")
cr<-read.csv("Credit.csv",na.strings = c("",NA))
library(dplyr)  #for Manuplation
options(scipen=999)  #switch off Scintific Notation in terms of number Notation

## Data Exploration using Credit Data Set
  #Sanity check 
  #Identifying Outiliars ,Replace them
  #Impute Missing Values
  #Bin Data Using -Quantile function,ntile() foir binning
  # Partioning Data into Train nd Test

names(cr)

#Removing Duplicate columns :Giving Same Information
#column 1: NPA STATUS         (Good_Bad)
#COlumn 12 :MonthlyIncome.1   (MonthlyIncome)
cr<-cr[,-c(1,12)]   # [,column oprn]

names(cr)

#Sanity check.
#Quantitative (Numeric):5points summary (Min,max,mean,median,quantile,NA's)
#Qualitative(catgorical) :Finding Freq Distribution
summary(cr)

#Missing Value Treatment.
 #DV/Target Variable :Good/Bad
 #IDV :except good/Bad column
 #Its Bad idea to impute Missing values for DV :Better to Negalt it/Delete it.
index<-which(is.na(cr$Good_Bad)) #Which column as Missing Values
index
cr<-cr[-index,] # [Row oprn ,]
cr

summary(cr) # NO NA'S in Good_Bad column



#Looking Individual Variables :

summary(cr$RevolvingUtilizationOfUnsecuredLines)
cr%>%filter(RevolvingUtilizationOfUnsecuredLines==0)%>%nrow()  #10878 having 0 values
cr%>%filter(RevolvingUtilizationOfUnsecuredLines>=0.99)%>%nrow()#14383 having equal/Greater den 0.99
#Percentile BREAKUP (quantile function used)
quantile(cr$RevolvingUtilizationOfUnsecuredLines,p=c(1:100)/100) 
#Discus vit client ,2 is limit.
cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)%>%nrow()
cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)->cr


summary(cr$age)
cr%>%filter(age==0)%>%nrow() #only one person having zero age stil having credt card(Its Data entry Mistake)
quantile(cr$age,p=(1:100)/100)
cr%>%filter(age!=0)->cr


summary(cr$Gender)
summary(cr)

#############Missing Value Treatment for Contionoues nd Catgorical Variable:####################

#Imputing Missing values for Catgorical Variable
unique(cr$NumberOfTime30.59DaysPastDueNotWorse)

table1<-table(cr$NumberOfTime30.59DaysPastDueNotWorse,cr$Good_Bad)
bad_rate<-table1[,1]/rowSums(table1)
ind2<-which(is.na(cr$NumberOfTime30.59DaysPastDueNotWorse))
table(cr$Good_Bad[ind2])/length(ind2)

cr$NumberOfTime30.59DaysPastDueNotWorse[ind2]<-6
summary(cr$NumberOfTime30.59DaysPastDueNotWorse)


#Imputing Missing Values for Continuoues Variable (By creating decile using ntile function)
summary(cr$MonthlyIncome)
library(dplyr) # to use ntile function

cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(Good_Bad,quantile)%>%summarize(N=n())%>%filter(Good_Bad=="Bad")->dat
cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(quantile)%>%summarize(N=n())->dat1

dat$Percentage<-dat$N/dat1$N

#Replace with 8 quantile
quantile(cr$MonthlyIncome,p=(0:10)/10,na.rm=T)
cr$MonthlyIncome[is.na(cr$MonthlyIncome)]<-9200
summary(cr$MonthlyIncome)


####################### Spliting Data into Train nd Test Using Random Sampling ######################################
#1.Using set.seed(100) 
#2.Using Library(caret)

#1.Using set.seed(100)  :
set.seed(100)
indexP<-sample(1:nrow(cr),0.70*nrow(cr),replace=F)
train_cr<-cr[indexP,]
test_cr<-cr[-indexP,]


##2.Using Library(caret) :
library(caret)
indexPC<-CreateDataPartition(y=cr$Good_Bad,times=1,p=0.70,list=F)
train_cr<-cr[indexPC,]
test_cr<-cr[-indexPC,]

table(train_cr$Good_Bad)/nrow(train_crC)
table(test_cr$Good_Bad)/nrow(test_crC)














