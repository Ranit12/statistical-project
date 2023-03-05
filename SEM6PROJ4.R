#Installing required packages
install.packages('tidyverse') #for data manipulation
install.packages('dummies') #for creating dummy variables
install.packages('ggpubr') #would be used to arrange plots on a single page with ggarange
install.packages('caTools') #For randomly splitting data into test and train
install.packages('rpart') #for creating a decision tree model
install.packages('rpart.plot') #for plotting the decision tree
install.packages('ggplot2') #for boxplot and necessary diagrams
install.packages('ggrepel')
install.packages('reshape2')
install.packages("ggExtra")
install.packages('GGally')
install.packages('hrbrthemes')
install.packages("writexl")


library(tidyverse)
library(dummies)
library(ggpubr)
library(caTools) 
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(ggExtra)
library(GGally)
library(hrbrthemes)
library(writexl)

#Getting our data
Data = read.csv("C:/Users/RANIT/OneDrive/Desktop/RANIT PROJECT/Medicalpremium.csv",header=T)

glimpse(Data)

# Loading the data set
View(Data)

# ------------------------------------------
## EXPLORING THE DATA SET 
# ------------------------------------------

# Getting the Column Names
names(Data)

# Displaying top 10 Rows of the data set
head(Data,n=10)

# Displaying bottom 20 Rows of the data set
tail(Data,n=10)
Values
sum(duplicated(Data))
# Getting the Structure of the whole data set
str(Data)

# Getting the Summary of the whole data set
summary(Data)

#Boxplot

heightout<- ggplot(data  = Data, aes(x = Height))+geom_boxplot()

weightout <- ggplot(data = Data, aes(x = Weight))+geom_boxplot()

premiumpriceout <- ggplot(data = Data, aes(x = PremiumPrice))+geom_boxplot()

ggarrange(heightout, weightout, premiumpriceout,labels = c("Height", "Weight", "PremiumPrice"))


boxplot(Data$Weight)
boxplot(Data$Weight, plot= FALSE)$out
boxplot(x)
boxplot(Data$PremiumPrice)

boxplot(Data$PremiumPrice)
outlier <- boxplot(Data$PremiumPrice,plot= FALSE)$out
summary(newData)
boxplot(Data)
#remoivng the outlier 
x<-rnorm(1000)
x[1:16]<-c(118,121,119,129,127,132,120,128,120,23,126,121,118,128,124,122)
x_out_rm<-x[!x%in%boxplot.stats(x)$out]
boxplot(x_out_rm)
outlier <- boxplot(Data$Weight,plot= FALSE)$out
outlier
newData3 <- Data[-which(Data$PremiumPrice %in% outlier),]
boxplot(Data$Weight1)
summary(newData3$PremiumPrice)
boxplot(newData3$PremiumPrice)
# Checking missing Values
table(is.na(Data))

# Checking null values
is.null(Data)

# Creating frequency table with respect to the 'Diabetes' column
Diabetes_freq <- table(Data$Diabetes)

# Finding percentages of 'Yes-No'
percentageYN <- round(Diabetes_freq/986*100,digits=2)

# Drawing a Pie Chart for "Diabetes"
pie(Diabetes_freq,labels = paste(names(Diabetes_freq),percentageYN,"%",sep = " "),main = "Distribution of Diabetes",border="black",col = c(2,5))

# Creating frequency table with respect to the 'BloodPressureProblems' column
BloodPressureProblems_freq <- table(Data$BloodPressureProblems)

# Finding percentages of 'Yes-No'
percentageYN <- round(BloodPressureProblems_freq/986*100,digits=2)

# Drawing a Pie Chart for "BloodPressureProblems"
pie(BloodPressureProblems_freq,labels = paste(names(BloodPressureProblems_freq),percentageYN,"%",sep = " "),main = "Distribution of BloodPressureProblems",border="black",col = c(3,2))

# Creating frequency table with respect to the 'HistoryOfCancerInFamily' column
HistoryOfCancerInFamily_freq <- table(Data$HistoryOfCancerInFamily)

# Finding percentages of 'Yes-No'
percentageYN <- round(HistoryOfCancerInFamily_freq/986*100,digits=2)

# Drawing a Pie Chart for "HistoryOfCancerInFamily"
pie(HistoryOfCancerInFamily_freq,labels = paste(names(HistoryOfCancerInFamily_freq),percentageYN,"%",sep = " "),main = "Distribution of HistoryOfCancerInFamily",border="black",col =c(4,5))

ggplot(data = Data, aes(x= NumberOfMajorSurgeries,y=PremiumPrice))+geom_bar()+ geom_text(aes(label = ..count..), stat = 'count', vjust = 1.2, colour = 'white')+labs(title = 'Distribution of PremiumPrice amongst NumberOfMajorSurgeries ',y = 'PremiumPrice')

ggplot(data = Data, aes(x= NumberOfMajorSurgeries,y=PremiumPrice))+geom_bar()+ geom_text(aes(label = ..count..), stat = 'count', vjust = 1.2, colour = 'white')+labs(title = 'Distribution of PremiumPrice amongst NumberOfMajorSurgeries ',y = 'PremiumPrice')
rlang::last_error()
ggplot(data = Data, aes(x= NumberOfMajorSurgeries, fill= NumberOfMajorSurgeries))+geom_bar()+facet_grid(~PremiumPrice) + geom_text(aes(label = ..count..), stat = 'count',vjust = 1.2, colour = 'white')+labs(title = 'Distribution of NumberOfMajorSurgeries',y = 'Frequency')


Data5 <- Data 

Data5$AgeGroup[Data5$Age >10 & Data5$Age <19 ] <- '10 - 19' 

Data5$AgeGroup[Data5$Age >20 & Data5$Age <29 ] <- '20 - 29' 

Data5$AgeGroup[Data5$Age >30 & Data5$Age <39 ] <- '30 - 39' 

Data5$AgeGroup[Data5$Age >40 & Data5$Age <49 ] <- '40 - 49' 

Data5$AgeGroup[Data5$Age >50 & Data5$Age <59 ] <- '50 - 59' 

Data5$AgeGroup[Data5$Age >60 & Data5$Age <69 ] <- '60 - 69'
head(Data5)

ggplot(Data = hdf, aes(x= Sex, fill= Sex))+geom_bar()+  facet_grid(~ChestPainType) +  geom_text(aes(label = ..count..), stat = 'count', vjust = 1.2, colour = 'white')+labs(title = 'Distribution of chest pain type between Male and Female',y = 'Frequency')

#Creating the correlation matrix
ggcorr(Data, label = T, color = "black", size = 5)+labs(title = "Correlation Matrix")+theme(plot.title = element_text(family = "Roboto Condensed", size = 19, face = "bold",vjust = 1),plot.subtitle = element_text(family = "Roboto Condensed", size = 16,vjust = 0)) 

plot(Data)

#Creating scatter plot between premium price and age
ggplot(data=Data,aes(x=Age, y=PremiumPrice, colour= as.character(HeartDisease)))+geom_point()+labs(title='Heart Disease and Blood Pressure', x='Resting Blood Pressure', y='Maximum Heart Rate')
# Applying factors to ‘Age’ column
 AGE <- as.factor(Data$Age)

#Density plot
ggplot(Data,aes(PremiumPrice))+geom_density(aes(fill=AGE),color=NA, alpha=0.6)+labs(title = " Density plot for distribution of different ages ")

• Creating histogram for distribution of Age
AGE = Data$Age
ggplot(data.frame(Data$Age), aes(x=AGE)) + geom_histogram(aes(y=..density..), fill="grey",alpha=0.5)+ geom_density(alpha=.2,col="black",size=2) + labs(title="Distribution of AGE")
• Distribution of BMI
# Assigning the ‘bmi values to a variable 
BMI = Data$bmi
 # Creating histogram for distribution of BMI 
ggplot(data.frame(Data$bmi), aes(x=BMI)) + geom_histogram(aes(y=..density..),fill="orange",alpha=0.5) + geom_density(alpha=.2,col="orange",size=2) + labs(title="Distribution of BMI")

# Assigning the ‘PremiumPrice’ values to a variable 
PREMIUMPRICE = Data$PremiumPrice
 
# Creating histogram for distribution of PREMIUMPRICE
ggplot(data.frame(Data$PremiumPrice), aes(x=PREMIUMPRICE)) + 
geom_histogram(aes(y=..density..),fill="light pink",alpha=0.5) + 
geom_density(alpha=.2,col="violet",size=2) + labs(title="Distribution of 
PREMIUMPRICE")

ggplot(Data,aes(x= PremiumPrice,y=bmi,color=Diabetes,size=Age)) + geom_point(alpha = .6) + labs(title = "Relation of PremiumPrice with Bmi, Age, and Diabetes")

boxplot(Data$ PremiumPrice ~Data$ HistoryOfCancerInFamily, xlab = " HistoryOfCancerInFamily ",ylab = " PremiumPrice ", main = "Box Plot",col=c("light green","violet"))

boxplot(Data$ PremiumPrice ~Data$KnownAllergies, xlab = " KnownAllergies ",ylab = " PremiumPrice ", main = "Box Plot",col=c("light green","violet"))

boxplot(Data$ PremiumPrice ~Data$ AnyChronicDiseases, xlab = " AnyChronicDiseases ",ylab = " PremiumPrice ", main = "Box Plot",col=c("light blue","green"))

boxplot(Data$ PremiumPrice ~Data$AnyTransplants, xlab = " AnyTransplants ",ylab = " PremiumPrice ", main = "Box Plot",col=c("violet","light blue"))

boxplot(Data$ PremiumPrice ~Data$BloodPressureProblems, xlab = " BloodPressureProblems ",ylab = " PremiumPrice ", main = "Box Plot",col=c("violet","orange"))

boxplot(Data$ PremiumPrice ~Data$Diabetes, xlab = " Diabetes ",ylab = " PremiumPrice ", main = "Box Plot",col=c("yellow","pink"))










# Creating histogram for distribution of PREMIUMPRICE
ggplot(data.frame(Data$PremiumPrice), aes(x=log(PREMIUMPRICE)))+ 
geom_histogram(aes(y=..density..),fill="yellow",alpha=0.5) + 
geom_density(alpha=.01,col="light green",size=2) + labs(title="Distribution 
of PREMIUMPRICE after log transformation")

AGE = Data$Age
cor(Data)

Data$Diabetes <- as.factor(Data$Diabetes)
Data$BloodPressureProblems <- as.factor(Data$BloodPressureProblems)
Data$AnyTransplants <- as.factor(Data$AnyTransplants)
Data$AnyChronicDiseases <- as.factor(Data$AnyChronicDiseases)
Data$KnownAllergies <- as.factor(Data$KnownAllergies)
Data$HistoryOfCancerInFamily <- as.factor(Data$HistoryOfCancerInFamily)
Data$NumberOfMajorSurgeries <- as.factor(Data$NumberOfMajorSurgeries)

str(Data)

#Calculatting BMI
Data$bmi <- 10000*(Data$Weight/(Data$Height)^2)
 
summary(Data$bmi)
Data <- data %>%mutate( bmiCategory = case_when(
                                  bmi<18.49999 ~ "under weight",
                                  bmi>18.5 & bmi<24.99999 ~ "normal weight",
                                  bmi>25 & bmi<29.99999 ~ "over weight",
                                  bmi>30 ~ "obesity"))

#Average Difference in Premium Prices for Diabetic and Non-Diabetic People
data %>% 
  select(Diabetes,PremiumPrice) %>%
  group_by(Diabetes) %>% 
  summarise( PremiumPrice = mean(PremiumPrice)) %>% 
  ggplot(.,aes(Diabetes,PremiumPrice))+
  geom_bar(stat = "identity",width = 0.4, fill = "#56B4E9", alpha = 0.6)+
  labs(title = "Bar plot for Diabetics People")

ggplot(Data, aes(PremiumPrice))+
  geom_density(aes(fill = Diabetes), color = NA, alpha = 0.6)+
  labs(title = "Density plot for Diabetics and Non-diabetic proples")

# Creating Boxplots for PremiumPrice ~ NumberOfMajorSurgeries 
boxplot(Data$PremiumPrice~Data$NumberOfMajorSurgeries, xlab = " Number Of Major Surgeries", ylab = "PremiumPrice", main = "Box Plot",col=c("light blue","orange","yellow","light green"))

ggplot(Data,aes(x=bmi,y=PremiumPrice))+geom_point(col="orange")+geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+labs(title="Scatter Plot -- PremiumPrice ~ bmi")


Data$PremiumPrice <- as.factor(Data$PremiumPrice)
summary(Data)


sample <- sample.split(Data$PremiumPrice, SplitRatio = 0.75)
train <- subset(Data, sample == TRUE)
test <- subset(Data, sample == FALSE)
dim(train)
dim(test)
View(test)



attach(Data)
install.packages("car")
library(car)

t.test(PremiumPrice~Diabetes,mu=0,conf=0.95,var.eq=F,Paired=F)

t.test(PremiumPrice~BloodPressureProblems,mu=0,conf=0.95,var.eq=F,Paired=F)

t.test(PremiumPrice~AnyTransplants,mu=0,conf=0.95,var.eq=F,Paired=F)

t.test(PremiumPrice~AnyChronicDiseases,mu=0,conf=0.95,var.eq=F,Paired=F)

t.test(PremiumPrice~KnownAllergies,mu=0,conf=0.95,var.eq=F,Paired=F)

t.test(PremiumPrice~HistoryOfCancerInFamily,mu=0,conf=0.95,var.eq=F,Paired=F)
boxplot(PremiumPrice~Diabetes)
boxplot(PremiumPrice~BloodPressureProblems)
boxplot(PremiumPrice~AnyTransplants)
boxplot(PremiumPrice~AnyChronicDiseases)
boxplot(PremiumPrice~KnownAllergies)

install.packages("dplyr")
library(dplyr)
a<-aov(Data$PremiumPrice~factor(Data$NumberOfMajorSurgeries))

summary(a)

multiple.regression<-lm(PremiumPrice~Age+Diabetes+BloodPressureProblems+AnyTransplants+AnyChronicDiseases+Height+Weight+KnownAllergies+HistoryOfCancerInFamily+NumberOfMajorSurgeries,data = train)
summary(multiple.regression) 
sigma(multiple.regression)/mean(train$PremiumPrice)
plot(multiple.regression)


multiple.regression1<-lm(PremiumPrice~Age+AnyTransplants+AnyChronicDiseases+Weight+HistoryOfCancerInFamily+NumberOfMajorSurgeries,data = train)
summary(multiple.regression1) 
sigma(multiple.regression1)/mean(train$PremiumPrice)
plot(multiple.regression1)

predict(multiple.regression1,train)
pred_values=predict(multiple.regression1,data=train)
train$pred_premium=pred_values
View(train)
write_xlsx(train,path="F:\\PROJECT2022\\RRR\\TEST-ds.xlsx")


plot(Data$Height,Data$PremiumPrice,main="Polynomial Regression",las=1) 


ggplot(train,aes(x=Age,y=PremiumPrice))+geom_point(col="light green")+geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+labs(title="Scatter Plot -- PreimumPrice ~ Age")


ggplot(train,aes(x=Diabetes,y=PremiumPrice))+geom_point(col="light pink")+geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+labs(title="Scatter Plot -- PreimumPrice ~ Diabetes")

ggplot(train,aes(x=Diabetes,y=PremiumPrice))+geom_point(col="light pink")+geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+labs(title="Scatter Plot -- PreimumPrice ~ Diabetes")

plot(train)

a<-train$Age
b=train$Diabetes
c=train$BloodPressureProblems
d=train$AnyTransplants
e=train$AnyChronicDiseases
f=train$Height
g=train$Weight
h=train$KnownAllergies
i=train$HistoryOfCancerInFamily
j=train$NumberOfMajorSurgeries


PR2 = lm(Data$PremiumPrice ~ polym(a, b,c, d,e,f,g,h,i,j, degree=3, raw=TRUE),data=Data)


summary(PR2)

 PR1.1=lm(train$PremiumPrice ~ polym(train$Age,degree=2, raw=TRUE),data=train)
summary(PR1.1)
plot(PR1.1)

PR1.2=lm(train$PremiumPrice ~ polym(train$Diabetes,degree=2, raw=TRUE),data=train)
summary(PR1.2)
plot(PR1.2)

PR1.3=lm(train$PremiumPrice ~ polym(train$BloodPressureProblems,degree=2, raw=TRUE),data=train)
summary(PR1.3)
plot(PR1.3)


PR1.4=lm(train$PremiumPrice ~ polym(train$AnyTransplants,degree=2, raw=TRUE),data=train)
summary(PR1.4)
plot(PR1.4)

PR1.5=lm(train$PremiumPrice ~ polym(train$AnyChronicDiseases,degree=2, raw=TRUE),data=train)
summary(PR1.5)
plot(PR1.5)

PR1.6=lm(train$PremiumPrice ~ polym(train$Height,degree=2, raw=TRUE),data=train)
summary(PR1.6)
plot(PR1.6)

PR1.7=lm(train$PremiumPrice ~ polym(train$Weight,degree=2, raw=TRUE),data=train)
summary(PR1.7)
plot(PR1.7)

PR1.8=lm(train$PremiumPrice ~ polym(train$KnownAllergies,degree=2, raw=TRUE),data=train)
summary(PR1.8)
plot(PR1.8)




PR1.9=lm(train$PremiumPrice ~ polym(train$HistoryOfCancerInFamily,degree=2, raw=TRUE),data=train)
summary(PR1.9)
plot(PR1.9)

PR1.10=lm(train$PremiumPrice~polym(train$NumberOfMajorSurgeries,degree=2,raw=TRUE),data=train)
summary(PR1.10)
plot(PR1.10)

PR1 = lm(PremiumPrice ~ polym(Age,Diabetes,BloodPressureProblems,AnyTransplants,AnyChronicDiseases,Height,Weight,KnownAllergies,HistoryOfCancerInFamily,NumberOfMajorSurgeries,degree=2,raw=TRUE),data=train)
summary(PR1)
sigma(PR1)/mean(train$PremiumPrice)
plot(PR1)

predict(PR1,train)
pred_values1=predict(PR1,data=train)
train$pred_premium=pred_values1
View(train)
write_xlsx(train,path="F:\\PROJECT2022\\RRR\\TEST-ds1.xlsx")

PR2 = lm(PremiumPrice ~ polym(Age,Diabetes,BloodPressureProblems,AnyTransplants,AnyChronicDiseases,Weight,NumberOfMajorSurgeries,degree=2,raw=TRUE),data=train)
summary(PR2)
sigma(PR2)/mean(train$PremiumPrice)

plot(PR2)

PR3= lm(PremiumPrice ~ polym(Age,Diabetes,BloodPressureProblems,AnyTransplants,AnyChronicDiseases,Height,Weight,KnownAllergies,HistoryOfCancerInFamily,NumberOfMajorSurgeries,degree=4,raw=TRUE),data=train)
summary(PR3)
sigma(PR3)/mean(train$PremiumPrice)


plot(PR3)

predict(PR3,train)


pred_values2=predict(PR3,data=train)
train$pred_premium=pred_values2
View(train)
write_xlsx(train,path="F:\\PROJECT2022\\RRR\\TEST-ds2.xlsx")





PR4 = lm(PremiumPrice ~ polym(Age,Diabetes,BloodPressureProblems,AnyTransplants,AnyChronicDiseases,Weight,NumberOfMajorSurgeries,degree=4,raw=TRUE),data=train)
summary(PR4)
sigma(PR4)/mean(train$PremiumPrice)

plot(PR4)
predict(PR4,train)

pred_values3=predict(PR4,data=train)
train$pred_premium=pred_values3
View(train)
write_xlsx(train,path="F:\\PROJECT2022\\RRR\\TEST-ds3.xlsx")


plot(train$PremiumPrice,pred_values)

plot(train$PremiumPrice,pred_values1)

plot(train$PremiumPrice,pred_values2)

plot(train$PremiumPrice,pred_values3)


