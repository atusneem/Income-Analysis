# Final Project Script
# Ayra Tusneem
# April 5th, 2019

# Working Directories for all group members
#Change directory for DataSet
setwd("/Users/ayratusneem/Downloads") # Ayra

# Installing Packages
install.packages('ISLR')
install.packages("useful")
install.packages('glmnet')
install.packages('coefplot')
install.packages('plotmo')
install.packages('leaps')
install.packages('doBy',repos = "http://cran.us.r-project.org")
install.packages('Hmisc')
install.packages('corrplot',repos = 'http://cran.us.r-project.org')
install.packages('corrgram', repos = 'http://cran.us.r-project.org')
install.packages('randomForest')

#Libraries
library(doBy)
library(plotmo)
library(coefplot)
library(ISLR)
library(useful)
library(glmnet)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(corrgram)
library(randomForest)

# Importing the Data
Data <- read.csv("adult.data")

# Transforming the Dataset & Data Cleaning
colnames(Data) <- c('Age', 'Work Class', 'FNLWGT', 'Education', 'Education/Num',
                    'Marital Status', 'Occupation', 'Relationship', 'Race', 'Sex',
                    'Capital Gain', 'Capital Loss', 'Hours Per Week', 'Native Country','Income')
Data[Data == "?"] <- NA
colSums(is.na(Data))
Data <- na.omit(Data)
sum(is.na(Data))
Data[!rev(duplicated(rev(Data$.))),]


# Dummy Variable for Income
Data$Income1 <- as.numeric(Data$Income == " >50K")

# Creating New Variable for US Citizen
Data$USBorn <- Data$`Native Country`==" United-States"
  
# Transforming Variables
Data$`Work Class` <- as.factor(Data$`Work Class`)
Data$Occupation <- as.factor(Data$Occupation)
Data$`Native Country` <- as.factor(Data$`Native Country`)

# Histograms & Log Transformed Variables
HistAge <- hist(Data$Age)
Data$lnAge <- log(Data$Age)
hist(Data$lnAge)

hist(Data$`Hours Per Week`)
Data$lnHoursPerWeek <- log(Data$`Hours Per Week`)
hist(Data$lnHoursPerWeek)

# Summaries
summary(Data)
str(Data)
head(Data)
by(Data, Data$Income1, summary)
Hmisc::describe(Data)


#Removing FNLWGT, age, and hours per week from dataset
Data <- Data[,-3]
Data <- Data[,-1]
Data <- Data[,-11]


# Correlations & Corrplot
Data_numeric <- as.data.frame(lapply(Data,as.numeric))
Data_factor <- as.data.frame(lapply(Data, as.factor))
corrplot::corrplot(cor(Data_numeric))
correlations <- cor(Data_numeric, use="complete.obs", method="pearson")
correlations
corrplot(correlations,method='color')
corrgram(Data_numeric, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

# Tables
table(Data$`Work Class`, Data$Income)
table(Data$Sex, Data$Income)
table(Data$Race, Data$Income)

# Plots
ggplot(Data,aes(x=lnAge,y=Education)) + geom_point(aes(color=Income1)) + facet_wrap(~Race)
ggplot(Data,aes(x=lnAge, y=`Marital Status`)) + geom_point(aes(color=Income1))
ggplot(Data,aes(x=lnHoursPerWeek,y=`Work Class`)) + geom_point(aes(color=Income1))+facet_wrap(~Education)
ggplot(Data,aes(x=lnHoursPerWeek, y=Occupation)) + geom_point(aes(color=Income1))

# SummaryBy for Education
doBy::summaryBy(Income1 ~ `Education/Num`, data = Data, FUN = c(mean,sd))
summaryBy(Income1 ~ Education, data = Data, FUN = c(mean,sd))



# Test and Train Data
set.seed(1861)
TrainIndex <- sample(1:nrow (Data_numeric), size = floor(0.75 * nrow(Data_numeric)))
DataTrain <- Data_numeric[TrainIndex,]
DataTest <- Data_numeric[-TrainIndex,]

# Linear Regression on Age, Education, and Race
lmFit2 <- lm(Income1 ~ lnAge + Education + Race, data = DataTrain)
summary(lmFit2)
plot(lmFit2, which = c(1,3))

lmFit2Test <- predict(lmFit2 , newdata = DataTest, type="response")
summary(lmFit2Test)

residuals <- as.data.frame(residuals(lmFit2))
head(residuals)
summary(residuals)
#figure out stat bin width
ggplot(residuals, aes(x = residuals(lmFit2))) +  geom_histogram(fill='blue') 

# Predictions of above Regression
Income1.hat <- predict(lmFit2,DataTest)
predictions <- cbind(Income1.hat,DataTest$Income1) 
colnames(predictions) <- c('pred','real')
predictions <- as.data.frame(predictions)
predictions

#Linear Regression Log of Hours Per Week
lmFit3 <- lm(Income1 ~ lnHoursPerWeek, data = DataTrain)
summary(lmFit3)
plot(lmFit3, which = c(1,3))

###Logistic

# Logistic Regression on all Predictors
logit_Income <- glm(Income1~., data = DataTrain)
summary(logit_Income)
plot(logit_Income, which = c(1,3))

# Logistic on Age, Education, and Race
logisticFit2 <- glm(Income1 ~ lnAge + `Education.Num` + Race + Occupation, family="binomial",data=DataTrain)
summary(logisticFit2)
plot(logisticFit2, which = c(1,3))


MSE<-function(pred,true){
  mean((pred-true)^2)
}
MSELogit.Fit2 <- MSE(logisticFit2$fitted.values, DataTest$Income1)
MSELogit.Fit2

IncomePred <-predict(logisticFit2,type = 'response')

IncomeTable <- table(IncomePred > .35,DataTrain$Income1) #confusion matrix
IncomeTable

#Logistic Regression on Age, Education/Num, Hours Per Week, Race, Sex
logisticFit3 <- glm(Income1 ~ lnAge + `Education.Num` + Race + Sex + lnHoursPerWeek, family = "binomial", data = DataTrain)
summary(logisticFit3)
plot(logisticFit3, which = c(1,3))

MSE<-function(pred,true){
  mean((pred-true)^2)
}
MSELogit.Fit3 <- MSE(logisticFit3$fitted.values, DataTest$Income1)
MSELogit.Fit3

IncomePred3 <-predict(logisticFit3,type = 'response')

IncomeTable3 <- table(IncomePred > .35,DataTrain$Income1) #confusion matrix
IncomeTable3

# Logistic Regression only on Education/Num as Predictor
logisticFit4 <- glm(Income1 ~ `Education.Num`, family="binomial", data = DataTrain)
summary(logisticFit4)
plot(logisticFit4, which = c(1,3))
exp(logisticFit4$coefficients)

ggplot(Data,aes(x=lnAge,y=Education))+geom_point(aes(color=Income1))+facet_wrap(~Race)
ggplot(Data,aes(x=lnAge,y =`Work Class`)) + geom_point(aes(color=Income1)) + facet_wrap(~Education)
ggplot(Data,aes(x=lnAge,y=`Marital Status`)) + geom_point(aes(color=Income1))
ggplot(Data,aes(x= lnHoursPerWeek,y=`Work Class`))+geom_point(aes(color=Income1))+facet_wrap(~Education)
ggplot(Data,aes(x=lnHoursPerWeek,y=Occupation))+geom_point(aes(color=Income1))

require("useful")
require("glmnet")
require('coefplot')
require('plotmo')


# IncomeVar, formula for predicting income over 50K
IncomeVar <- as.formula(Income1 ~ lnAge + `Work.Class` + Education + `Education.Num` + `Marital.Status` + Occupation + Relationship + Race + Sex + `Capital.Gain` + `Capital.Loss` + lnHoursPerWeek)

# X & Y variables for ridge and lasso
Xvar<-build.x(formula = IncomeVar, data = DataTrain)
Yvar<-build.y(formula = IncomeVar, data = DataTrain)

# Ridge
RidgeFit.Income1 <- cv.glmnet(x= Xvar,y= Yvar,alpha = 0)
coef(RidgeFit.Income1, s="lambda.min")
plot(RidgeFit.Income1)

RidgeFit.Income1$lambda.min

# Lasso
LassoFit.Income1 <- cv.glmnet(x = Xvar, y = Yvar, alpha = 1)
coef(LassoFit.Income1, s="lambda.min")
plot(LassoFit.Income1)

OLSFit <- lm(IncomeVar, data = DataTrain)
summary(OLSFit)

# Test Data & RMSE
RMSE=function(x1,x2){
  sqrt( mean((x1-x2)^2))}

X.test <- build.x(IncomeVar, DataTest)
Y.test <- build.y(IncomeVar, DataTest)
GR.Ridge.yhat <- predict(RidgeFit.Income1, newx = X.test, s="lambda.min")
GR.Lasso.yhat <- predict(LassoFit.Income1, newx = X.test, s="lambda.min")

RMSE(GR.Ridge.yhat,Y.test)
RMSE(GR.Lasso.yhat,Y.test)

#Random Forest
require('randomForest')
set.seed(101)
dim(Data)
train = sample(1:nrow(Data),300)
rf.Income = randomForest::randomForest(Income1~Education.Num + Race + Sex, data = Data_factor, subset = train, family = "binomial")
rf.Income
varImpPlot(rf.Income)

#OLS

OLSFit<-lm(IncomeVar,data = DataTrain)
OLSFit
summary(OLSFit)
#Age-.0215
WC<-.5215+.1228+.2126+.1833
#Work Class-1.0402
Ed<-.1492+.0112+.1380+.2791
#Education-.5775
incvar2<-as.formula(Income~lnAge+`Education.Num`+Occupation+Relationship+Sex+lnHoursPerWeek+`Capital.Gain`+`Capital.Loss`)
OLSFit2<-lm(incvar2 ,data=DataTrain)
summary(OLSFit2)
incompred<-predict(OLSFit2,type='response')
table1<-table(incompred>.5,DataTrain$Income)
table1
#Accuracy for OLS
A1<-(657+10341)/(11665+1924+679+2012)
A1
incompred2<-predict(OLSFit2,type='response')
table2<-table(incompred2>.5,DataTrain$Income)
table2
#Accuracy for Lasso
A2<-(682+10274)/(10179+3205+2196+700)
A2
#Sensitivity for Lasso
Sn2<-1065/(1065+2840)
Sn2
#Specificity for Lasso
Sp2<-9058/(9058+3317)
Sp2





