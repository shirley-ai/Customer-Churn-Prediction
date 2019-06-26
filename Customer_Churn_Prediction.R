#####################################################################################################
#                            Customer Churn Prediction
#                             Shirley Chen, Nov 2018

#####################################################################################################
#content-------------------------------------------------------------------------------------------
# Summary
# Data exploratory analysis
# Data preparation for Linear models
# Linear Models - Logistic Regression 
# Data preparation for Classification models
# Naive Bayes model
# KNN model
# Neural Network model
# Random Forest model
# Predict Customer Churn Prediction for Edinburgh_test data
#---------------------------------------------------------------------------------------------------

# Summary ------------------------------------------------------------------------------------------
# build 5 sets of models to predict CCP, which are Logistic Regression, Naive Bayes, KNN, 
# Neural Network and Random Forest respectively. For each type of model we divided 'Edinburgh_train_sample'
# into training, validation and test set, and tuned models through cross validation. We found 
# Random Forest performed best in prediction so we chose this model to predict 'Edinburgh_test'

# Data exploratory----------------------------------------------------------------------------------
rm(list=ls())

library(plyr)
#install.packages("corrplot")
library(corrplot)

#read data
library(plyr)
#install.packages("readxl")
library(readxl)
data=read_excel("C:/Users/xueli/Documents/R WD/term project/Edinburgh_train_sample.xls")
str(data)
as.factor(data$SeniorCitizen)

numeric_1<-c(19,20,22:29,31,33,34)
charactor_1<-c('gender','SeniorCitizen','Partner','Dependents','PhoneService','MultipleLines','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','PaperlessBilling','VoiceMailPlan','InternationalPlan','Churn','InternetService','Contract','PaymentMethod')
#mean,std,count
summary(data[,numeric_1])
vars1<-which(data$Churn == 'Yes' )
vars2<-which(data$Churn == 'No' )
data_yes<-data[vars1,charactor_1]
data_no<-data[vars2,]
summary(data_yes)
summary(data_no)
#diagram
library('ggplot2')
library('reshape2')
A = c("churn","nonchurn")
B = c(12,35)
C = c(10,42)
dat = data.frame(A,B,C)
names(dat) = c("type","male","female")
dat = melt(dat,variable.name="Sample",value.name = "Num")
p = ggplot(dat, aes(x = type,y = Num,fill = Sample))+
  geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
  scale_fill_manual(values = c("grey","pink"))+  
  geom_text(aes(label = dat$Num),position=position_dodge(width = 0.5),size = 5,vjust = -0.25)+ 
  guides(fill = guide_legend(reverse = F))+  
  theme(plot.title = element_text(size = 25,face = "bold", vjust = 0.5, hjust = 0.5), 
        legend.title = element_blank(),           
        legend.text = element_text(size = 18, face = "bold"),    
        legend.position = 'right',            
        legend.key.size=unit(0.8,'cm')) 
# Data preparatory for Linear models----------------------------------------------------------------
rm(list=ls())

#read data
library(plyr)
install.packages("readxl")
library(readxl)
data_raw=read_excel("C:/Users/xueli/Documents/R WD/term project/Edinburgh_train_sample.xls")
str(data)
as.factor(data$SeniorCitizen)

numeric_1<-c(19,20,22:29,31,33,34)
charactor_1<-c('gender','SeniorCitizen','Partner','Dependents','PhoneService','MultipleLines','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','PaperlessBilling','VoiceMailPlan','InternationalPlan','Churn','InternetService','Contract','PaymentMethod')

#missing value replacement
#method 1,
#with phone service: from "NA" to median ; without phone service: from "NA" to 0 (!!!pay attention to correlation)


#variable value replacing (¡°no phone service¡± to ¡°no¡±, ¡°no internet service¡± to ¡°no¡±)
library(plyr)
data_raw$MultipleLines <- as.factor(mapvalues(data_raw$MultipleLines,from=c("No phone service"), to=c("No")))
data_raw$OnlineSecurity<-as.factor(mapvalues(data_raw$OnlineSecurity, from =c("No internet service"),to=c("No")))
data_raw$OnlineBackup<-as.factor(mapvalues(data_raw$OnlineBackup, from =c("No internet service"),to=c("No")))
data_raw$DeviceProtection<-as.factor(mapvalues(data_raw$DeviceProtection, from =c("No internet service"),to=c("No")))
data_raw$TechSupport<-as.factor(mapvalues(data_raw$TechSupport, from =c("No internet service"),to=c("No")))
data_raw$StreamingTV<-as.factor(mapvalues(data_raw$StreamingTV, from =c("No internet service"),to=c("No")))
data_raw$StreamingMovies<-as.factor(mapvalues(data_raw$StreamingMovies, from =c("No internet service"),to=c("No")))

# Change the values in column ¡°SeniorCitizen¡± from 0 or 1 to ¡°No¡± or ¡°Yes¡±
data_raw$SeniorCitizen <- as.factor(mapvalues(data_raw$SeniorCitizen,
                                              from=c("0","1"),to=c("No", "Yes")))
install.packages('imputeMissings')
library(imputeMissings)
imputedata<- impute(data = data_raw, flag = FALSE)
#correlation test
numeric.var <- sapply(data_raw, is.numeric)
corr.matrix <- cor(data_raw[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

if(!require(dummy, quietly = TRUE)) install.packages('dummy', quiet = TRUE) ; require(dummy, quietly = TRUE)
c1<-c('gender','SeniorCitizen','Partner','Dependents','PhoneService','MultipleLines','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','PaperlessBilling','VoiceMailPlan','InternationalPlan','Churn')
c2<-c('InternetService','Contract')
c3<-c('PaymentMethod')
dummies1 <- dummy::dummy(x = data_raw[,c1],p=1,int = FALSE)
dummies2 <- dummy::dummy(x = data_raw[,c2],p=2,int = FALSE)
dummies3 <- dummy::dummy(x = data_raw[,c3],p=3,int = FALSE)
completed_data<-cbind(data_raw[1],data_raw[c('customerID','tenure','MonthlyCharges','TotalCharges')],data_raw[22:29],data_raw[31],data_raw[33:34],dummies1,dummies2,dummies3)

# Linear Models - Logistic Regression---------------------------------------------------------------

#logistics regression
if (!require('AUC')) { 
  install.packages('AUC',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('AUC') 
}

#create indicators for the train/val/test set
data4<-completed_data[,2:38]
#churn_no change into churn_yes.
c4<-c('Churn')
dummies4 <- dummy::dummy(x = data_raw[,c4],p=2,int = FALSE)
data4[,c('Churn_No')]<-NULL
data4<-cbind(data4,dummies4[2])
ind <-sample(x =1:nrow(data4), size = nrow(data4),replace = FALSE)
#indicaters,replace=each number ocurs once,no duplicate
trainind <- ind[1:round(length(ind)*.70)]
valind <- ind[(round(length(ind)*.70)+1):round(length(ind)*.85)]
testind <- ind[round(length(ind)*.85+1):length(ind)] 

#Test whether there are no intersects
intersect(trainind, valind)
intersect(valind, testind)
intersect(trainind,testind)

#Create the sets and separate the response
train<-data4[trainind,]
y_train <- train$Churn_Yes
train$Churn_Yes <- NULL

test<-data4[testind,]
y_test <- test$Churn_Yes
test$Churn_Yes <- NULL

val<-data4[valind,]
y_val <- val$Churn_Yes
val$Churn_Yes <- NULL

trainBIG <- rbind(train,val)
y_trainBIG <- as.factor(c(as.character(y_train),as.character(y_val)))

save(train,val,test,trainBIG,y_train, y_test, y_val, y_trainBIG, file = 'trainvaltestmedian.Rdata')
load('trainvaltestmedian.Rdata')
save(trainind,valind,testind,ind,file='ind.Rdata')
load('ind.Rdata')
LR <- glm(formula = y_trainBIG~., data = trainBIG, family = binomial("logit"))
LR
coefficients(LR)
#The odds ratio
exp(coefficients(LR))

#make a prediction
predLR <- as.numeric(predict(LR,test, type='response'))
(auc_lr <- AUC::auc(roc(predLR, y_test)))

AIC(LR)
BIC(LR)


#Build a stepwise losgitic regression model
LRstep <- step(object = LR, direction = 'both')
coefficients(LRstep)
predLRstep <- as.numeric(predict(LRstep, test,type = 'response'))
if(!require(AUC, quietly = TRUE)) install.packages('AUC', quiet = TRUE) ; require(AUC, quietly = TRUE)
(auc_step <- AUC::auc(roc(predLRstep, y_test)))
AIC(LRstep)
BIC(LRstep)
#lasso
if (!require('glmnet')) {   
  install.packages('glmnet',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('glmnet') 
}

(LRl1 <- glmnet(x=data.matrix(train),y=y_train,
                family="binomial"))

#Plot the lambda paramater
plot(LRl1, xvar= 'lambda')

#Look at the coefficients of the LR model for high and low values of lambda
coef(LRl1)[,1:2]
coef(LRl1)[,60:61]

aucs <- numeric()
for (i in 1:length(LRl1$lambda)) {
  print(i)
  predLRl1 <- predict(LRl1,newx=data.matrix(val),
                      type="response",
                      s=LRl1$lambda[i])
  aucs[i] <- AUC::auc(roc(as.numeric(predLRl1),y_val))#0,8181#0.8889
}

#Let's determine the optimal lambda value
plot(1:length(LRl1$lambda),aucs,type="l")
(LR.lambda <- LRl1$lambda[which.max(aucs)])

#With this final lambda we re-do the analysis on the big training set
LRl1 <- glmnet(x=data.matrix(trainBIG),y=y_trainBIG,
               family="binomial")
#We then use that model with the optimal lambda.
predLRl1 <- as.numeric(predict(LRl1,newx=data.matrix(test),
                               type="response",
                               s=LR.lambda))
#Finally we assess the performance of the model
(auc_l1 <- AUC::auc(roc(predLRl1,y_test)))

# Data preparation for Classification models--------------------------------------------------------
# load data
rm(list=ls())
classes <- c("character","factor","factor","factor","factor","numeric","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","numeric","factor","numeric","numeric")
data <- read.csv("C:/Users/xueli/Documents/R WD/term project/Edinburgh_train_sample.csv",colClasses=classes)
str(data)
colnames(data)[1] <- 'customerID'


#deal with missing value
install.packages('imputeMissings')
library(imputeMissings)
data<- impute(data = data, flag = FALSE)

str(data)
data$customerID <- NULL #remove customer id, not useful for modelling
# also create a numeric data
data_num<-as.data.frame(sapply(data,as.numeric)) 
str(data_num)
# Naive Bayes model---------------------------------------------------------------------------------
#divide data into training, validation and test set (6:2:2)
allind <- sample(x=1:nrow(data),size=nrow(data))
trainind <- allind[1:60]
valind <- allind[61:80]
testind <- allind[81:99]
data_tr <- data[trainind,] #our validation set
data_v <- data[valind,] #our validation set
data_test <- data[testind,] #our test set
# also make a train big
data_tr_big<-rbind(data_tr,data_v)
data_tr_big$Churn <- NULL
#isolate our response to make it easier to call functions later
y_tr <- data_tr$Churn
data_tr$Churn <- NULL
y_v <- data_v$Churn
data_v$Churn <- NULL
y_test <- data_test$Churn
data_test$Churn <- NULL

y_tr_big<-as.factor(c(y_tr,y_v))
#Check the distribution of the dependent variable.
#It should be similar in the three sets. if not, repeat above steps
table(y_tr)
table(y_v)
table(y_test)

#check whether we made mistakes
dim(data_tr)
dim(data_v)
dim(data_test)
# clean the environment
rm(list=setdiff(ls(),c("data","data_tr","data_v","data_tr_big","y_tr","y_v","y_tr_big","data_test","y_test")))

if (!require("e1071")) {
  install.packages('e1071',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('e1071')
}
if (!require("AUC")) {
  install.packages('AUC',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('AUC')
}

# first put all data into the model - auc=0.79
NB <- naiveBayes(x=data_tr, y=y_tr)
predNB <- predict(NB,data_v,type="raw", threshold = 0.001)[,2]
AUC::auc(roc(predNB,y_v))
# tenure and contract - auc 0.92
NB <- naiveBayes(x=data_tr[,c(5,15)], y=y_tr)
predNB <- predict(NB,data_v[,c(5,15)],type="raw", threshold = 0.001)[,2]
AUC::auc(roc(predNB,y_v))
# tenure, contract, customer service,total day call, number of message - auc 0.91
NB <- naiveBayes(x=data_tr[,c(27,5,15,21,26)], y=y_tr)
predNB <- predict(NB,data_v[,c(27,5,15,21,26)],type="raw", threshold = 0.001)[,2]
AUC::auc(roc(predNB,y_v))
# tenure, contract, customer service,total day call, number of message,phone service, internet service - auc 0.93
NB <- naiveBayes(x=data_tr[,c(27,5,15,21,26,6,8)], y=y_tr)
predNB <- predict(NB,data_v[,c(27,5,15,21,26,6,8)],type="raw", threshold = 0.001)[,2]
AUC::auc(roc(predNB,y_v))

#---winner----#
#tenure, contract, customer service,total day call,number of message,phone service, internet service
NB <- naiveBayes(x=data_tr[,c(27,5,15,21,26,6,8)], y=y_tr)
predNB <- predict(NB,data_v[,c(27,5,15,21,26,6,8)],type="raw", threshold = 0.001)[,2]
AUC::auc(roc(predNB,y_v)) #0.96

#----test-----#

auc <- numeric()
for (j in 1:10) {
  allind <- sample(x=1:nrow(data),size=nrow(data))
  trainind <- allind[1:60]
  valind <- allind[61:80]
  testind <- allind[81:99]
  data_tr <- data[trainind,] #our validation set
  data_v <- data[valind,] #our validation set
  data_test <- data[testind,] #our test set
  # also make a train big
  data_tr_big<-rbind(data_tr,data_v)
  data_tr_big$Churn <- NULL
  #isolate our response to make it easier to call functions later
  y_tr <- data_tr$Churn
  data_tr$Churn <- NULL
  y_v <- data_v$Churn
  data_v$Churn <- NULL
  y_test <- data_test$Churn
  data_test$Churn <- NULL
  
  y_tr_big<-as.factor(c(y_tr,y_v))
  NB <- naiveBayes(x=data_tr_big[,c(27,5,15,21,26,6,8)], y=y_tr_big)
  predNB <- predict(NB,data_test[,c(27,5,15,21,26,6,8)],type="raw", threshold = 0.001)[,2]
  auc[j]<-AUC::auc(roc(predNB,y_test))
 
  }
(mean(auc)) #0.845

plot(roc(predNB,y_test))
# KNN model-----------------------------------------------------------------------------------------
#KNN requires numerical variables so we need to use data_num
#divide data into training, validation and test set (6:2:2)
allind <- sample(x=1:nrow(data_num),size=nrow(data_num))
trainind <- allind[1:60]
valind <- allind[61:80]
testind <- allind[81:99]
data_tr <- data_num[trainind,] #our validation set
data_v <- data_num[valind,] #our validation set
data_test <- data_num[testind,] #our test set
# also make a train big
data_tr_big<-rbind(data_tr,data_v)
data_tr_big$Churn <- NULL
#isolate our response to make it easier to call functions later
y_tr <- as.factor(data_tr$Churn)
data_tr$Churn <- NULL
y_v <- as.factor(data_v$Churn)
data_v$Churn <- NULL
y_test <- as.factor(data_test$Churn)
data_test$Churn <- NULL

y_tr_big<-as.factor(c(y_tr,y_v))
#Check the distribution of the dependent variable.
#It should be similar in the three sets. if not, repeat above steps
table(y_tr)
table(y_v)
table(y_test)

#check whether we made mistakes
dim(data_tr)
dim(data_v)
dim(data_test)
# clean the environment
rm(list=setdiff(ls(),c("data","data_tr","data_v","data_tr_big","y_tr","y_v","y_tr_big","data_test","y_test")))

if (!require("FNN")) {
  install.packages('FNN',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('FNN')
}

#5=tenure
#6=phone service
#8=internet service
#15=contract
#18=monthy charges
#21=day calls
#26=number of voice mail messages
#27=customer service calls

# build sets for KNN
# after trying different variables below is the best combination
trainKNN<-data_tr[,c(5,8,15,18)]
valKNN<-data_v[,c(5,8,15,18)]
y_tr_KNN<-y_tr
y_v_KNN<-y_v

# standardize
stdev <- sapply(trainKNN,sd)
means <- sapply(trainKNN,mean)
trainKNN <- data.frame(t((t(trainKNN)-means)/stdev))
valKNN <- data.frame(t((t(valKNN)-means)/stdev))


# choose the best K
auc <- numeric()
for (k in 1:nrow(trainKNN)) {
  indicatorsKNN <- as.integer(knnx.index(data=trainKNN,
                                         query=valKNN,
                                         k=k))
  #retrieve the actual y from the training set
  predKNN <- as.integer(as.character(y_tr_KNN[indicatorsKNN]))
  #if k > 1 then we take the proportion of 1s
  predKNN <- rowMeans(data.frame(matrix(data=predKNN,
                                        ncol=k,
                                        nrow=nrow(valKNN))))
  auc[k] <- AUC::auc(roc(predKNN,y_v_KNN))
}
(k <- which.max(auc))

indicatorsKNN <- as.integer(knnx.index(data=trainKNN,
                                       query=valKNN,
                                       k=k))
#retrieve the actual y from the training set
predKNN_optimal <- as.integer(as.character(y_tr_KNN[indicatorsKNN]))
#if k > 1 then we take the proportion of 1s
predKNN_optimal <- rowMeans(data.frame(matrix(data=predKNN_optimal,
                                              ncol=k,
                                              nrow=nrow(valKNN))))
AUC::auc(roc(predKNN_optimal,y_v_KNN))
#----winner-------#
# k=8
# auc=0.94
# input=c(5,8,15,18)

#-----test------# 

auc <- numeric()
for (j in 1:10) {
  allind <- sample(x=1:nrow(data_num),size=nrow(data_num))
  trainind <- allind[1:60]
  valind <- allind[61:80]
  testind <- allind[81:99]
  data_tr <- data_num[trainind,] #our validation set
  data_v <- data_num[valind,] #our validation set
  data_test <- data_num[testind,] #our test set
  # also make a train big
  data_tr_big<-rbind(data_tr,data_v)
  data_tr_big$Churn <- NULL
  #isolate our response to make it easier to call functions later
  y_tr <- as.factor(data_tr$Churn)
  data_tr$Churn <- NULL
  y_v <- as.factor(data_v$Churn)
  data_v$Churn <- NULL
  y_test <- as.factor(data_test$Churn)
  data_test$Churn <- NULL
  
  y_tr_big<-as.factor(c(y_tr,y_v))
  trainbigKNN<-data_tr_big[,c(5,8,15,18)]
  testKNN<-data_test[,c(5,8,15,18)]
  y_trbig_KNN<-y_tr_big
  y_test_KNN<-y_test
  
  # standardize
  stdev <- sapply(trainKNN,sd)
  means <- sapply(trainKNN,mean)
  trainbigKNN <- data.frame(t((t(trainbigKNN)-means)/stdev))
  testKNN <- data.frame(t((t(testKNN)-means)/stdev))
  # initialize K
  k <- 8
  #retrieve the indicators of the k nearest neighbors of the query data
  indicatorsKNN <- as.integer(knnx.index(data=trainbigKNN,
                                         query=testKNN,
                                         k=k))
  #retrieve the actual y from the training set
  predKNN_optimal <- as.integer(as.character(y_tr_big[indicatorsKNN]))
  #if k > 1 then we take the proportion of 1s
  predKNN_optimal <- rowMeans(data.frame(matrix(data=predKNN_optimal,
                                                ncol=k,
                                                nrow=nrow(testKNN))))
  
  auc[j]<-AUC::auc(roc(predKNN_optimal,y_test_KNN))
  
}
(mean(auc)) #0.77

# Neural Network model------------------------------------------------------------------------------
#divide data into training, validation and test set (6:2:2)
allind <- sample(x=1:nrow(data_num),size=nrow(data_num))
trainind <- allind[1:60]
valind <- allind[61:80]
testind <- allind[81:99]
data_tr <- data_num[trainind,] #our validation set
data_v <- data_num[valind,] #our validation set
data_test <- data_num[testind,] #our test set
# also make a train big
data_tr_big<-rbind(data_tr,data_v)
data_tr_big$Churn <- NULL
#isolate our response to make it easier to call functions later
y_tr <- as.factor(data_tr$Churn)
data_tr$Churn <- NULL
y_v <- as.factor(data_v$Churn)
data_v$Churn <- NULL
y_test <- as.factor(data_test$Churn)
data_test$Churn <- NULL

y_tr_big<-as.factor(c(y_tr,y_v))
#Check the distribution of the dependent variable.
#It should be similar in the three sets. if not, repeat above steps
table(y_tr)
table(y_v)
table(y_test)

#check whether we made mistakes
dim(data_tr)
dim(data_v)
dim(data_test)
# clean the environment
rm(list=setdiff(ls(),c("data","data_tr","data_v","data_tr_big","y_tr","y_v","y_tr_big","data_test","y_test")))

if (!require("nnet")) {
  install.packages('nnet',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('nnet')
}
#-# Loading required package: nnet
#Run an external function to facilitate tuning
source("http://ballings.co/hidden/aCRM/code/chapter2/tuneMember.R")


#first we need to scale the data to range [0,1] avoid numerical problems
tr_num_ID <- sapply(data_tr, is.numeric)
tr_nn <- data_tr[, tr_num_ID]
minima <- sapply(tr_nn,min)
scaling <- sapply(tr_nn,max)-minima

#center is subtracted from each column. Because we use the minima this sets the minimum to zero.
#scale: each column is divided by scale. Because we use the range this sets the maximum to one.
tr_nn_scaled <- data.frame(base::scale(tr_nn,
                                       center=minima,
                                       scale=scaling))


#check
sapply(tr_nn_scaled,range)

NN.rang <- 0.5 #the range of the initial random weights parameter
NN.maxit <- 10000 #set high in order not to run into early stopping
NN.size <- seq(1,20) #number of units in the hidden layer
NN.decay <- seq(0,0.5,by=0.01) #weight decay.
#Same as lambda in regularized LR. Controls overfitting
call <- call("nnet",
             formula = y_tr ~ .,
             data=tr_nn_scaled,
             rang=NN.rang, maxit=NN.maxit,
             trace=FALSE, MaxNWts= Inf)
tuning <- list(size=NN.size, decay=NN.decay)
#tune nnet
#scale validation data
v_num_ID <- sapply(data_v, is.numeric)
v_nn <- data_v[, v_num_ID]
v_nn_scaled <- data.frame(base::scale(v_nn,
                                      center=minima,
                                      scale=scaling))



(result <- tuneMember(call=call,
                      tuning=tuning,
                      xtest=v_nn_scaled,
                      ytest=y_v,
                    predicttype="raw"))
# size=3
# decay=0.02
# auc=0.86

#Create final model
trbig_nn<-rbind(tr_nn,v_nn)
trbig_nn_scaled <- data.frame(base::scale(trbig_nn,
                                          center=minima,
                                          scale=scaling))

NN <- nnet(y_tr_big ~ .,
           trbig_nn_scaled,
           size = result$size,
           rang = NN.rang,
           decay = result$decay,
           maxit = NN.maxit,
           trace=TRUE,
           MaxNWts= Inf)

#predict on test
test_num_ID <- sapply(data_test, is.numeric)
test_nn <- data_test[, test_num_ID]
test_nn_scaled <- data.frame(base::scale(test_nn,
                                         center=minima,
                                         scale=scaling))

predNN <- as.numeric(predict(NN,test_nn_scaled,type="raw"))
auc(roc(predNN,y_test))
# auc=0.85
plot(roc(predNN,y_test))

# Random Forest model-------------------------------------------------------------------------------
#divide data into training, validation and test set (6:2:2)
allind <- sample(x=1:nrow(data_num),size=nrow(data_num))
trainind <- allind[1:60]
valind <- allind[61:80]
testind <- allind[81:99]
data_tr <- data_num[trainind,] #our validation set
data_v <- data_num[valind,] #our validation set
data_test <- data_num[testind,] #our test set
# also make a train big
data_tr_big<-rbind(data_tr,data_v)
data_tr_big$Churn <- NULL
#isolate our response to make it easier to call functions later
y_tr <- as.factor(data_tr$Churn)
data_tr$Churn <- NULL
y_v <- as.factor(data_v$Churn)
data_v$Churn <- NULL
y_test <- as.factor(data_test$Churn)
data_test$Churn <- NULL

y_tr_big<-as.factor(c(y_tr,y_v))
#Check the distribution of the dependent variable.
#It should be similar in the three sets. if not, repeat above steps
table(y_tr)
table(y_v)
table(y_test)

#check whether we made mistakes
dim(data_tr)
dim(data_v)
dim(data_test)
# clean the environment
rm(list=setdiff(ls(),c("data","data_tr","data_v","data_tr_big","y_tr","y_v","y_tr_big","data_test","y_test")))

#load the package randomForest
if (!require("randomForest")) {
  install.packages('randomForest',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('randomForest')
}



#create a first random forest model
rFmodel <- randomForest(x=data_tr,
                        y=y_tr,
                        ntree=1000,
                        importance=TRUE)
#look at the importance of the variables
importance(rFmodel, type=1)
predrF <- predict(rFmodel,data_v,type="prob")[,2]
#assess final performance
AUC::auc(roc(predrF,y_v)) #0.97

#create the second random forest model
rFmodel <- randomForest(x=data_tr,
                        y=y_tr,
                        ntree=500,
                        importance=TRUE)
#look at the importance of the variables
importance(rFmodel, type=1)
predrF <- predict(rFmodel,data_v,type="prob")[,2]
#assess final performance
AUC::auc(roc(predrF,y_v)) #0.97

#500 and 1000 trees have the same prediction power so we choose 500
rFmodel <- randomForest(x=data_tr_big,
                        y=y_tr_big,
                        ntree=500,
                        importance=TRUE)

predrF <- predict(rFmodel,data_test,type="prob")[,2]

#AUC, accuracy, sensitivity, specificity
AUC::auc(roc(predrF,y_test)) # 0.85
plot(roc(predrF,y_test))
a<-accuracy(predrF,y_test,perc.rank = TRUE) #84%
plot(a)
b<-sensitivity(predrF,y_test,perc.rank = TRUE) #75%
plot(b)
c<-specificity(predrF,y_test,perc.rank = TRUE) #87%
plot(c)

# test performance
auc <- numeric()
for (j in 1:10) {
  allind <- sample(x=1:nrow(data_num),size=nrow(data_num))
  trainind <- allind[1:60]
  valind <- allind[61:80]
  testind <- allind[81:99]
  data_tr <- data_num[trainind,] #our validation set
  data_v <- data_num[valind,] #our validation set
  data_test <- data_num[testind,] #our test set
  # also make a train big
  data_tr_big<-rbind(data_tr,data_v)
  data_tr_big$Churn <- NULL
  #isolate our response to make it easier to call functions later
  y_tr <- as.factor(data_tr$Churn)
  data_tr$Churn <- NULL
  y_v <- as.factor(data_v$Churn)
  data_v$Churn <- NULL
  y_test <- as.factor(data_test$Churn)
  data_test$Churn <- NULL
  
  y_tr_big<-as.factor(c(y_tr,y_v))
   
  rFmodel <- randomForest(x=data_tr_big,
                          y=y_tr_big,
                          ntree=500,
                          importance=TRUE)
  
  predrF <- predict(rFmodel,data_test,type="prob")[,2]
  #Final performance
  auc[j]<-AUC::auc(roc(predrF,y_test))

}
(mean(auc)) #0.88
plot(roc(predrF,y_test))

# sensitivity analysis
allind <- sample(x=1:nrow(data_num),size=nrow(data_num))
trainind <- allind[1:60]
valind <- allind[61:80]
testind <- allind[81:99]
data_tr <- data_num[trainind,] #our validation set
data_v <- data_num[valind,] #our validation set
data_test <- data_num[testind,] #our test set
# also make a train big
data_tr_big<-rbind(data_tr,data_v)
data_tr_big$Churn <- NULL
#isolate our response to make it easier to call functions later
y_tr <- as.factor(data_tr$Churn)
data_tr$Churn <- NULL
y_v <- as.factor(data_v$Churn)
data_v$Churn <- NULL
y_test <- as.factor(data_test$Churn)
data_test$Churn <- NULL
y_tr_big<-as.factor(c(y_tr,y_v))

train<-data_tr_big
test<-data_test

# 0.872 for the first model

#data_tr_big$tenure<-NULL
#data_test$tenure<-NULL
# 0.850

#data_tr_big$Contract<-NULL
#data_test$Contract<-NULL
# 0.852

#data_tr_big$MonthlyCharges<-NULL
#data_test$MonthlyCharges<-NULL
#0.866

#data_tr_big$InternationalPlan<-NULL
#data_test$InternationalPlan<-NULL
#0.849

#data_tr_big$VoiceMailPlan<-NULL
#data_test$VoiceMailPlan<-NULL
# 0.868

#data_tr_big$TotalDayCalls<-NULL
#data_test$TotalDayCalls<-NULL
# 0.855

#data_tr_big$NumbervMailMessages<-NULL
#data_test$NumbervMailMessages<-NULL
# 0.877

#data_tr_big$CustomerServiceCalls<-NULL
#data_test$CustomerServiceCalls<-NULL
# 0.793

#delete all the strong predictors
# 0.748

for (j in 1:10) {
  rFmodel <- randomForest(x=data_tr_big,
                          y=y_tr_big,
                          ntree=500,
                          importance=TRUE)
  
  predrF <- predict(rFmodel,data_test,type="prob")[,2]
  #Final performance
  auc[j]<-AUC::auc(roc(predrF,y_test))
}
mean(auc)

data_tr_big<-train
data_test<-test


# Predict Customer Churn Prediction for Edinburgh_test data-----------------------------------------

classes <- c("character","factor","factor","factor","factor","numeric","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","numeric","factor","numeric","numeric")
testdata <- read.csv("C:/Users/xueli/Documents/R WD/term project/Edinburgh_test.csv",colClasses=classes)
str(testdata)
colnames(testdata)[1] <- 'customerID'

#deal with missing value
install.packages('imputeMissings')
library(imputeMissings)
testdata<- impute(data = testdata, flag = FALSE)#for phone service users impute with median

str(testdata)
customerID<-testdata$customerID
testdata$customerID <- NULL 
testdata_num<-as.data.frame(sapply(testdata,as.numeric)) 

#load the package randomForest
if (!require("randomForest")) {
  install.packages('randomForest',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('randomForest')
}


# make prediction
rFmodel <- randomForest(x=data_tr_big,
                        y=y_tr_big,
                        ntree=500,
                        importance=TRUE)
predrF <- predict(rFmodel,testdata_num,type="prob")[,2]
importance(rFmodel, type=1)
final<-data.frame(customerID,predrF)
varImpPlot(rFmodel,type=1)
# export result
write.csv(final, file="C:/Users/xueli/Documents/R WD/term project/final.csv")

