##### Project Statistical Learning #####
#### Andrea Corvi - Andrea Huscher ####

### Loading packages neeeded
library(readr)
library(mice)
library(plyr)
library(ggplot2)
library(e1071)
library(dplyr)
library(magrittr)
library(mltools)
library(caret)
library(pROC)
library(arm)
library(lsr)
library(corrplot)
library(corrgram)
library(rpart)
library(xgboost)
### Dataset reading and storing
bank <- read_delim(".../bank-full.csv",";", escape_double = FALSE, trim_ws = TRUE)
str(bank)
summary(bank)

## Check and set variables type
bank$job= as.factor(bank$job)
bank$marital= as.factor(bank$marital)
bank$education= as.factor(bank$education)
bank$default= as.factor(bank$default)
bank$housing= as.factor(bank$housing)
bank$loan= as.factor(bank$loan)
bank$contact= as.factor(bank$contact)
bank$month= as.factor(bank$month)
bank$poutcome= as.factor(bank$poutcome)
bank$y= as.factor(bank$y)

## Check NAs
md.pattern(bank, rotate.names = T) # No NAs, all good

### EDA

plyr::count(bank$y)
# no 39922
# yes  5289. Yes is only 11% of the total observations.
# For the sake of this project we want to "play" on the treshold in order
# not to perform resampling techniques which in our opinion will add bias

names(bank)
# names(bank)
# [1] "age" n      "job" c      "marital"c   "education"c "default"c   "balance"n  
# [7] "housing" c  "loan" c     "contact" c  "day"n       "month"n     "duration" n
# [13] "campaign"n  "pdays" n    "previous"c  "poutcome"c  "y"c

##Correlation matrix

#Function to create correlation matrix for all the types of variables we have
cor2 = function(df){
  
  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  
  cor_fun <- function(pos_1, pos_2){
    
    # both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){
      r <- stats::cor(df[[pos_1]]
                      , df[[pos_2]]
                      , use = "pairwise.complete.obs"
      )
    }
    
    # one is numeric and other is a factor/character
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
    }
    
    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
    }
    
    # both are factor/character
    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
    }
    
    return(r)
  } 
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  corrmat <- outer(1:ncol(df)
                   , 1:ncol(df)
                   , function(x, y) cor_fun(x, y)
  )
  
  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)
  
  return(corrmat)
}
cormatri = cor2(bank.full)
corrplot(cormatri)


##Plots
mypalette= c("#102232","#81BADF")

#Age
ggplot2::ggplot(bank, aes(x=age))+
  geom_histogram(aes(fill = bank$y), bins=40)+
  scale_fill_manual(aesthetics = "fill",values = mypalette) +  labs(title="Age", y="Count") +
  theme(legend.position="right",
        axis.title.x=element_blank(),
        legend.title=element_blank())

vezzi = ggplot2::ggplot(bank.full,aes(x=age))+
  geom_bar(aes(fill = bank.full$y)) +  xlim(60,100)+
  scale_fill_manual(aesthetics = "fill",values = mypalette)+
  labs(title = "Old clients", y="Count") + theme(legend.position="right",
                                                 axis.title.x=element_blank(),
                                                 legend.title=element_blank())
vezzi
giovani = ggplot2::ggplot(bank.full,aes(x=age))+
  geom_bar(aes(fill = bank.full$y)) +  xlim(18,23)+
  scale_fill_manual(aesthetics = "fill",values = mypalette)+
  labs(title = "Young clients", y="Count") + theme(legend.position="right",
                                                   axis.title.x=element_blank(),
                                                   legend.title=element_blank())
giovani

#Job Plot
positions <- c("blue-collar", "management", "technician", "admin.", "services", "retired", "self-employed", "entrepreneur", "unemployed", "housemaid", "student", "unknown")
p <- ggplot(bank, aes(x = job)) + geom_bar(aes(fill = bank.full$y)) + scale_x_discrete(limits = positions)+ scale_fill_manual(aesthetics = "fill",values = mypalette)+
  labs(subtitle="Jobs of clients contacted", y="Count")+ theme(legend.position="right",
                                                               axis.title.x=element_blank(),
                                                               legend.title=element_blank())
p
jobss = plyr::count(bank.full$job[bank.full$y=="yes"])
fulljobs = plyr::count(bank.full$job)
jobsno = plyr::count(bank.full$job[bank.full$y=="no"])
conta4 = plyr::count(bank.full$y)
jobss[2,2]/conta4[2,2] #percetage of new deposit from blue collar 13%
jobss[5,2]/conta4[2,2] #percentage of new deposit from management 24%
jobss[10,2]/conta4[2,2] #percentage of new deposit from technician 15%
jobss[1,2]/conta4[2,2] #percentage of new deposit from admin 12%
jobss[9,2]/conta4[2,2] #percentage of new deposit from student 5%
jobss[9,2]/fulljobs[9,2] #percentage of success among students 28%
jobss[5,2]/fulljobs[5,2] #percentage of success among management 13%

#Marital status
marital_plot = ggplot2::ggplot(bank.full,aes(x=marital))+
  geom_bar(aes(fill = bank.full$y))+ labs(title="Marital status of clients contacted", y="Count")+ scale_fill_manual(aesthetics = "fill",values = mypalette) +
  theme(legend.position="right",
        axis.title.x=element_blank(),
        legend.title=element_blank())
marital_plot
conta1 = plyr::count(bank.full$marital[bank.full$y=="yes"])
conta2 = plyr::count(bank.full$marital)
conta3 = plyr::count(bank.full$marital[bank.full$y=="no"])
conta4 = plyr::count(bank.full$y)
percdiv = conta1[1,2]/conta2[1,2] ## percentage of divorced that opened a new deposit, about 12%
percmarr = conta1[2,2]/conta2[2,2] ## percentage of married that opened a new deposit, about 10%
percsin = conta1[3,2]/conta2[3,2] ## percentage of single that opened a new deposit, about 15%
percdivyes = conta1[1,2]/conta4[2,2] ## percentage of single that onened over the total sum of new deposit
percmarryes = conta1[2,2]/conta4[2,2] ## percentage of married that opened over te total sum of new deposit
percsinyes = conta1[3,2]/conta4[2,2] ##percentage of single thata opened over the total sum of new deposit
percdivno = conta3[1,2]/conta4[1,2] ##percentage of divorce that didn't open over the total sum of not opened deposit
percmarrno = conta3[2,2]/conta4[1,2] ##percentage of married that didn't open over the total sum of not opened deposit
percsinno = conta3[3,2]/conta4[1,2] ##percentage of single that didn't open over the total sum of not opened deposit

print(c(percdivyes, percdivno, percdiv))
print(c(percmarryes, percmarrno, percmarr))
print(c(percsinyes, percsinno, percsin))
##We see that more or less the divorced percentage changes slightly between the different percentages, 
divorced = conta2[1,2]/45211
married = conta2[2,2]/45211
single = conta2[3,2]/45211

#Education
education_plot = ggplot2::ggplot(bank.full,aes(x=education))+
  geom_bar(aes(fill = bank.full$y)) +
  scale_fill_manual(aesthetics = "fill",values = mypalette) +
  labs(title="Educational level of clients contacted", y="Count")+
  theme(legend.position="right",axis.title.x=element_blank(),legend.title=element_blank())

education_plot

educationss = plyr::count(bank.full$education[bank.full$y=="yes"])
fulledu = plyr::count(bank.full$education)
eduno = plyr::count(bank.full$education[bank.full$y=="no"])
conta4 = plyr::count(bank.full$y)
educationss[2,2]/conta4[2,2] #percentage of new deposit from secondary 46%
educationss[3,2]/conta4[2,2] #percentage of new deposit from tertiary 38%
educationss[1,2]/conta4[2,2] #percentage of new deposit from primary 11%
educationss[4,2]/conta4[2,2] #percentage of new deposit from unknowns 5%
educationss[2,2]/eduno[2,2] #percentage of success from secondary 12%
educationss[3,2]/eduno[3,2] #percentage of success from tertiary 18%
educationss[1,2]/eduno[1,2] #percentage of success from primary 9%
educationss[4,2]/eduno[4,2] #percentage of success from unknown 15%

#Default 
default_plot = ggplot2::ggplot(bank.full,aes(x=default))+
  geom_bar(aes(fill = bank.full$y))
default_plot

#Balance
balance_plot = ggplot2::ggplot(bank.full,aes(x=balance, color = bank.full$y))+
  geom_point(aes(y = bank.full$y, fill = bank.full$y),  alpha =0.1) +
  coord_cartesian(xlim=c(5000,60000))+
  geom_jitter(aes(y=bank.full$y), alpha =0.3)+
  scale_color_manual(aesthetics = "color",values = mypalette)+
  labs(title="Balance", subtitle="Average yearly balance in € of client contacted", y="Outcome")+
  theme(legend.position="right",axis.title.x=element_blank(),legend.title=element_blank())

balance_plot

second_balance_plot = ggplot2::ggplot(bank.full,aes(x=balance))+
  geom_histogram(aes(fill = bank.full$y))+
  xlim(5000, 25000) +
  ylim(0,100)+
  scale_fill_manual(aesthetics = "fill",values = mypalette)+
  theme(legend.position="right",axis.title.x=element_blank(),legend.title=element_blank())

second_balance_plot

#Housing
housing_plot = ggplot2::ggplot(bank.full,aes(x=housing))+
  geom_bar(aes(fill = bank.full$y))+
  scale_fill_manual(aesthetics = "fill",values = mypalette)+
  labs(title = "Clients have a house loan or not?", y="Count") +
  theme(legend.position="right", axis.title.x=element_blank(),legend.title=element_blank())

housing_plot

housingsss = plyr::count(bank.full$housing[bank.full$y=="yes"])
fullhou = plyr::count(bank.full$housing)
houno = plyr::count(bank.full$housing[bank.full$y=="no"])
conta4 = plyr::count(bank.full$y)
housingsss[1,2]/conta4[2,2] #percentage of new deposit from no HL 63%
housingsss[2,2]/conta4[2,2] #percentage of new deposit from yes HL 36%
housingsss[1,2]/fullhou[1,2] #percentage of success from no 16.7%
housingsss[2,2]/fullhou[2,2] #percentage of success from yes 7.6%

#Loan 
loan_plot = ggplot2::ggplot(bank.full,aes(x=loan))+
  geom_bar(aes(fill = bank.full$y))
loan_plot

#Contact 
contact_plot = ggplot2::ggplot(bank.full,aes(x=contact))+
  geom_bar(aes(fill = bank.full$y))
contact_plot

#Month
month_order = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
month_plot = ggplot2::ggplot(bank.full,aes(x=month))+
  geom_bar(aes(fill = bank.full$y))+
  scale_x_discrete(limits = month_order)+
  labs(title = "Month of last contact", y = "Count")+
  theme(legend.position="right",axis.title.x=element_blank(),legend.title=element_blank())+
  scale_fill_manual(aesthetics = "fill",values = mypalette) 

month_plot

monthsss = plyr::count(bank.full$month[bank.full$y=="yes"])
fullmonth = plyr::count(bank.full$month)
monthno = plyr::count(bank.full$month[bank.full$y=="no"])
conta4 = plyr::count(bank.full$y)
monthsss[8,2]/fullmonth[8,2] #percentage of success 52% march
monthsss[7,2]/fullmonth[7,2] #percentage of success 10% june
monthsss[12,2]/fullmonth[12,2] #percentage of success 46% september
monthsss[3,2]/fullmonth[3,2] #percentage of success 46% december


#Duration
duration_plot = ggplot2::ggplot(bank,aes(x=duration))+
  geom_bar(aes(fill = bank$y))+
  ylim(0,180)+
  scale_fill_manual(aesthetics = "fill",values = mypalette)+
  xlim(0,1000)
duration_plot 

#Campaign
campaign_plot = ggplot2::ggplot(bank.full,aes(x=campaign))+
  geom_bar(aes(fill = bank.full$y))+
  xlim(0,38)+
  scale_fill_manual(aesthetics = "fill",values = mypalette) +
  labs(title = "Number of contacts performed for each client", y = "Count")+
  theme(legend.position="right",axis.title.x=element_blank(),legend.title=element_blank())

campaign_plot

#Pdays 
pdays_plot = ggplot2::ggplot(bank.full,aes(x=pdays))+
  geom_point(aes(y= bank.full$y, fill = bank.full$y))
pdays_plot

#Previous 
previous_plot = ggplot2::ggplot(bank.full,aes(x=previous))+
  geom_bar(aes(fill = bank.full$y)) + xlim(0,30) + ylim(0,5000)
previous_plot

#Poutcome
poutcome_plot = ggplot2::ggplot(bank.full,aes(x=poutcome))+
  geom_bar(aes(fill = bank.full$y)) +
  scale_fill_manual(aesthetics = "fill",values = mypalette)+
  labs(title="Previous results", y="Count") +
  theme(legend.position="right",axis.title.x=element_blank(), legend.title=element_blank())
poutcome_plot


### Data Analysis

### Train_test Split
set.seed(1234)
train.indices <- sample(nrow(bank),nrow(bank)*0.8)
train <- bank[train.indices, ]
test  <- bank[-train.indices, ]

### Logistic regression
logreg_model = glm(train$y ~., family = binomial,data =train)
summary(logreg_model)
logreg_model$coefficients
anov = anova(logreg_model, test = "Chisq")
hatprob=predict(logreg_model,newdata=test,type=c("response"))
predicted.classes <- ifelse(hatprob> 0.5, "yes", "no")
confMat= table(predicted.classes, test$y)
addmargins(confMat)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy   # 0.9026872 really high but not the evaluation metric we should look at with this unbalanced dataset

## Function for retrieving confusion matrix and metrics, author: Raffaele Argiento
myroc <- function(eps,hatprob,y,add_point=FALSE){
  
  n <- length(hatprob)
  haty_new <- rep(0,n)
  haty_new[hatprob>eps] <- 1
  
  if(eps< min(hatprob) || eps>=max(hatprob)){stop("Change eps is too small or too large")}
  conf_mat <- table(y,haty_new)
  
  cat("The threshold is:",eps,"\n")
  cat("Confound matrix\n")
  print(conf_mat)
  accuracy <- sum(diag(conf_mat))/sum(conf_mat)
  cat("Accuracy: ",accuracy,"\n")
  # TPR 
  sensitivity <- conf_mat[2,2]/(sum(conf_mat[2,]))
  cat("Sensitivity: ",sensitivity,"\n")
  # FPR
  specificity <- conf_mat[1,1]/sum(conf_mat[1,])
  ## 1-specificity ## FPR

  cat("Specificity:",specificity,"\n")
  
  if(add_point){
    points(specificity,sensitivity,cex=2)
  }
  return(c(accuracy,sensitivity,specificity))
}

myroc(eps=0.5,hatprob=hatprob,y=test$y)
# sensitivity is too low, not acceptable in our case

log_roc <- roc(test$y ~ hatprob,algorithm=1)
coords(log_roc,"best")
# threshold specificity sensitivity   auc
# 0.08952067  0.79610195  0.89316651  0.909
myroc(eps=0.08952067,hatprob=hatprob,y=test$y)
plot(roc(test$y,hatprob), legacy.axes = TRUE ,lwd=3, col= "dodgerblue4", print.auc = TRUE, print.thres = 0.08952067)
# Very good performance, the model misses only 111 possible positive clients. 
## Logistic Goodness
resp  <- residuals(logreg_model, type = "pearson")
resd   <- residuals(logreg_model, type = "deviance")

## Deviance and Pearson's statistic
## Binned residuals

binnedplot(fitted(logreg_model), rstandard(logreg_model), main="")
binnedplot(fitted(logreg_model), resp , main="")
binnedplot(fitted(logreg_model), resd , main="")
plot(logreg_model)

### SVM
# Given our need to modify the treshold we don't expect a good performance from 
# an hard classifier such as SVM, still we decided to try.
# Basically will produce an example of why accuracy isn't a good evaluation metric for this problem

svm_tuned <- tune(svm, y ~., data = train, kernel = "polynomial",cost = c(0.01,0.001,0.1,1,10,100,1000), degree = c(2,3,4,5)) # grid_search
saveRDS(svm_tuned, file = ".../svm_tuned.rds")

svm_tuned$best.performance
svm_model = svm_tuned$best.model
pred_svm = predict(svm_model, newdata = test)
confMat= table(pred_svm, test$y)
addmargins(confMat)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy  # very high but only because of imbalanced dataset. We could have try
# some resambling method but that would mean to add bias anche change the test dataset for all the other models

plot(roc(as.numeric(test$y),as.numeric(pred_svm)), legacy.axes = TRUE ,lwd=3, col= "dodgerblue4", print.auc = TRUE)
svm_roc = roc(as.numeric(test$y),as.numeric(pred_svm))
coords(svm_roc,"best")
# specificity sensitivity 
# 0.9873813   0.1636189 
# -----------------> problem we don't have probabilities

### Decision Trees
library(rpart)
rpmodel <- rpart(train$y~.,data=train, control=rpart.control(minsplit=1, minbucket=1, cp=0.001, xval = 10),parms=list(split="gini"))   # best model, cp choosen by auc comparison of different values
printcp(rpmodel)
plotcp(rpmodel)
predicted.classes <- predict(rpmodel, newdata=test, type=c("prob"))
confMat= table(predicted.classes, test$y)
addmargins(confMat)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy    #  0.9026872

dt_probs = as.data.frame(predicted.classes)
DT_roc <- roc(test$y ~ dt_probs$yes,algorithm=1)
coords(DT_roc,"best")
# threshold specificity sensitivity   auc
# 0.07613691  0.85869565  0.74398460   0.832
myroc(eps=0.07613691,hatprob=dt_probs$yes,y=test$y)
plot(roc(test$y,dt_probs$yes), legacy.axes = TRUE ,lwd=3, col= "dodgerblue4", print.auc = TRUE, print.thres = 0.07613691)
# ------> repeat this for different complexity parameters
## Manual gridsearch decision tree

# cp = 0.001
rpmodel <- rpart(train$y~.,data=train,
                 control=rpart.control(minsplit=1, minbucket=1, cp=0.001, xval = 10),parms=list(split="gini"))
predicted.classes <- predict(rpmodel, newdata=test, type=c("prob"))
dt_probs = as.data.frame(predicted.classes)
auc(test$y,dt_probs$yes)
# cp = 0.01
rpmodel <- rpart(train$y~.,data=train,
                 control=rpart.control(minsplit=1, minbucket=1, cp=0.01, xval = 10),parms=list(split="gini"))
predicted.classes <- predict(rpmodel, newdata=test, type=c("prob"))
dt_probs = as.data.frame(predicted.classes)
auc(test$y,dt_probs$yes)
# cp = 0.005
rpmodel <- rpart(train$y~.,data=train,
                 control=rpart.control(minsplit=1, minbucket=1, cp=0.005, xval = 10),parms=list(split="gini"))
predicted.classes <- predict(rpmodel, newdata=test, type=c("prob"))
dt_probs = as.data.frame(predicted.classes)
auc(test$y,dt_probs$yes)
# cp = 0.003
rpmodel <- rpart(train$y~.,data=train,
                 control=rpart.control(minsplit=1, minbucket=1, cp=0.003, xval = 10),parms=list(split="gini"))
predicted.classes <- predict(rpmodel, newdata=test, type=c("prob"))
dt_probs = as.data.frame(predicted.classes)
auc(test$y,dt_probs$yes)

## Plot of not enough deep tree, for graphical purposes only
rpmodel_plot <- rpart(train$y~.,data=train,
                 control=rpart.control(minsplit=1, minbucket=1, cp=0.01, xval = 10),parms=list(split="gini"))

rpart.plot::rpart.plot(rpmodel_plot) 


### Xgboost

nrounds = 1000
tune_grid <- expand.grid(   # prepare gridsearch for tuning model
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(4,7,10),
  gamma =0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 10, # 10 folds 
  verboseIter = FALSE, 
  allowParallel = TRUE,
  classProbs=TRUE
)

xgb_train_mat = data.matrix(train[-17])
levels(bank$y) = c("no","yes") #0 = no, 1= yes
xgb_test_mat =data.matrix(test[-17])

xgb_tune <- caret::train(    # model fitting
  x = xgb_train_mat,
  y = train$y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

saveRDS(xgb_tune, file = ".../xgb_tuned.rds")

bst_model = xgb_tune$finalModel
bst_pred = predict(bst_model, newdata = xgb_test_mat)
actual_pred = 1- bst_pred

xgb_roc <- roc(test$y ~ actual_pred,algorithm=1)
coords(xgb_roc,"best")
myroc(eps=0.1251488,hatprob=actual_pred,y=test$y)
#threshold specificity sensitivity   auc
#0.1251488   0.8497001   0.8912416   0.933
plot(roc(test$y,actual_pred), legacy.axes = TRUE ,lwd=3, col= "dodgerblue4", print.auc = TRUE,print.thres = 0.1251488 )

#### All ROCs
Xgb = roc(test$y,actual_pred)
DT = roc(test$y,dt_probs$yes)
SVM = roc(as.numeric(test$y),as.numeric(pred_svm))
LR = roc(test$y,hatprob)
plot(Xgb, legacy.axes = TRUE ,lwd=3, col= "dodgerblue4")
lines(DT, lwd=3, col= "seagreen")
lines(SVM, lwd=3, col= "slateblue")
lines(LR, lwd=3, col= "firebrick")
legend( "right", c("XGB  0.932",  "LR      0.909","DT      0.832", "SVM  0.576"), 
        text.col=c("dodgerblue4", "firebrick", "seagreen", "slateblue") )
# By plotting all the roc curves on the same graph it is even more clear that xgboost is the model that is performing
# better. We need to do some considerations before choosing the best model for our project. Clearly the alternative
# are logistic regression and xgboost. The first is the quickest model by far considering training time, the latter 
# has a better AUC because it increases the specificty. Regarding the sensitivity, which is what we're looking for,
# it is equal to 0.89 in both cases. Given those considerations we choose the logistic regression as the ideal model.
# Why?
# 1- Faster training
# 2- Same sensitivity
# 3- Allows interpretability