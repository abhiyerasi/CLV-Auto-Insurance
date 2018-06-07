rm(list=ls(all=TRUE))

library(e1071)

library(caret) # for box-cox transformation

library(lmtest) # bptest for testing heteroscedasticity

library(gvlma) # global validation of linear model assumptions

######################### Reading and seeing the structure of the data ###############

#DIY Set directory and read the data 
setwd("E:/Cute2/CSE7302c_CUTe01_Exam-Files/data/Hopmonk Final data with 30 Variables")
hopmonkdata=read.csv("hopmonk.csv",header=T)
hopmonkdata=subset(hopmonkdata,select=-c(CONTACT_WID))
#DIY Data Exploration - Check Data Structure, and Summary
str(hopmonkdata)
levels(hopmonkdata$FavoriteSource30)=levels(hopmonkdata$FavoriteSource)
levels(hopmonkdata$FavoriteSource180)=levels(hopmonkdata$FavoriteSource180)

target=subset(hopmonkdata,select = c(Customer_value))
cathopmonk=hopmonkdata[sapply(hopmonkdata,is.factor)]
numerichopmonk=hopmonkdata[sapply(hopmonkdata,is.numeric)]
numerichopmonk=scale(subset(numerichopmonk,select = -c(Customer_value)))

hopmonkdata=data.frame(numerichopmonk,cathopmonk,target)

hopmonkdata$Customer_value=sqrt(hopmonkdata$Customer_value)


############################## Splitting into train test data #############################

#DIY split the data into train and test data sets (70/30 Split)
#DIY save train data to a dataframe named "train" and test data to a dataframe named "test"
# Split dataframe into training & testing sets
library(caret)
set.seed(100929)

inTrain <- createDataPartition(hopmonkdata$Customer_value, p = .7, list = FALSE)
Train <- hopmonkdata[ inTrain, ] # Training dataset for all model development

Test <- hopmonkdata[ -inTrain, ] # Final sample for model validation

##########################################################################################
############################ Regression with all the variables ###########################
##########################################################################################


# BUILD LINEAR REGRESSION MODEL 
LinReg1 =lm(formula =Customer_value~., data = Train)

# Build model with all attributes into model. 
# "TotalRevenueGenerated" is the target variable 
summary(LinReg1)


############################## Residuals plot for checking the assumptions ################

#Review the residual plots
par(mfrow=c(2,2))
plot(LinReg1)

#resid=residuals(LinReg1)
#table(resid)

#gvlma(x=LinReg1)

#library(olsrr)
#ols_bp_test(LinReg1)
#lmtest::bptest(LinReg1,studentize = F)

#plot(LinReg1,which=4)
#par(mfrow=c(1,1)) #reset default

# look at the residual and comment on them
# Do they  follow normal distribution? 

#yes they are normally distributed inspite of some values

# Look at the other residual plots and check 
# whether the linear regression assumptions are 
# satisfied or not ?
#Yes they are satisied with one value as a outlier and heteroscadacity is been prevaled
#by checking it with the varioius tests.

##################### Checking the residuals values of mse rmse #################
# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
train_error=regr.eval(Train$Customer_value, LinReg1$fitted.values) 
train_error_df=data.frame(train_error)

#Error verification on test data
test=subset(Test,select = -c(Customer_value))
target=Test$Customer_value
pred=predict(LinReg1,test)

Pred<- 
  regr.eval(target, pred)

test_error_df=data.frame(Pred)
colnames(test_error_df)="test_error"

error_df=data.frame(train_error_df,test_error_df)
##############################################################################################
###############################################################################################
#####################  Step AIC ###########################################3

#Running the stepaic
library(MASS)
step=stepAIC(object = LinReg1,direction = "both")

############################## Splitting into train test data #############################

#DIY split the data into train and test data sets (70/30 Split)
#DIY save train data to a dataframe named "train" and test data to a dataframe named "test"
# Split dataframe into training & testing sets
AIC_Train <- hopmonkdata[ inTrain, ] # Training dataset for all model development

AIC_Test <- hopmonkdata[ -inTrain, ] # Final sample for model validation
##########################################################################################
############################ Regression with all 32 variables ###########################
##########################################################################################


# BUILD LINEAR REGRESSION MODEL 
AIC_LinReg =lm(formula =Customer_value ~ Customer_value360 + Frequency + Frequency360 + 
                 Customer_value180 + Frequency7 + Number_Games_Played + Customer_value30 + 
                 Frequency180 + FreqGamePlay + FreqGamePlay360 + Frequency30 + 
                 No_of_Childrens + TenureDays + MaxChildAge + Recency360 + 
                 FreqGamePlay7 + pur_freq + avg_order_value + MinChildAge + 
                 maxRecencyCum + minRecencyCum + Country + FavoriteSource + 
                 FavoriteGame90 + FavoriteChannel180,data = AIC_Train)

# Build model with all attributes into model. 
# "TotalRevenueGenerated" is the target variable 
summary(AIC_LinReg)
############################## Residuals plot for checking the assumptions ################

#Review the residual plots
par(mfrow=c(2,2))
plot(AIC_LinReg)

#resid=residuals(LinReg1)
#table(resid)

gvlma(x=AIC_LinReg)

#library(olsrr)
#ols_bp_test(LinReg1)
lmtest::bptest(AIC_LinReg,studentize = F)

#plot(LinReg1,which=4)
par(mfrow=c(1,1)) #reset default

# look at the residual and comment on them
# Do they  follow normal distribution? 

#yes they are normally distributed inspite of some values

# Look at the other residual plots and check 
# whether the linear regression assumptions are 
# satisfied or not ?
#Yes they are satisied with one value as a outlier and heteroscadacity is been prevaled
#by checking it with the varioius tests.

##################### Checking the residuals values of mse rmse #################
# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
AIC_train_error=regr.eval(AIC_Train$Customer_value, AIC_LinReg$fitted.values) 
AIC_train_error_df=data.frame(AIC_train_error)

#Error verification on test data
AIC_test=subset(AIC_Test,select = -c(Customer_value))
AIC_target=AIC_Test$Customer_value
AIC_pred=predict(AIC_LinReg,AIC_test)

AIC_Pred<- 
  regr.eval(AIC_target, AIC_pred)

AIC_test_error_df=data.frame(AIC_Pred)
colnames(AIC_test_error_df)="AIC_test_error"

AIC_error_df=data.frame(train_error_df,test_error_df,AIC_train_error_df,AIC_test_error_df)
####################################################################################################
############################################################################################
######################## Ridge And Lasso Regression ####################################

#Converting categorical attributes into dummy variables
xfactors <- model.matrix( ~ Country+FavoriteSource+FavoriteChannel180+
                            FavoriteGame90,data = hopmonkdata)[,-1]




numeric_ridge_lasso=hopmonkdata[sapply(hopmonkdata,is.numeric)]
numeric_ridge_lasso=subset(numeric_ridge_lasso,select=-c(Customer_value))
lasso_target=subset(hopmonkdata,select=c(Customer_value))

#############################################################################
#Converted the data into matrix form to input into glm model
Lasso_data <- as.matrix(data.frame(numeric_ridge_lasso, xfactors))
Lasso_train = Lasso_data[inTrain,] 
Lasso_test = Lasso_data[-inTrain,]

#Target Varaible
ytrain=lasso_target$Customer_value[inTrain]
ytest = lasso_target$Customer_value[-inTrain]

# Lasso Regression  using glmnet - L1 norm
library(glmnet)
# fit model
fit1 <- glmnet(Lasso_train,ytrain, alpha=1)

plot(fit1,xvar="lambda",label=TRUE,main="Log Lambda vs Coefficients ")
plot(fit1,xvar="dev",label=TRUE)

#Model Selection
coef(fit1)
cv.lasso=cv.glmnet(Lasso_train,ytrain)
plot(cv.lasso)
coef(cv.lasso)
min(cv.lasso$lambda)
#lambda.min - value of lambda that gives minimum cvm - mean cross-validated error

# Make model based on the lambda value which has the least means square error.
fit2=glmnet(Lasso_train,ytrain,lambda=cv.lasso$lambda.min,alpha=1)
m=coef(fit2)

train_pred_lasso=data.frame(predict(fit2,Lasso_train))
test_pred_lasso=data.frame(predict(fit2,Lasso_test))
library(DMwR)
etrain_lasso=regr.eval(ytrain, predict(fit2,Lasso_train))
etest_lasso=regr.eval(ytest, predict(fit2,Lasso_test))
lasso_error=data.frame(etrain_lasso,etest_lasso)


#############################################################################
#############################################################################
#Converted the data into matrix form to input into glm model
Ridge_data <- as.matrix(data.frame(numeric_ridge_lasso, xfactors))
Ridge_train = Ridge_data[inTrain,] 
Ridge_test = Ridge_data[-inTrain,]

#Target Varaible
ytrain_ridge=lasso_target$Customer_value[inTrain]
ytest_ridge = lasso_target$Customer_value[-inTrain]

# Lasso Regression  using glmnet - L1 norm
library(glmnet)
# fit model
fit3 <- glmnet(Ridge_train,ytrain_ridge, alpha=0)

plot(fit3,xvar="lambda",label=TRUE)
plot(fit3,xvar="dev",label=TRUE)

#Model Selection
coef(fit3)
cv.ridge=cv.glmnet(Ridge_train,ytrain_ridge)
plot(cv.ridge)
coef(cv.ridge)
min(cv.ridge$lambda)
#lambda.min - value of lambda that gives minimum cvm - mean cross-validated error

# Make model based on the lambda value which has the least means square error.
fit4=glmnet(Ridge_train,ytrain_ridge,lambda=cv.lasso$lambda.min,alpha=1)
m=coef(fit4)

train_pred_ridge=data.frame(predict(fit4,Ridge_train))
test_pred_ridge=data.frame(predict(fit4,Ridge_test))
library(DMwR)
etrain_ridge=regr.eval(ytrain_ridge, predict(fit4,Ridge_train))
etest_ridge=regr.eval(ytest_ridge, predict(fit4,Ridge_test))
ridge_error=data.frame(etrain_ridge,etest_ridge)

totalerror=t(data.frame(AIC_error_df,lasso_error,ridge_error))


################################################################################
#######################################################################
######################### K folds cross validation####################
x=Train[,-36]
y=Train$Customer_value
x_test=Test[,-36]
y_test=Test$Customer_value
model=train(x,y,'lm',trControl = trainControl(method = 'cv',number = 15))
'''
Linear Regression 

39663 samples
35 predictor

No pre-processing
Resampling: Cross-Validated (15 fold) 
Summary of sample sizes: 37018, 37019, 37020, 37019, 37018, 37017, ... 
Resampling results:

RMSE      Rsquared 
10.88842  0.9547582

Tuning parameter 'intercept' was held constant at a value of TRUE
'''
kfold_train=predict(model$finalModel,x)
kfold_test=predict(model$finalModel,x_test)
library(DMwR)
regr.eval(y_test,kfold_test)
regr.eval(y,kfold_train)
