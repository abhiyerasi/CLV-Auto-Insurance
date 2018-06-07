rm(list=ls(all=TRUE))

######################### Reading and seeing the structure of the data ###############

#DIY Set directory and read the data 

setwd("E:\\totally new\\newmith")
auto=read.csv("Train.csv",header=T)
autotest=read.csv("Test.csv",header=T)

# Remove the CustomerID as it doesnt make any sense in predicting the value of CLTV.
autoinsurance=subset(auto,select=-c(CustomerID,Location.Geo))
autoinsurancetest=subset(autotest,select = -c(Location.Geo))

#DIY Data Exploration - Check Data Structure, and Summary
str(autoinsurance)
str(autoinsurancetest)
summary(autoinsurance)
summary(autoinsurancetest)

autoinsurance$Income=as.numeric(as.character(autoinsurance$Income))


autoinsurance =autoinsurance[(rowSums(is.na(autoinsurance)) <= 2),]
###################### Eploratory Data Analysis #############################
autoinsurance$Number.of.Open.Complaints[is.na(autoinsurance$Number.of.Open.Complaints)] <- 0
autoinsurancetest$Number.of.Open.Complaints[is.na(autoinsurancetest$Number.of.Open.Complaints)] <- 0
# In order to remove the rows were the NA values are greater that 20 percent.

library(ggplot2)

## Imuting Na in the Coverage Column.
# Inorder to check the means of the various coverages and imputing based on that.
ggplot(data=autoinsurance,aes(Coverage,Monthly.Premium.Auto,colour="rainbow"))+geom_boxplot(na.rm = T)
ggplot(data=autoinsurance,aes(Monthly.Premium.Auto,colour="rainbow"))+geom_density(na.rm = T)

autoinsurance$Coverage=ifelse(is.na(autoinsurance$Coverage),(ifelse(autoinsurance$Monthly.Premium.Auto<75,"Basic",ifelse(autoinsurance$Monthly.Premium.Auto>=75&autoinsurance$Monthly.Premium.Auto<90,"Extended","Premium"))),autoinsurance$Coverage)
autoinsurance$Coverage=as.factor(autoinsurance$Coverage)
levels(autoinsurance$Coverage)=c("Basic","Extended","Premium","Basic","Extended","Premium")
summary(autoinsurance)
levels(autoinsurancetest$Coverage)

ggplot(data=autoinsurance,aes(Income,colour="rainbow"))+geom_density(na.rm = T)+facet_wrap(~Education)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Knn Imputation in order to impute the Na values
library(DMwR)
autoinsurance=knnImputation(autoinsurance,k=5)


sum(is.na(autoinsurance))
cor(autoinsurance$Customer.Lifetime.Value,autoinsurance$Monthly.Premium.Auto)
cor(autoinsurance$Customer.Lifetime.Value,autoinsurance$Months.Since.Policy.Inception)

## Replacing the various values of vehicle size and making itfactors
autoinsurance$Vehicle.Size=ifelse(autoinsurance$Vehicle.Size<=1,"Large",ifelse(autoinsurance$Vehicle.Size<=2,"mediumsize","Small"))
autoinsurance$Vehicle.Size=as.factor(autoinsurance$Vehicle.Size)
levels(autoinsurance$Vehicle.Size)=levels(autoinsurancetest$Vehicle.Size)
levels(autoinsurancetest$Vehicle.Size)
########

####### Outlier Treatments ##########################################
num=autoinsurance[sapply(autoinsurance,is.numeric)]
# As there are only 8 variables simply plot the box plot and the values above the q3+Iqr remove them
library(ggplot2)
ggplot(data = num,aes(x="",y=Customer.Lifetime.Value,col="rainbow"))+geom_boxplot() # Greater than 50000
autoinsurance=autoinsurance[autoinsurance$Customer.Lifetime.Value<50000,]

ggplot(data = num,aes(x="",y=Income,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=Monthly.Premium.Auto,col="rainbow"))+geom_boxplot() # greater than 160
#autoinsurance=autoinsurance[autoinsurance$Monthly.Premium.Auto<300,]


ggplot(data = num,aes(x="",y=Months.Since.Last.Claim,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=Months.Since.Policy.Inception,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=Number.of.Open.Complaints,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=Number.of.Policies,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=Total.Claim.Amount,col="rainbow"))+geom_boxplot() # Greater than 900 claim amount remove it.
#autoinsurance=autoinsurance[autoinsurance$Total.Claim.Amount<900,]
##############################################################################
forimpute=rbind(subset(autoinsurance,select = -c(Customer.Lifetime.Value)),subset(autoinsurancetest,select=-c(CustomerID)))
autoinsurancetestnew=knnImputation(forimpute,k=5)
autoinsurancetestinal=autoinsurancetestnew[9465:11231,]
all(autoinsurancetest$Coverage==autoinsurancetestinal$Coverage)
autoinsurancetestinal$CustomerID=autoinsurancetest$CustomerID

write.csv(autoinsurancetestinal,"finaltestfile.csv")
#levels(autoinsurance$Education)=c("Below Graduate","Below Graduate","Above Graduate","Below Graduate","Above Graduate")
#levels(autoinsurancetestinal$Education)=c("Below Graduate","Below Graduate","Above Graduate","Below Graduate","Above Graduate")

###########################################################################################
############################## Splitting into train test data #############################

## Taking only the variables which are enough predictors
#autoinsurance$PV=(autoinsurance$Monthly.Premium.Auto*autoinsurance$Months.Since.Policy.Inception)-autoinsurance$Total.Claim.Amount
#boxplot(autoinsurance$PV)
#autoinsurance=autoinsurance[autoinsurance$PV<10500,]

finaldf=autoinsurance
write.csv(finaldf,"finaldfcharactered.csv")
#DIY split the data into train and test data sets (70/30 Split)
#DIY save train data to a dataframe named "train" and test data to a dataframe named "test"
# Split dataframe into training & testing sets
library(caret)
set.seed(123)

inTrain <- createDataPartition(finaldf$Customer.Lifetime.Value, p = .7, list = FALSE)
Train <- finaldf[ inTrain, ] # Training dataset for all model development
Test <- finaldf[ -inTrain, ] # Final sample for model validation

# BUILD LINEAR REGRESSION MODEL 
LinReg1 =lm(formula =1/(Customer.Lifetime.Value)~., data = Train)
library(MASS)
stepAIC(LinReg1)
step=lm(formula = 1/(Customer.Lifetime.Value) ~ Coverage + EmploymentStatus + 
          Income + Marital.Status + Monthly.Premium.Auto + Number.of.Open.Complaints + 
          Number.of.Policies + Renew.Offer.Type + Vehicle.Class, data = Train)
# Build model with all attributes into model. 
# "TotalRevenueGenerated" is the target variable 
summary(LinReg1)
summary(step)

library(car)
vif(step)

linregtrain=predict(LinReg1)
linregtest=predict(LinReg1,subset(Test,select=-c(Customer.Lifetime.Value)))
lintrainR2=(cor(linregtrain,1/Train$Customer.Lifetime.Value))^2
lintestR2=(cor(linregtest,1/Test$Customer.Lifetime.Value))^2

Aictrain=predict(step)
Aictest=predict(step,Test)
AictrainR2=(cor(Aictrain,1/Train$Customer.Lifetime.Value))^2
AictestR2=(cor(Aictest,1/Test$Customer.Lifetime.Value))^2

r2=data.frame(lintrainR2*100,lintestR2*100,AictrainR2*100,AictestR2*100)
############################## Residuals plot for checking the assumptions ################

#Review the residual plots
par(mfrow=c(2,2))
plot(LinReg1)

#resid=residuals(LinReg1)
#table(resid)
library(gvlm)
gvlma(x=step)

par(mfrow=c(1,1)) #reset default
plot(LinReg1,which=4)
#########################################################################################
newfinaldf=subset(finaldf,select= c(Coverage,EmploymentStatus, 
                                    Income,Marital.Status,Monthly.Premium.Auto,Number.of.Open.Complaints, 
                                    Number.of.Policies,Renew.Offer.Type,Vehicle.Class,Customer.Lifetime.Value))
newinaltestdf=subset(autoinsurancetestinal,select= c(CustomerID,Coverage,EmploymentStatus, 
                                                     Income,Marital.Status,Monthly.Premium.Auto,Number.of.Open.Complaints, 
                                                     Number.of.Policies,Renew.Offer.Type,Vehicle.Class))
write.csv(newfinaldf,"traindontmess.csv")
write.csv(newinaltestdf,"testdontmess.csv")


library(randomForest)

model_rfnew0 <- randomForest(Customer.Lifetime.Value ~ Coverage + EmploymentStatus + 
                               Income + Marital.Status + Monthly.Premium.Auto + Number.of.Open.Complaints + 
                               Number.of.Policies + Renew.Offer.Type + Vehicle.Class , finaldf,ntree = 20,mtry = 2)
varimp=importance(model_rfnew0)
varImpPlot(model_rfnew0)

#Coverage,EmploymentStatus, 
# Income,Marital.Status,Monthly.Premium.Auto,Number.of.Open.Complaints, 
#  Number.of.Policies,Renew.Offer.Type,Vehicle.Class
