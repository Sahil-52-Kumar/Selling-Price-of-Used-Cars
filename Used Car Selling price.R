rm(list = ls())
setwd("D:/Sahil/Sahil Kumar/programs & notes/Projects/Prediction Selling Price of Used Cars")
getwd()
options(max.print=999999)
# Read the CSV File 
train <- read.csv("train.csv", header = T)
head(train)
summary(train)
str(train)
## dataset for work
train1<-train
View(train1)

colSums(is.na(train1))
##=========================================================================
# separating Alphabetical and Numeric values and storing the data in train1
##=========================================================================

# manupulating values in engine Column

Engine1 <- gsub(" CC","",train1$Engine)
Engine2<-as.integer(Engine1)
class(Engine2)
# storing only numeric values of engine in train 1
train1$Engine<-Engine2 

#manipulating Values in Power Column
Power1 <- gsub(" bhp","",train1$Power)
Power2 <- as.numeric(Power1)
class(Power2)
train1$Power<-Power2

#manipulating Values in Mileage Column
Mileage1<- gsub(" .*","",train1$Mileage)
Mileage2<-as.numeric(Mileage1)
class(Mileage2)
train1$Mileage <- Mileage2
Mileage3<-which(train1$Mileage == 0)
train1[Mileage3,10]<-NA
Mileage4<-which(is.na(train1$Mileage))
class(Mileage4)

# Now the data is converted to Numeric.Lets check for Missing Values in train 1
colSums(is.na(train1))
# NA's in Engine = 29, Power=119, Mileage=1, Seats=33

#gsub(" .*","")- means keep the numeric value as remove the all alphabetic values & 
#"" means no replacement as there is no space. In case need replacement, mention that between ""
#gsub(".* ","")Remove the numeric value and keep all Alphabetic values, & "" means no replacement of removed values


#===============================================================================
# Data Imputation using MICE function of R 
#===============================================================================
library(mice)
methods(mice)
my_imp<-mice(train1,m=5,method = c("","","","","","","","pmm","pmm","pmm","pmm","","",""),maxit=100)

my_imp$imp
final_clean_ds<-complete(my_imp,5)
train2<-final_clean_ds
View(train2)
summary(train2)
str(train2)
colSums(is.na(train2))
#==============================================================================
# All NA values have been removed and the data is now totally clean for progress
#==============================================================================

# Adding a new column Brand in train 2
train2$Brand <- gsub(" .*", "", train1$Name)# extracting Brand name from Names

View(train2)


#===========================================
#===========================================
# Model building and variable screening
#===========================================
#===========================================

Fit0 <- lm(formula = Price~Year+Kilometers_Driven+factor(Location)+factor(Fuel_Type)+Engine+Power+Mileage+factor(Transmission)+Seats
           +factor(Owner_Type)+factor(Brand),data = train2)

summary(Fit0)

# Adj r2 = 0.7667, F statistic = 299.10 @ 53 & 4755
#=============================
# Diagnostic plots
#=============================


res = rstandard(Fit0)   # standardized residual
yhat = predict(Fit0) # predicted y


#par(mfrow=c(1,1))
#---------------------
# uncorrelated error
plot(res); abline(h=0); 
#---------------------

#---------------------
# constant variance
plot(yhat,res); abline(h=0); 
#---------------------


#---------------------
# multicollinearity
library(car)
vif(Fit0)
barplot(vif(Fit0)[,3],las=2)

#---------------------


##====================
# Normality Check
##====================


hist(res)
qqnorm(res);qqline(res) # showing residual is  not normal


#=============================
# Fixing violations
#=============================


# possibly non-normality and non-constant variance can be taken care of as follows:
ff=boxCox(Fit0)
best_lambda=ff$x[which.max(ff$y)]
best_lambda # Best Labda value = 0.02020202
Price1 =train2$Price^best_lambda
train2$Price1<-Price1
#---------------------
# X-outliers 
barplot(hatvalues(Fit0)) 
which(hatvalues(Fit0)>0.5)

# Y-outliers 
plot(res); abline(h=0); 
which(res>8)
train2[c(1404,4403,4675,4721),]
# Influential observations
barplot(cooks.distance(Fit0))
which(cooks.distance(Fit0)>1)

# removing influential values from data

train3<-train2[-4403,]
class(train3)
str(train3)
#===============================================================================
## checking model again

Fit1<- lm(formula = Price1~Year+Kilometers_Driven+factor(Location)+factor(Fuel_Type)+Engine+Power+Mileage+factor(Transmission)+Seats
          +factor(Owner_Type)+factor(Brand),data = train3)

summary(Fit1)


# Model has become better with Adj R2 = 0.9155, F stat= 983.1 @ 53 & 4754 DOF
# all variables are also significant
#===========================================================================
# 2nd step of Model evalautaion
#=============================
# Diagnostic plots
#=============================


res = rstandard(Fit1)   # standardized residual
yhat = predict(Fit1) # predicted y


#par(mfrow=c(1,1))
#---------------------
# uncorrelated error
plot(res); abline(h=0); # No Pattern
#---------------------

#---------------------
# constant variance
plot(yhat,res); abline(h=0); # No Pattern
#---------------------


#---------------------
# multicollinearity
library(car)
vif(Fit1)
barplot(vif(Fit1)[,3],las=2)

##====================
# Normality Check
##====================


hist(res)
qqnorm(res);qqline(res) # showing residual is  not normal


#---------------------
# automatic variable selection

#null = lm(Price1~1)
#full = Fit1

#aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
#finalaic = lm(aicmodel$terms)
#summary(finalaic)

#===========================================================================#
# Variable Interaction for Model fitting 

Fit2<- lm(formula = Price1~(Year+Kilometers_Driven+Engine+Power+Mileage+Seats)^5 # binary # ^5 means 5 level varaible selection
          +factor(Owner_Type)+factor(Brand)+factor(Location)+factor(Fuel_Type)
          +factor(Transmission),data = train3)

summary(Fit2)

# Adj R2 = 0.9315, F stat = 585.9 & 109 & 4698 DOF
#=================================================================================#

#==============================================================================#
# Price Prediction basis Fit 2
#============================================================================
test<- read.csv("test.csv",header = T)
class(test)
View(test)

##=========================================================================
# separating Alphabetical and Numeric values and storing the data in test1
##=========================================================================

# manupulating values in engine Column

TestEngine1 <- gsub(" CC","",test$Engine)
TestEngine2<-as.integer(TestEngine1)
class(TestEngine2)
# storing only numeric values of engine in train 1
test$Engine<-TestEngine2 

#manipulating Values in Power Column
TestPower1 <- gsub(" bhp","",test$Power)
TestPower2 <- as.numeric(TestPower1)
class(TestPower2)
test$Power<-TestPower2

#manipulating Values in Mileage Column
TestMileage1<- gsub(" .*","",test$Mileage)
TestMileage2<-as.numeric(TestMileage1)
test$Mileage<-TestMileage2
class(TestMileage2)
TestMileage3<-which(test$Mileage == 0)
test[TestMileage3,10]<-NA
TestMileage4<-which(is.na(test$Mileage))
class(TestMileage4)

# Now the data is converted to Numeric.Lets check for Missing Values in train 1
colSums(is.na(test))

which(test$Seats == 0) #52 no car have 0 seats

test[52,11] <- 5
test$Brand <- gsub(" .*", "", test$Name)
test[1042,14]<-"BMW"

which(is.na(test$Seats))

# NA's in Engine = 7, Power=27, Mileage=12, Seats=9

#gsub(" .*","")- means keep the numeric value as remove the all alphabetic values & 
#"" means no replacement as there is no space. In case need replacement, mention that between ""
#gsub(".* ","")Remove the numeric value and keep all Alphabetic values, & "" means no replacement of removed values

# Removing Dummy Column New_price from train 1

#===============================================================================
# Data Imputation using MICE function of R 
#===============================================================================
library(mice)
methods(mice)
testmy_imp<-mice(test,m=5,method = c("","","","","","","","pmm","pmm","pmm","pmm","","",""),maxit=100)
summary(test$Power)

testmy_imp$imp
testfinal_clean_ds<-complete(testmy_imp,3)
test1<-testfinal_clean_ds
View(test1)
summary(test1)
str(test1)
colSums(is.na(test1))


#=================================================
#Final Prediction of Price
#=================================================
predictionValues <- predict(Fit2, newdata=test1)
best_lambda

FinalPredictionvalues<- exp(log(predictionValues)/best_lambda)
class(FinalPredictionvalues)
Final_Prediction <- as.data.frame(FinalPredictionvalues)
library(writexl)

write.csv(Final_Prediction,"D:\\Sahil\\Sahil Kumar\\programs & notes\\Projects\\Prediction Selling Price of Used Cars\\Final_Prediction_sahil.csv")

