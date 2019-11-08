library(frequency)
library(Hmisc)
library(Metrics)
library(randomForest)
library(lattice)
library(caret)

############## import data #########
Data <- read.csv("C:/Users/rxm1279/Desktop/Model Test Data.csv")


############## check missing values ################

#check missing cases
Data[!complete.cases(Data), ]

# double check missing value, so there is no missing value for any variables
sum(is.na(Data$y))
sum(is.na(Data$var_1)) 
sum(is.na(Data$var_2))
sum(is.na(Data$var_3))
sum(is.na(Data$var_4))
sum(is.na(Data$var_5))
sum(is.na(Data$var_6))
sum(is.na(Data$var_7))
sum(is.na(Data$var_8))
sum(is.na(Data$var_9))
sum(is.na(Data$var_10))


############# univariate analysis ###################

# summary descriptive statistics for all variables
summary(Data) 

freq(Data$var_10) # only var_10 is categorical variable so we should use freq() to check more descriptive statsitics details about it


# the histogram for all the variables
hist(Data$y)
hist(Data$var_1)
hist(Data$var_2)
hist(Data$var_3)
hist(Data$var_4)
hist(Data$var_5)
hist(Data$var_6)
hist(Data$var_7)
hist(Data$var_8)
hist(Data$var_9)
hist(Data$var_10)

boxplot(Data$y)
boxplot(Data$var_1)
boxplot(Data$var_2)
boxplot(Data$var_3)
boxplot(Data$var_4)
boxplot(Data$var_5)
boxplot(Data$var_6)
boxplot(Data$var_7)
boxplot(Data$var_8)
boxplot(Data$var_9)
boxplot(Data$var_10)

par(mfrow = c(2,2))
for (i in 1:4){
  boxplot(Data[,i])
} 


############# bivariate analysis #############

# correlation matrix and related p-value for all variables

library(Hmisc)
rcorr(as.matrix(Data))

# look at if there is linear relationship between dependent variable y and all independent variable x
scatter.smooth(Data$var_1, Data$y) 
scatter.smooth(Data$var_2, Data$y) 
scatter.smooth(Data$var_3, Data$y) 
scatter.smooth(Data$var_4, Data$y) 
scatter.smooth(Data$var_5, Data$y) 
scatter.smooth(Data$var_6, Data$y) 
scatter.smooth(Data$var_7, Data$y) 
scatter.smooth(Data$var_8, Data$y) 
scatter.smooth(Data$var_9, Data$y) 
scatter.smooth(Data$var_10, Data$y) 


############ training / testing ###############

#set a sample size for 80% of total observations
smp_size <- floor(0.8 * nrow(Data))

set.seed(100)
train_ind <- sample(seq_len(nrow(Data)), size = smp_size)

# create the train and test data sets
train <- Data[train_ind, ]
test <- Data[-train_ind, ]


################ modeling ##############

# build an initual linear regression model based on train dataset 
LRmodel <- lm(y ~ ., data = train) # . is include all the variables
summary(LRmodel)


# refine predictors by using backward selection and check model fit
step(LRmodel, direction = "backward")


# final model after fit
fitMod <- lm(y ~ var_1 + var_2 + var_3 + var_7 + var_9 + var_10, data = train)
summary(fitMod)

# do the prediction to the test dataset

LRpred <- predict(fitMod, test)

mse(test$y, LRpred)


library(msm)
model.diag.metrics <- augment(LRmodel)



############# test assumptions ########################

par(mfrow = c(2,2))
plot(fitMod)



RFmodel <- randomForest(y ~ ., data = train)
RFpred <- predict(RFmodel, test)
mse(test$y, RFpred)
plot(RFmodel)
