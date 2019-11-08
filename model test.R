library(frequency)
library(Hmisc)
library(Metrics)
library(randomForest)
library(ggplot2)
library(tidyverse)


############## import data #########
Data <- read.csv("/Users/maruixuan/Downloads/Model Test Data.csv")


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


############# bivariate analysis #############

# correlation matrix and related p-value for all variables

library(Hmisc)
rcorr(as.matrix(Data))

# look at if there is linear relationship between dependent variable y and all independent variables x

sca1 <- ggplot(Data, aes(x = var_1, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca1)

sca2 <- ggplot(Data, aes(x = var_2, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca2)

sca3 <- ggplot(Data, aes(x = var_3, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca3)

sca4 <- ggplot(Data, aes(x = var_4, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca4)

sca5 <- ggplot(Data, aes(x = var_5, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca5)

sca6 <- ggplot(Data, aes(x = var_6, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca6)

sca7 <- ggplot(Data, aes(x = var_7, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca7)

sca8 <- ggplot(Data, aes(x = var_8, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca8)

sca9 <- ggplot(Data, aes(x = var_9, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca9)

sca10 <- ggplot(Data, aes(x = var_10, y = y)) + geom_point() + geom_smooth(method = lm, se = TRUE)
plot(sca10)



############ training / testing ###############

#set a sample size for 80% of total observations
smp_size <- floor(0.8 * nrow(Data))

set.seed(100)
train_ind <- sample(seq_len(nrow(Data)), size = smp_size)

# create the train and test data sets
train <- Data[train_ind, ]
test <- Data[-train_ind, ]


################ Multiple Linear Regression ##############

# build an initial linear regression model based on train dataset 
LRmodel <- lm(y ~ ., data = train) # . is include all the variables
summary(LRmodel)


# refine predictors by using backward selection and check model fit
step(LRmodel, direction = "backward")


# final model after fit
fitMod <- lm(y ~ var_1 + var_2 + var_3 + var_7 + var_9 + var_10, data = train)
summary(fitMod)

# do the prediction to the test dataset

LRpred <- predict(fitMod, test)

# using MSE to check the model performance
mse(test$y, LRpred)



############# test assumptions ########################

par(mfrow = c(2,2))
plot(fitMod)


########### Random Forest ###################
# build the random forest model based on train data
RFmodel <- randomForest(y ~ ., data = train)
RFmodel

# make prediction for test data
RFpred <- predict(RFmodel, test)

# using MSE to check the model performance
mse(test$y, RFpred)

# exploring which predictor is more important for estimation with graph
as_tibble(importance(RFmodel))
order(importance(RFmodel))

varImpPlot(RFmodel)

