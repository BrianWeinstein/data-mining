#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 4, Problem 8
# 2015-08-13
#
# The following code uses boosting to predict Salary in the Hitters data set.
#############################




# Setup ################################################################################

# set working directory
setwd("~/Documents/data-mining/homework_4")

# load libraries
library(ISLR) # for the Hitters dataset
library(gbm) # 8c - boosting
library(ggplot2)
theme_set(theme_bw())
library(glmnet) # 8e - lasso
library(dplyr)
library(randomForest) # 8g - bagging

# load data into global environment
HittersRaw <- Hitters # save raw data for debugging
Hitters <- Hitters




# Problem 8a ################################################################################

# remove observations with no salary info
Hitters <- Hitters[!is.na(Hitters$Salary), ]

# log transform the salaries
Hitters$Salary <- log(Hitters$Salary)




# Problem 8b ################################################################################

# create training and testing sets
Hitters.train <- Hitters[1:200, ]
Hitters.test <- Hitters[-(1:200), ]




# Problem 8c ################################################################################

# create a list of lambda values (shrinkage parameters) to test over
lambdaList <- seq(0.0001, 0.3, by=0.005)

# initialize an empty dataframe to store MSEs
mse.boost <- data.frame(lambda=numeric(), mse.train=numeric(), mse.test=numeric())

# test over the lambda grid
for(i in 1:length(lambdaList)){
  
  # train the model
  model.boost <- gbm(formula = Salary ~ ., data=Hitters.train,
                     n.trees=1000, shrinkage=lambdaList[i],
                     distribution="gaussian")
  
  # use model to predict values for the training and testing data
  pred.train.boost <- predict(model.boost, newdata=Hitters.train, n.trees=1000)
  pred.test.boost <- predict(model.boost, newdata=Hitters.test, n.trees=1000)
  
  # calculate the training and testing MSEs and insert into dataframe
  mse.boost <- rbind(mse.boost,
                     data.frame(lambda=lambdaList[i],
                                mse.train=mean((Hitters.train$Salary - pred.train.boost)^2),
                                mse.test=mean((Hitters.test$Salary - pred.test.boost)^2)))
  
}

# clear temporary definitions
rm(model.boost, pred.train.boost, pred.test.boost, i)

# plot training mse vs lambda
ggplot(mse.boost, aes(x=lambda, y=mse.train)) + 
  geom_line() + geom_point()
ggsave(filename='writeup/8c_trainMSE.png', width=5, height=3)




# Problem 8d ################################################################################

# plot testing mse vs lambda
ggplot(mse.boost, aes(x=lambda, y=mse.test)) + 
  geom_line() + geom_point()
ggsave(filename='writeup/8d_testMSE.png', width=5, height=3)




# Problem 8e ################################################################################

# ols regression
model.ols <- lm(formula = Salary ~ ., data=Hitters.train)
pred.test.ols <- predict(model.ols, newdata=Hitters.test)
mse.test.ols <- mean((Hitters.test$Salary - pred.test.ols)^2)

# lasso
model.lasso <- cv.glmnet(x=model.matrix(Salary ~ ., Hitters.train)[, -1],
                         y=Hitters.train$Salary,
                         alpha=1, family="gaussian",
                         type.measure="mse")
pred.test.lasso <-  predict(model.lasso, newx=model.matrix(Salary ~ ., Hitters.test)[, -1],
                            s="lambda.min", type="response")
mse.test.lasso <- mean((Hitters.test$Salary - (as.data.frame(pred.test.lasso)$"1"))^2)

# mse from boosting in part 8d
filter(mse.boost, mse.boost$mse.test==min(mse.boost$mse.test))
mse.test.boost <- filter(mse.boost, mse.boost$mse.test==min(mse.boost$mse.test)) %>%
  select(mse.test) %>%
  as.numeric()
lambda.test.boost <- filter(mse.boost, mse.boost$mse.test==min(mse.boost$mse.test)) %>%
  select(lambda) %>%
  as.numeric()




# Problem 8f ################################################################################

# defining model.boost with the lambda that minimizes test MSE
model.boost <- gbm(formula = Salary ~ ., data=Hitters.train,
                   n.trees=1000, shrinkage=lambda.test.boost,
                   distribution="gaussian")

# get relative influence statistics
summary(model.boost)




# Problem 8g ################################################################################

# Set seed for pseudo-random numbers
set.seed(1)

# train the model
model.bag <- randomForest(formula = Salary ~ .,
                          data=Hitters.train,
                          mtry=(ncol(Hitters.train) - 1), # consider *all* variables in
                                                          # each split (bagging)
                          ntree=1000,
                          importance=TRUE)

# use model to predict values for the testing data
pred.test.bag <- predict(model.bag,
                         newdata=Hitters.test[, -which(colnames(Hitters.test)=="Salary")])

# calculate the testing MSE
mse.test.bag <- mean((Hitters.test$Salary - pred.test.bag)^2)



