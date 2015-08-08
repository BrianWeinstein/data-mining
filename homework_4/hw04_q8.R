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
library(ISLR)
library(gbm)
library(ggplot2)
theme_set(theme_bw())

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

# Perform boosting on the training set with 1,000 trees for a range of values 
# of the shrinkage parameter λ. Produce a plot with different shrinkage values 
# on the x-axis and the corresponding training set MSE on the y-axis.


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





