#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 3, Problem 3
# 2015-07-30
#
# The following code analyzes the federalist papers
#############################

# this code relies on variables defined in hw03_q2.R (and hw03_q1.R)
# source("hw03_q2.R")

# load libraries
library(caret)

# classify the testing documents as either Hamilton or Madison
predictions <- naive.bayes(logp.hamilton.train,
                           logp.madison.train,
                           logPriorHamiltonTrain,
                           logPriorMadisonTrain,
                           rbind(dtm.hamilton.test, dtm.madison.test))
# 
predictions <- data.frame(trueValue=c(rep("Hamilton", nrow(dtm.hamilton.test)),
                                      rep("Madison", nrow(dtm.madison.test))),
                          pred=predictions)

# compute the confusion matrix

confusionMatrix(data=predictions$pred,
                reference=predictions$trueValue,
                dnn=c("Prediction", "True Value"),
                positive="Hamilton")

