#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 2, Problem 2
# 2015-07-23
#
# The following code performs kNN.
#############################

#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_2")

# load libraries
library(reshape2)
library(ggplot2)



#################
# Problem 5a
#################

# load the data and use dist() to get a distance matrix

#----- START YOUR CODE BLOCK HERE -----#

# read in the data
rawData <- read.csv(file="datasets/hw02_q5.csv")

# calculate distances among all pairs of inputs
distances <- dist(rawData[, 1:2])

# convert distances to matrix
distances <- as.matrix(distances, type="any")

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 5b
#################

#----- START YOUR CODE BLOCK HERE -----#


EstimatePoint <- function(k, pointNumber, testPoint){
  # estimates the value of a point in LOO CV in kNN
  # inputs: k = number of nearest neighbors to use
  #         pointNumber = row number of point to estimate
  #         testPoint = row number of point to leave out
  # output: estimated Y value for the point in row pointNumber
  
  # get the row numbers of the k nearest neighbors to pointNumber
  nearestPtRows <- which(distances[pointNumber, ] %in% sort(distances[pointNumber,][-testPoint])[1:k])
  
  # estimate the left out value as the mean of the k nearest y values
  mean(rawData$y[nearestPtRows])
  
}


GetAllEstimates <- function(k, testPoint){
  # for a given k and test point, estimates the values at all points
  # inputs: k = number of nearest neighbors to use
  #         testPoint = row number of point to leave out
  # output: estimated Y values for the points in row testPoint
  
  as.vector(
    sapply(1:nrow(rawData),
           function(pointNumber){
             estimates <- EstimatePoint(k=k, pointNumber=pointNumber, testPoint=testPoint)
           }
    )
  )
}


# initialize an empty data frame for k, testMSE, and trainMSE
errorTableLO1 <- data.frame(k=integer(), testMSE=numeric(), trainMSE=numeric())

# for testPoint=1, find the testing and training MSE using k=1:10
for(kValue in 1:10){
  
  estimates <- GetAllEstimates(k=kValue, testPoint=1)
  
  testMSE <- mean((estimates[1]-rawData$y[1])^2) # including only the testPoint
  trainMSE <- mean((estimates[-1]-rawData$y[-1])^2) # excluding the testPoint
  
  errorTableLO1 <- rbind(errorTableLO1, cbind(kValue, testMSE, trainMSE))
  
  rm(estimates, kValue, trainMSE, testMSE)
}

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 5c
#################

#----- START YOUR CODE BLOCK HERE -----#


# initialize an empty data frame for k, testMSE, and trainMSE
errorTableK <- data.frame(k=integer(), testMSE=numeric(), trainMSE=numeric())

# for each testPoint=1:nrow(rawData), ind the testing and training MSE using k=1:10
for(kValue in 1:10){
  
  # initialize an empty data frame for testPoint, testMSE, and trainMSE
  errorTableTestPoint <- data.frame(testPoint=integer(), testMSE=numeric(), trainMSE=numeric())
  
  for(testPoint in 1:nrow(rawData)){
    
    estimates <- GetAllEstimates(k=kValue, testPoint=testPoint)
    
    testMSE <- mean((estimates[testPoint]-rawData$y[testPoint])^2) # including only the testPoint
    trainMSE <- mean((estimates[-testPoint]-rawData$y[-testPoint])^2) # excluding the testPoint
    
    errorTableTestPoint <- rbind(errorTableTestPoint, cbind(testPoint, testMSE, trainMSE))
    
    rm(estimates, trainMSE, testMSE)
    
  }
  
  # average over all the MSEs in errorTableTestPoint and insert into errorTableK
  errorTableK <- rbind(errorTableK,
                       cbind(kValue=kValue,
                             testMSE=mean(errorTableTestPoint$testMSE),
                             trainMSE=mean(errorTableTestPoint$trainMSE)
                       )
  )
  
  rm(kValue, errorTableTestPoint)
  
}

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 5d
#################

plotData <- melt(errorTableK,
                 id.vars="kValue",
                 measure.vars=c("testMSE", "trainMSE"),
                 variable.name="set",
                 value.name="MSE")

ggplot(plotData, aes(x=kValue, y=MSE, color=set, shape=set)) +
  geom_point() +
  geom_line() + 
  theme_bw()
ggsave(filename='writeup/5d.png', width=6, height=2)





#################
# End of Script
#################
