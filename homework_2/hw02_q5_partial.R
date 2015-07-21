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


GetEstimate <- function(k, pointNumber, testPoint){
  # write a function to estimate the value of a point in LOO CV in kNN
  # Inputs: k = number of nearest neighbors to use
  #         pointNumber = row number of point to estimate
  #         testPoint = row number of point to leave out
  # Output: estimated Y value for the point in row testPoint
  
  # get the row numbers of the k nearest neighbors to pointNumber
  nearestPtRows <- which(distances[pointNumber, ] %in% sort(distances[pointNumber,][-testPoint])[1:k])
  
  # estimate the left out value as the mean of the k nearest y values
  mean(rawData$y[nearestPtRows])
  
}



k=1
testPoint=3

# for a given k and test point, estimate the values at each point
estimates <- as.vector(
  sapply(1:nrow(rawData),
         function(pointNumber){
           estimates <- GetEstimate(k=k, pointNumber=pointNumber, testPoint=testPoint)
         }
  )
)

estimates







#----- END YOUR CODE BLOCK HERE -----#




#################
# Problem 5c
#################

#----- START YOUR CODE BLOCK HERE -----#


GetEstimate <- function(testPoint, k){
  # write a function to estimate the value of a point left out in LOO cross validation in kNN
  # Inputs: testPoint = row number of point to leave out
  #         k = number of nearest neighbors to use
  # Output: estimated Y value for the point in row testPoint
  
  # get the row numbers of the k nearest neighbors
  nearestPtRows <- which(distances[testPoint, ] %in% sort(distances[testPoint, ])[1:k])
  
  # estimate the left out value as the mean of the k nearest y values
  mean(rawData[nearestPtRows, "y"])
  
}

TrainMSE <- function(true, estimate){
  mean((true-estimate)^2)
  
  
}





#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


