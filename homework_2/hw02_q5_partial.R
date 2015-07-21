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

GetEstimate <- function(testPoint, k){
  # write a function to estimate the value of a point left out in LOO cross validation in kNN
  # Inputs: testPoint = row number of point to leave out
  #         k = number of nearest neighbors to use
  # Output: estimated Y value for the point in row testPoint
  
  # get the row numbers of the k nearest neighbors, excluding the point iteself
  nearestPtRow <- which(distances[testPoint, ] %in% sort(distances[testPoint, ])[2:(k+1)])
  
  # estimate the left out value as the mean of the k nearest y values
  mean(rawData[nearestPtRow, "y"])

}










#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 5c
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


