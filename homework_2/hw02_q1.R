#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 2, Problem 1
# 2015-07-23
#
# Manually going through the steps for PCA.
#############################



#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_2")

# load libraries


#################
# Problem 1a
#################

# read in hw02_q1_p1.csv
rawData <- read.csv(file="datasets/hw02_q1_p1.csv")

# column means
rawData.colmeans <- apply(rawData, 2, mean)

# row means
rawData.rowmeans <- apply(rawData, 1, mean)



#################
# Problem 1b
#################

# center the data
data <- as.matrix(apply(rawData, 2, function(col) col - mean(col)))

# empirical covariance matrix
sig <- (1/nrow(data))*t(data)%*%data



#################
# Problem 1c
#################

# eigenvalues and eigenvectors
evals <- eigen(sig)$values
evecs <- eigen(sig)$vectors



#################
# Problem 1d
#################
#d. (5 Points) Give all of the loadings and all of the scores for the data.

# loadings
evecs

# scores
data%*%evecs


# 
# e. (5 Points) Plot the proportion of variance captured against the number of components included. How many components should we include and why?
# f. (5 Points) Load hw02 q1 p2.csv. This has 5 new observations in the original coordinates. Give their scores.
# g. (5 Points) Now use only the first two scores to represent the observations from the previous part. What are the coordinates of the projections in the original space, x′? What is their Euclidean distance from the original data points?
# h. (5 Points) Define the error of a point as
# d(x′, x) = x′ − x,
# which is a 5-dimensional vector of errors. In what direction is d(x′,x) for the 5 new points? Why do you think this is?
# 
# 
# 
