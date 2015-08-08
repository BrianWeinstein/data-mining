#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 4, Problem 8
# 2015-08-13
#
# The following code predicts Salary in the Hitters data set.
#############################




# Setup ################################################################################

# set working directory
setwd("~/Documents/data-mining/homework_4")

# load libraries
library(ISLR)
library(gbm)

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

# Set seed for pseudo-random numbers
set.seed(1)

# 












