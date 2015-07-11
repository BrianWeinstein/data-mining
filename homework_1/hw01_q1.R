#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 1, Problem 1
# 2015-07-13
#
# Chapter 2.4, Problem 8 from James/Witten/Hastie/Tibshirani, An Introduction to Statistical Learning, Springer 2014
#############################



#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_1")

# load libraries
library(ggplot2)


#################
# Problem 8a
#################

# read in the College.csv dataset
college <- read.csv(file="datasets/College.csv")



#################
# Problem 8b
#################

rownames(college) <- college[, 1]
fix(college)

college <- college[, -1]
fix(college)



#################
# Problem 8c
#################

# 8ci
summary(college)

# 8cii
pairs(college[, 1:10])

# 8ciii
ggplot(data=college, aes(x=Private, y=Outstate)) + 
  geom_boxplot() +
  theme_bw()

# 8civ
Elite=rep("No",nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

summary(college)

ggplot(data=college, aes(x=Elite, y=Outstate)) + 
  geom_boxplot() +
  theme_bw()

# 8cv
par(mfrow=c(3,4)) # split plot window into 4x3 grid

lapply(c(5, 10, 20, 40), function(bins) hist(college$Room.Board, breaks=bins, main=paste(bins, " bins", sep="")))
lapply(c(5, 10, 20, 40), function(bins) hist(college$Personal, breaks=bins, main=paste(bins, " bins", sep="")))
lapply(c(5, 10, 20, 40), function(bins) hist(college$S.F.Ratio, breaks=bins, main=paste(bins, " bins", sep="")))

par(mfrow=c(1,1)) # reset plot window partition

# 8cvi
# Continue exploring the data, and provide a brief summary of what you discover.









