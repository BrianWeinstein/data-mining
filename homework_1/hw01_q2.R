#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 1, Problem 2
# 2015-07-13
#
# Chapter 2.4, Problem 9 from James/Witten/Hastie/Tibshirani, An Introduction to Statistical Learning, Springer 2014
#############################



#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_1")

# load libraries
library(ggplot2)
library(dplyr)
theme_set(theme_bw())

# read in the Auto.csv dataset
Auto <- read.csv(file="datasets/Auto.csv", header=T, na.strings="?")

# remove any entries with missing data
Auto <- na.omit(Auto)



#################
# Problem 9a
#################

head(Auto)

# check class of each column
str(Auto)

# reassign incorrectly-classed columns
Auto$origin <- as.factor(Auto$origin)



#################
# Problem 9b
#################

# min and max for each quantitative variable
data.frame(
  statistic=c("min", "max"),
  lapply(Auto,
         function(col){
           if(is.integer(col)|is.numeric(col)){
             range(col)
           } else {
             NA
           }
         }
  )
)



#################
# Problem 9c
#################

# mean and standard deviation for each quantitative variable
data.frame(
  statistic=c("mean", "sd"),
  lapply(Auto,
         function(col){
           if(is.integer(col)|is.numeric(col)){
             c(mean(col), sd(col))
           } else {
             NA
           }
         }
  )
)



#################
# Problem 9d
#################

# remove observations 10 through 85
autoSub <- Auto[-(10:85), ]

# min, max, mean, standard deviation for each quantitative variable
data.frame(
  statistic=c("min", "max", "mean", "sd"),
  lapply(autoSub,
         function(col){
           if(is.integer(col)|is.numeric(col)){
             c(range(col), mean(col), sd(col))
           } else {
             NA
           }
         }
  )
)



#################
# Problem 9e
#################

# scatter plots among all quantitative predictors
pairs(Auto[, 1:7])

# mpg by year
ggplot(Auto, aes(x=factor(year), y=mpg)) +
  geom_boxplot()
ggsave(filename='writeup/9e_mpg_vs_year.png', width=5, height=3)

# mpg by cylinders
ggplot(Auto, aes(x=factor(cylinders), y=mpg)) +
  geom_boxplot()
ggsave(filename='writeup/9e_mpg_by_cyl.png', width=5, height=3)

# mpg vs horsepower
ggplot(Auto, aes(x=horsepower, y=mpg)) + 
  geom_point()

# acceleration vs weight
ggplot(Auto, aes(x=weight, y=acceleration)) + 
  geom_point()
ggsave(filename='writeup/9e_acc_vs_weight.png', width=5, height=2.5)

# acceleration vs horsepower
ggplot(Auto, aes(x=horsepower, y=acceleration)) + 
  geom_point()
ggsave(filename='writeup/9e_acc_vs_hp.png', width=5, height=2.5)

# horsepower vs weight
ggplot(Auto, aes(x=weight, y=horsepower)) + 
  geom_point()
ggsave(filename='writeup/9e_hp_vs_weight.png', width=5, height=2.5)

# weight by origin (1. American, 2. European, 3. Japanese)
ggplot(Auto, aes(x=origin, y=weight)) + 
  geom_boxplot()
ggsave(filename='writeup/9e_weight_by_origin.png', width=5, height=2.5)

# mpg by origin (1. American, 2. European, 3. Japanese)
ggplot(Auto, aes(x=origin, y=mpg)) + 
  geom_boxplot()
ggsave(filename='writeup/9e_mpg_by_origin.png', width=5, height=2.5)

# mpg vs weight, by origin (1. American, 2. European, 3. Japanese)
ggplot(Auto, aes(x=weight, y=mpg, shape=origin, color=origin)) + 
  geom_point() +
  theme(legend.position=c(0.85,0.7)) 
ggsave(filename='writeup/9e_mpg_vs_weight_by_origin.png', width=5, height=2.5)



#################
# Problem 9f
#################

# mpg vs displacement
ggplot(Auto, aes(x=displacement, y=mpg)) + 
  geom_point()
ggsave(filename='writeup/9e_mpg_vs_disp.png', width=5, height=2.5)

# mpg vs acceleration
ggplot(Auto, aes(x=acceleration, y=mpg)) + 
  geom_point()
ggsave(filename='writeup/9e_mpg_vs_acc.png', width=5, height=2.5)
