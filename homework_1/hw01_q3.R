#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 1, Problem 3
# 2015-07-13
#
# Chapter 2.4, Problem 10 from James/Witten/Hastie/Tibshirani, An Introduction to Statistical Learning, Springer 2014
#############################



#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_1")

# load libraries
library(ggplot2)
theme_set(theme_bw())
library(dplyr)



#################
# Problem 10a
#################

library(MASS)

View(Boston)

?Boston

dim(Boston)

Boston <- Boston # adding to global environment for convenience



#################
# Problem 10b
#################


pairs(Boston)


ggplot(Boston, aes(x=medv, y=ptratio)) + 
  geom_point()
ggsave(filename='writeup/10b_ptratio_vs_medv.png', width=5, height=2.5)

ggplot(Boston, aes(x=dis, y=nox)) + 
  geom_point()
ggsave(filename='writeup/10b_nox_vs_dis.png', width=5, height=2.5)




#################
# Problem 10c
#################

ggplot(Boston, aes(x=ptratio, y=crim)) + 
  geom_point()

ggplot(Boston, aes(x=lstat, y=crim)) + 
  geom_point()
ggsave(filename='writeup/10b_crim_vs_lstat.png', width=5, height=2.5)

ggplot(Boston, aes(x=indus, y=crim)) + 
  geom_point()

ggplot(Boston, aes(x=age, y=crim)) + 
  geom_point()
ggsave(filename='writeup/10b_crim_vs_age.png', width=5, height=2.5)

ggplot(Boston, aes(x=dis, y=crim)) + 
  geom_point()
ggsave(filename='writeup/10b_crim_vs_dis.png', width=5, height=2.5)

ggplot(Boston, aes(x=medv, y=crim)) + 
  geom_point()
ggsave(filename='writeup/10b_crim_vs_medv.png', width=5, height=2.5)




#################
# Problem 10d
#################

# crime rates

ggplot(Boston, aes(x=crim)) +
  geom_histogram()

data.frame(rowNum=row.names(Boston), Boston) %>%
  filter(crim > 25) %>%
  select(rowNum) %>%
  c()


# tax rates

ggplot(Boston, aes(x=tax)) +
  geom_histogram()

data.frame(rowNum=row.names(Boston), Boston) %>%
  filter(tax > 600) %>%
  select(rowNum) %>%
  c()

# pupil-teacher ratios

ggplot(Boston, aes(x=ptratio)) +
  geom_histogram()

data.frame(rowNum=row.names(Boston), Boston) %>%
  filter(tax > 600) %>%
  select(rowNum) %>%
  c()


# Range of each predictor

data.frame(
  stat=c("min", "max"),
  lapply(Boston, function(col){range(col)}
  )
)


#################
# Problem 10e
#################

# number of town bordering the Charles river
Boston %>% filter(chas==1) %>% nrow()



#################
# Problem 10f
#################

# median pupil-teacher ratio
median(Boston$ptratio)


#################
# Problem 10g
#################

# suburbs with lowest medv
data.frame(rowNum=row.names(Boston), Boston) %>%
  filter(medv==min(medv))



#################
# Problem 10h
#################

# suburbs with more than 7 rooms/dwelling
data.frame(rowNum=row.names(Boston), Boston) %>%
  filter(rm > 7) %>%
  nrow()





