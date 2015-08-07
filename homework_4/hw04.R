#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 4
# 2015-08-13
#
# The following code analyzes the federalist papers
#############################



# Setup ################################################################################

# set working directory
setwd("~/Documents/data-mining/homework_4")

# load libraries
library(tm)

# run hw03_q1.R to create the document term matrices
#source("../homework_3/hw03_q1.R", chdir = TRUE)
# or alternatively, just load the saved data (and remove unnecessary definitions)
load("~/Documents/data-mining/homework_3/hw03_q1_DATA.RData")
rm(hamilton.test, hamilton.train, madison.test, madison.train, mu,
   logp.hamilton.test, logp.hamilton.train, logp.madison.test, logp.madison.train)

# create a training dataframe
dtm.train <- data.frame(cbind(
  c(rep(0, nrow(dtm.madison.train)), rep(1, nrow(dtm.hamilton.train))),
  rbind(dtm.madison.train, dtm.hamilton.train)))
colnames(dtm.train) <- c("y", as.vector(dictionary$word))

# create a testing dataframe
dtm.test <- data.frame(cbind(
  c(rep(0, nrow(dtm.madison.test)), rep(1, nrow(dtm.hamilton.test))),
  rbind(dtm.madison.test, dtm.hamilton.test)))
colnames(dtm.test) <- c("y", as.vector(dictionary$word))



# Probem 4a ################################################################################







