#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 3, Problem 1
# 2015-07-30
#
# The following code analyzes the federalist papers
#############################

#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_3")

# load libraries
library(tm)



#################
# Problem 1a
#################

# load functions from hw03.R
source("hw03.R")

# preprocess text
preprocess.directory("datasets/FederalistPapers/fp_hamilton_train")
preprocess.directory("datasets/FederalistPapers/fp_hamilton_test")
preprocess.directory("datasets/FederalistPapers/fp_madison_train")
preprocess.directory("datasets/FederalistPapers/fp_madison_test")



#################
# Problem 1b
#################

hamilton.train <- read.directory("datasets/FederalistPapers/fp_hamilton_train_clean")
hamilton.test <- read.directory("datasets/FederalistPapers/fp_hamilton_test_clean")
madison.train <- read.directory("datasets/FederalistPapers/fp_madison_train_clean")
madison.test <- read.directory("datasets/FederalistPapers/fp_madison_test_clean")



#################
# Problem 1c
#################

dictionary <- make.sorted.dictionary.df(c(hamilton.train, hamilton.test,
                                          madison.train, madison.test))




#################
# Problem 1d
#################

dtm.hamilton.train <- make.document.term.matrix(infiles=hamilton.train,
                                                dictionary=dictionary)
dtm.hamilton.test <- make.document.term.matrix(infiles=hamilton.test,
                                               dictionary=dictionary)
dtm.madison.train <- make.document.term.matrix(infiles=madison.train,
                                               dictionary=dictionary)
dtm.madison.test <- make.document.term.matrix(infiles=madison.test,
                                              dictionary=dictionary)



#################
# Problem 1e
#################

mu=1/nrow(dictionary)

logp.hamilton.train <- make.log.pvec(dtm.hamilton.train, mu)
logp.hamilton.test <- make.log.pvec(dtm.hamilton.test, mu)
logp.madison.train <- make.log.pvec(dtm.madison.train, mu)
logp.madison.test <- make.log.pvec(dtm.madison.test, mu)



#################
# End of Script
#################


