#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 3, Problem 4
# 2015-07-30
#
# The following code analyzes the federalist papers
#############################

# this code relies on variables defined in hw03_q3.R (and hw03_q2.R and hw03_q1.R)
# source("hw03_q3.R")

# load libraries
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# Set seed for pseudo-random numbers
set.seed(1)

# split training sets into 5 folds
# adapted from http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
ndx.hamilton.train <- sample(1:nrow(dtm.hamilton.train))
folds.hamilton.train <- split(ndx.hamilton.train,
                              ceiling(
                                seq_along(ndx.hamilton.train)/(length(ndx.hamilton.train)/5)
                              ))
ndx.madison.train <- sample(1:nrow(dtm.madison.train))
folds.madison.train <- split(ndx.madison.train,
                             ceiling(
                               seq_along(ndx.madison.train)/(length(ndx.madison.train)/5)
                             ))

# define the list of possible mu values
muList <- c(0.1/nrow(dictionary),
            1/nrow(dictionary),
            10/nrow(dictionary),
            100/nrow(dictionary),
            1000/nrow(dictionary))

# initalize empty matrices to store accuracy measures
correctRate <- matrix(, 5, 5, dimnames=list(paste("mu", 1:5, sep=""), paste("fold", 1:5, sep="")))
falseNegRate <- matrix(, 5, 5, dimnames=list(paste("mu", 1:5, sep=""), paste("fold", 1:5, sep="")))
falsePosRate <- matrix(, 5, 5, dimnames=list(paste("mu", 1:5, sep=""), paste("fold", 1:5, sep="")))


for(muChoice in 1:length(muList)){
  
  # define mu
  mu=muList[[muChoice]]
  
  for(foldLeftOut in 1:5){
    
    # get indices for documents in each fold
    ndx.hamilton.1valid <- folds.hamilton.train[[foldLeftOut]]
    ndx.hamilton.4train <- ndx.hamilton.train[!(ndx.hamilton.train %in% ndx.hamilton.1valid)]
    ndx.madison.1valid <- folds.madison.train[[foldLeftOut]]
    ndx.madison.4train <- ndx.madison.train[!(ndx.madison.train %in% ndx.madison.1valid)]
    
    # calculate the log priors
    log.prior.hamilton <- log(length(ndx.hamilton.4train)/(length(ndx.hamilton.4train)+length(ndx.madison.4train)))
    log.prior.madison <- log(length(ndx.madison.4train)/(length(ndx.hamilton.4train)+length(ndx.madison.4train)))
    
    # compute the log probabilities for the dictionary
    #   in Hamilton- and Madison- authored documents for the given mu
    logp.hamilton.4train <- make.log.pvec(dtm.hamilton.train[ndx.hamilton.4train, ], mu)
    logp.madison.4train <- make.log.pvec(dtm.madison.train[ndx.madison.4train, ], mu)
    
    # define a test dtm and record the true values
    dtm.test <- rbind(dtm.hamilton.train[ndx.hamilton.1valid, ],
                      dtm.madison.train[ndx.madison.1valid, ])
    trueValues <- c(rep("Hamilton", length(ndx.hamilton.1valid)),
                    rep("Madison", length(ndx.madison.1valid)))
    
    # calculate the log posterior probabilities
    log.post.hamilton1 <- log.prior.hamilton + (dtm.test %*% logp.hamilton.4train)
    log.post.madison1 <- log.prior.madison + (dtm.test %*% logp.madison.4train)
    
    # compare the log posterior probabilities and assign to the author
    # with highest probability
    prediction <- data.frame(trueValue=trueValues,
                             pred=(log.post.hamilton1 >= log.post.madison1))
    prediction$pred <- gsub(TRUE, "Hamilton", prediction$pred)
    prediction$pred <- gsub(FALSE, "Madison", prediction$pred)
    
    # calculate accuracy measurements
    correctRate[muChoice, foldLeftOut] <- sum(prediction$trueValue==prediction$pred)/nrow(prediction)
    falseNegRate[muChoice, foldLeftOut] <- nrow(filter(prediction, trueValue=="Hamilton" & pred=="Madison"))/nrow(filter(prediction, trueValue=="Hamilton"))
    falsePosRate[muChoice, foldLeftOut] <- nrow(filter(prediction, trueValue=="Madison" & pred=="Hamilton"))/nrow(filter(prediction, trueValue=="Madison"))
    
  }
}


# estimate model accuracies
correctRate
ggplot(data.frame(mu=muList,
                  correctRate=rowMeans(correctRate)),
       aes(x=log(mu), y=correctRate)) +
  geom_line() + geom_point()
ggsave(filename='writeup/4a_correctRate.png', width=5, height=2)


falseNegRate
ggplot(data.frame(mu=muList,
                  falseNegRate=rowMeans(falseNegRate)),
       aes(x=log(mu), y=falseNegRate)) +
  geom_line() + geom_point()
ggsave(filename='writeup/4a_falseNegRate.png', width=5, height=2)

falsePosRate
ggplot(data.frame(mu=muList,
                  falsePosRate=rowMeans(falsePosRate)),
       aes(x=log(mu), y=falsePosRate)) +
  geom_line() + geom_point()
ggsave(filename='writeup/4a_falsePosRate.png', width=5, height=2)



