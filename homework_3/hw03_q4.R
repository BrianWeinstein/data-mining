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



#################
# Problem 4a
#################

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



#################
# Problem 4c
#################

# initalize empty matrices to store accuracy measures
correctRate4C <- matrix(, 5, 1, dimnames=list(paste("mu", 1:5, sep=""), "testSet"))
falseNegRate4C <- matrix(, 5, 1, dimnames=list(paste("mu", 1:5, sep=""), "testSet"))
falsePosRate4C <- matrix(, 5, 1, dimnames=list(paste("mu", 1:5, sep=""), "testSet"))

# calculate the log priors
log.prior.hamilton4C <- log(nrow(dtm.hamilton.train)/(nrow(dtm.hamilton.train)+nrow(dtm.madison.train)))
log.prior.madison4C <- log(nrow(dtm.madison.train)/(nrow(dtm.hamilton.train)+nrow(dtm.madison.train)))

for(muChoice in 1:length(muList)){
  
  # define mu
  mu=muList[[muChoice]]
  
  # compute the log probabilities for the dictionary
  #   in Hamilton- and Madison- authored documents for the given mu
  logp.hamilton.train4C <- make.log.pvec(dtm.hamilton.train, mu)
  logp.madison.train4C <- make.log.pvec(dtm.madison.train, mu)
  
  # define a test dtm and record the true values
  dtm.test <- rbind(dtm.hamilton.test,
                    dtm.madison.test)
  trueValues <- c(rep("Hamilton", nrow(dtm.hamilton.test)),
                  rep("Madison", nrow(dtm.madison.test)))
  
  # calculate the log posterior probabilities
  log.post.hamilton4C <- log.prior.hamilton4C + (dtm.test %*% logp.hamilton.train4C)
  log.post.madison4C <- log.prior.madison4C + (dtm.test %*% logp.madison.train4C)
  
  # compare the log posterior probabilities and assign to the author
  # with highest probability
  prediction <- data.frame(trueValue=trueValues,
                           pred=(log.post.hamilton4C >= log.post.madison4C))
  prediction$pred <- gsub(TRUE, "Hamilton", prediction$pred)
  prediction$pred <- gsub(FALSE, "Madison", prediction$pred)
  
  # calculate accuracy measurements
  correctRate4C[muChoice, 1] <- sum(prediction$trueValue==prediction$pred)/nrow(prediction)
  falseNegRate4C[muChoice, 1] <- nrow(filter(prediction, trueValue=="Hamilton" & pred=="Madison"))/nrow(filter(prediction, trueValue=="Hamilton"))
  falsePosRate4C[muChoice, 1] <- nrow(filter(prediction, trueValue=="Madison" & pred=="Hamilton"))/nrow(filter(prediction, trueValue=="Madison"))
  
}

# estimate model accuracies
t(correctRate4C)
ggplot(data.frame(mu=muList,
                  correctRate=c(correctRate4C)),
       aes(x=log(mu), y=correctRate)) +
  geom_line() + geom_point()
ggsave(filename='writeup/4c_correctRate.png', width=5, height=2)

t(falseNegRate4C)
ggplot(data.frame(mu=muList,
                  falseNegRate=c(falseNegRate4C)),
       aes(x=log(mu), y=falseNegRate)) +
  geom_line() + geom_point()
ggsave(filename='writeup/4c_falseNegRate.png', width=5, height=2)

t(falsePosRate4C)
ggplot(data.frame(mu=muList,
                  falsePosRate=c(falsePosRate4C)),
       aes(x=log(mu), y=falsePosRate)) +
  geom_line() + geom_point()
ggsave(filename='writeup/4c_falsePosRate.png', width=5, height=2)



#################
# Problem 4d
#################

# Calculate pct error for each value of mu
pctError <- data.frame(t((colMeans(correctRate)-t(correctRate4C))/t(correctRate4C)),
                       t((colMeans(falseNegRate)-t(correctRate4C))/t(falseNegRate4C)),
                       t((colMeans(falsePosRate)-t(correctRate4C))/t(falsePosRate4C)))
colnames(pctError) <- c("correctRate", "falseNegRate", "falsePosRate")

