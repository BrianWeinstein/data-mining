#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 3, Problem 2
# 2015-07-30
#
# The following code analyzes the federalist papers
#############################

# Estimate the log priors from the training data
logPriorHamiltonTrain <- log(nrow(dtm.hamilton.train)/(nrow(dtm.hamilton.train)+nrow(dtm.madison.train)))
logPriorMadisonTrain <- log(nrow(dtm.madison.train)/(nrow(dtm.hamilton.train)+nrow(dtm.madison.train)))


naive.bayes <- function(logp.hamilton.train, logp.madison.train,
                        log.prior.hamilton, log.prior.madison, dtm.test){
  # Performs naive bayes classification
  # Inputs:  logp.hamilton.train  :   vector of log probabilities of words
  #                                     occurring in the hamilton training data
  #          logp.madison.train   :   vector of log probabilities of words
  #                                     occurring in the madison training data
  #          log.prior.hamilton   :   the log prior of hamilton documents
  #          log.prior.madison    :   the log prior of madison documents 
  #          dtm.test             :   a document-term-matrix to classify
  # Output:  Classification labels for each document in dtm.test
  
  # calculate the log posterior probabilities
  log.post.hamilton <- log.prior.hamilton + (dtm.test %*% logp.hamilton.train)
  log.post.madison <- log.prior.madison + (dtm.test %*% logp.madison.train)
  
  # compare the log posterior probabilities and assign to the author
  # with highest probability
  prediction <- data.frame(logPostHam=log.post.hamilton,
                           logPostMad=log.post.madison)
  prediction$pred <- (log.post.hamilton >= log.post.madison)
  prediction$pred <- gsub(TRUE, "Hamilton", prediction$pred)
  prediction$pred <- gsub(FALSE, "Madison", prediction$pred)
  
  # return a vector of the predictions
  return(prediction$pred)
  
}


naive.bayes(logp.hamilton.train,
            logp.madison.train,
            logPriorHamiltonTrain,
            logPriorMadisonTrain,
            rbind(dtm.hamilton.test, dtm.madison.test))

