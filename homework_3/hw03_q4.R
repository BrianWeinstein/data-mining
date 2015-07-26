#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 3, Problem 4
# 2015-07-30
#
# The following code analyzes the federalist papers
#############################




mu=1/nrow(dictionary)

naive.bayes.5cv.mu <- function(log.prior.hamilton, log.prior.madison,
                               dtm.test, mu){
  
  
  # compute the log probabilities for the dictionary
  #   in Hamilton- and Madison- authored documents for the given mu
  logp.hamilton.train <- make.log.pvec(dtm.hamilton.train, mu)
  logp.madison.train <- make.log.pvec(dtm.madison.train, mu)
  
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
  
  
  #   predictions <- data.frame(trueValue=c(rep("Hamilton", nrow(dtm.hamilton.test)),
  #                                         rep("Madison", nrow(dtm.madison.test))),
  #                             pred=predictions)
  
}





# compute the confusion matrix
confusionMatrix(data=predictions$pred,
                reference=predictions$trueValue,
                dnn=c("Prediction", "True Value"),
                positive="Hamilton")









