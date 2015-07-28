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




# define mu
mu=1/nrow(dictionary)


# define the fold to leave out
foldLeftOut <- 1


# get indices for documents in each fold
ndx.hamilton.1valid <- folds.hamilton.train[[foldLeftOut]]
ndx.hamilton.4train <- ndx.hamilton.train[!(ndx.hamilton.train %in% ndx.hamilton.1valid)]
ndx.madison.1valid <- folds.madison.train[[foldLeftOut]]
ndx.madison.4train <- ndx.madison.train[!(ndx.madison.train %in% ndx.madison.1valid)]


# remove temporary lists
rm(ndx.hamilton.train, folds.hamilton.train, ndx.madison.train, folds.madison.train)


# calculate the log priors
log.prior.hamilton <- log(length(ndx.hamilton.4train)/(length(ndx.hamilton.4train)+length(ndx.madison.4train)))
log.prior.madison <- log(length(ndx.madison.4train)/(length(ndx.hamilton.4train)+length(ndx.madison.4train)))






# compute the log probabilities for the dictionary
#   in Hamilton- and Madison- authored documents for the given mu
logp.hamilton.4train <- make.log.pvec(dtm.hamilton.train[ndx.hamilton.4train, ], mu)
logp.madison.4train <- make.log.pvec(dtm.madison.train[ndx.madison.4train, ], mu)

# define a test dtm
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
correctRate <- sum(prediction$trueValue==prediction$pred)/nrow(prediction)
falseNegRate <- nrow(filter(prediction, trueValue=="Hamilton" & pred=="Madison"))/nrow(filter(prediction, trueValue=="Hamilton"))
falsePosRate <- nrow(filter(prediction, trueValue=="Madison" & pred=="Hamilton"))/nrow(filter(prediction, trueValue=="Madison"))





