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
library(rpart)
library(dplyr)

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



# Probem 5a ################################################################################

# rpart classification with Gini impurity coefficient splits
tree.gini5 <- rpart(formula = y ~ ., data=dtm.train, method="class", parms=list(split="gini"))

# make predicitons on the testing data
fit.gini5 <- predict(object=tree.gini5, newdata=dtm.test, type="class")

# assess model
pred.gini5 <- data.frame(trueValue=dtm.test$y, prediction=fit.gini5)
assess.gini5 <- data.frame(accuracy=sum(pred.gini5$trueValue==pred.gini5$prediction)/nrow(pred.gini5),
                           falseNegRate=nrow(filter(pred.gini5, trueValue==1 & prediction==0))/nrow(filter(pred.gini5, trueValue==1)),
                           falsePosRate=nrow(filter(pred.gini5, trueValue==0 & prediction==1))/nrow(filter(pred.gini5, trueValue==0)))

# plot the tree (determined from the training data)
plot(tree.gini5, margin=0.12)
text(tree.gini5, use.n=TRUE, all=TRUE, cex=0.85)
dev.copy(device=png, file="writeup/5a.png", height=450, width=500)
dev.off()


# Probem 5b ################################################################################

# rpart classification with Gini impurity coefficient splits
tree.info5 <- rpart(formula = y ~ ., data=dtm.train, method="class", parms=list(split="information"))

# make predicitons on the testing data
fit.info5 <- predict(object=tree.info5, newdata=dtm.test, type="class")

# assess model
pred.info5 <- data.frame(trueValue=dtm.test$y, prediction=fit.info5)
assess.info5 <- data.frame(accuracy=sum(pred.info5$trueValue==pred.info5$prediction)/nrow(pred.info5),
                           falseNegRate=nrow(filter(pred.info5, trueValue==1 & prediction==0))/nrow(filter(pred.info5, trueValue==1)),
                           falsePosRate=nrow(filter(pred.info5, trueValue==0 & prediction==1))/nrow(filter(pred.info5, trueValue==0)))

# plot the tree (determined from the training data)
plot(tree.info5, margin=0.12)
text(tree.info5, use.n=TRUE, all=TRUE, cex=0.85)
dev.copy(device=png, file="writeup/5b.png", height=450, width=500)
dev.off()





