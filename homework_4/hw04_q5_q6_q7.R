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
library(glmnet)

# run hw03_q1.R to create the document term matrices
#source("../homework_3/hw03_q1.R", chdir = TRUE)
# or alternatively, just load the saved data (and remove unnecessary definitions)
load("~/Documents/data-mining/homework_3/hw03_q1_DATA.RData")
rm(hamilton.test, hamilton.train, madison.test, madison.train, mu,
   logp.hamilton.test, logp.hamilton.train, logp.madison.test, logp.madison.train,
   make.document.term.matrix, make.log.pvec, make.sorted.dictionary.df, preprocess.directory, read.directory)

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


# remove original dataframes
rm(dtm.hamilton.test, dtm.hamilton.train, dtm.madison.test, dtm.madison.train)


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




# Probem 6a ################################################################################

# create centered and scaled versions of the document term matrices
cdtm.train <- cbind(dtm.train[, 1], scale(dtm.train[, -1]))
colnames(cdtm.train)[1] <- "y"
cdtm.test <- cbind(dtm.test[, 1], scale(dtm.test[, -1]))
colnames(cdtm.test)[1] <- "y"

# redefine NaN as 0
cdtm.train[is.nan(cdtm.train)] <- 0
cdtm.test[is.nan(cdtm.test)] <- 0




# Probem 6b ################################################################################

# train the model
model.ridge6 <- cv.glmnet(x=cdtm.train[, -1], y=cdtm.train[, 1],
                          alpha=0, family="binomial")

# make predicitons on the testing data
fit.ridge6 <- predict(model.ridge6, newx=cdtm.test[, -1], s="lambda.min", type="class")

# assess model
pred.ridge6 <- data.frame(trueValue=cdtm.test[, 1], prediction=as.numeric(fit.ridge6))
assess.ridge6 <- data.frame(accuracy=sum(pred.ridge6$trueValue==pred.ridge6$prediction)/nrow(pred.ridge6),
                            falseNegRate=nrow(filter(pred.ridge6, trueValue==1 & prediction==0))/nrow(filter(pred.ridge6, trueValue==1)),
                            falsePosRate=nrow(filter(pred.ridge6, trueValue==0 & prediction==1))/nrow(filter(pred.ridge6, trueValue==0)))

# get most important words

coefs.ridge6 <- as.data.frame(as.matrix(coef(model.ridge6, s="lambda.min")))
names(coefs.ridge6) <- "coef"

words.ridge6 <- coefs.ridge6 %>%
  mutate(word=row.names(coefs.ridge6),
         absCoef=abs(coef)) %>%
  select(word, coef, absCoef) %>%
  filter(word!="(Intercept)") %>%
  arrange(-absCoef) %>%
  head(10)




# Probem 6c ################################################################################

# train the model
model.lasso6 <- cv.glmnet(x=cdtm.train[, -1], y=cdtm.train[, 1],
                          alpha=1, family="binomial")

# make predicitons on the testing data
fit.lasso6 <- predict(model.lasso6, newx=cdtm.test[, -1], s="lambda.min", type="class")

# assess model
pred.lasso6 <- data.frame(trueValue=cdtm.test[, 1], prediction=as.numeric(fit.lasso6))
assess.lasso6 <- data.frame(accuracy=sum(pred.lasso6$trueValue==pred.lasso6$prediction)/nrow(pred.lasso6),
                            falseNegRate=nrow(filter(pred.lasso6, trueValue==1 & prediction==0))/nrow(filter(pred.lasso6, trueValue==1)),
                            falsePosRate=nrow(filter(pred.lasso6, trueValue==0 & prediction==1))/nrow(filter(pred.lasso6, trueValue==0)))

# get most important words

coefs.lasso6 <- as.data.frame(as.matrix(coef(model.lasso6, s="lambda.min")))
names(coefs.lasso6) <- "coef"

words.lasso6 <- coefs.lasso6 %>%
  mutate(word=row.names(coefs.lasso6),
         absCoef=abs(coef)) %>%
  select(word, coef, absCoef) %>%
  filter(word!="(Intercept)") %>%
  arrange(-absCoef) %>%
  head(10)

# compare to the words in 6b
intersect(words.ridge6$word, words.lasso6$word)



