#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 2, Problem 1
# 2015-07-23
#
# Manually going through the steps for PCA.
#############################



#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_2")

# load libraries
library(ggplot2)


#################
# Problem 1a
#################

# read in hw02_q1_p1.csv
rawData <- read.csv(file="datasets/hw02_q1_p1.csv")

# column means
rawData.colmeans <- apply(rawData, 2, mean)

# row means
rawData.rowmeans <- apply(rawData, 1, mean)



#################
# Problem 1b
#################

# center the data
data <- as.matrix(apply(rawData, 2, function(col) col - mean(col)))

# standardize the data
#data <- apply(data, 2, function(col) col/sd(col))

# empirical covariance matrix
sig <- (1/nrow(data))*t(data)%*%data



#################
# Problem 1c
#################

# eigenvalues and eigenvectors
evals <- eigen(sig)$values
evecs <- eigen(sig)$vectors



#################
# Problem 1d
#################

# loadings
evecs

# scores
data%*%t(evecs)



#################
# Problem 1e
#################

pve <- data.frame(var=evals/sum(evals))

ggplot(pve, aes(x=1:ncol(sig), y=var)) +
  geom_line() + geom_point() +
  xlab("principal component #") + ylab("pve") + theme_bw()
ggsave(filename='writeup/1e.png', width=5, height=3)



#################
# Problem 1f
#################

# read in hw02_q1_p2.csv
rawData2 <- as.matrix(read.csv(file="datasets/hw02_q1_p2.csv"))

# column means from part 1a
rawData.colmeans 

# center the data
data2 <- t(t(rawData2)-rawData.colmeans)

# empirical covariance matrix
sig2 <- (1/nrow(data2))*t(data2)%*%data2

# eigenvalues and eigenvectors
evals2 <- eigen(sig2)$values
evecs2 <- eigen(sig2)$vectors

# loadings
evecs2

# scores
scores2 <- data2%*%t(evecs2)


#################
# Problem 1g
#################

# use only the first 2 scores
scores2_2d <- scores2
scores2_2d[, 3:5] <- 0

# coordinates of the projections in the original space, x'
projInOrigSpace <- scores2_2d%*%evecs2
projInOrigSpace <- t(t(projInOrigSpace)+rawData.colmeans) # uncenter the data


# define a distance function
euc.dist <- function(vec1, vec2){
  sqrt(sum((vec1-vec2)^2))
}

# euclidean distance from the original data points
for(obs in 1:nrow(data2)){
  dist <- euc.dist(projInOrigSpace[obs, ], rawData2[obs, ])
  print(dist)
}



#################
# Problem 1h
#################

(projInOrigSpace-rawData2)

plot((projInOrigSpace-rawData2)[,1],(projInOrigSpace-rawData2)[,2])
arrows(0,0, 20*evecs2[1,1],20*evecs2[2,1],lwd=3,col="red")

