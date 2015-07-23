#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 2, Problem 6
# 2015-07-23
#
# The following code performs facial recognition with kNN.
#############################



#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_2")

# load libraries 
library(pixmap)
library(class)
library(dplyr)



#################
# Problem 6a
#################

views_6a <- c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00')

# load the data and save it as a matrix with the name face_matrix_6a

#----- START YOUR CODE BLOCK HERE -----#

# all subjects
pic_list <- 1:38

# initalize an empty list
pic_data <- vector("list", length(pic_list)*length(views_6a))

# initialize and empty dataframe to store subject and view labels
subjectViewLabels <- data.frame(subject=character(), view=character())

# list directories/files within directory
dir_list <- dir(path="datasets/CroppedYale/", all.files=FALSE)

# Read in each pgm file to the pic_data list
pos <- 1
for(i in 1:length(pic_list)){
  for(j in 1:length(views_6a)){
    
    # construct file path
    filename <- sprintf("datasets/CroppedYale/%s/%s_%s.pgm",
                        dir_list[pic_list[i]], dir_list[pic_list[i]], views_6a[j])
    
    # read in pgm file and assign to list
    pic_data[pos] <- read.pnm(file=filename)
    
    # record subject number and view label
    subjectViewLabels <- rbind(subjectViewLabels,
                               data.frame(subject=dir_list[pic_list[i]], view=views_6a[j]))
    
    
    # increment list index
    pos <- pos + 1
  }
}
rm(i, j, pos) # clear index variables

# subject view labels key
subjectViewLabels$key <- paste(subjectViewLabels$subject, subjectViewLabels$view, sep="/")
subjectViewLabels$subject <- as.character(subjectViewLabels$subject)
subjectViewLabels$view <- as.character(subjectViewLabels$view)


# initialize an empty matrix for all photos
face_matrix_6a <- vector()

# Convert each pgm file to a row of data
face_matrix_6a <- lapply(pic_data,
                         function(img){
                           as.vector(t(getChannels(img)), mode="any")
                         }
)

# row bind each vector (i.e., each photo) into one matrix
face_matrix_6a <- as.matrix(do.call(rbind, face_matrix_6a))

#----- END YOUR CODE BLOCK HERE -----#

# Get the size of the matrix for use later
fm_6a_size <- dim(face_matrix_6a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6a <- floor(fm_6a_size[1]*4/5) # Number of training obs
ntest_6a <- fm_6a_size[1]-ntrain_6a # Number of testing obs
set.seed(1) # Set pseudo-random numbers so everyone gets the same output
ind_train_6a <- sample(1:fm_6a_size[1],ntrain_6a) # Training indices
ind_test_6a <- c(1:fm_6a_size[1])[-ind_train_6a] # Testing indices

#----- START YOUR CODE BLOCK HERE -----#

# first 5 files in the training set
head(subjectViewLabels[(ind_train_6a), ], 5)

# first 5 files in the testing set
head(subjectViewLabels[(ind_test_6a), ], 5)



#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 6b
#################

#----- START YOUR CODE BLOCK HERE -----#

# define a mean face vector
mean_face <- apply(face_matrix_6a, 2, mean)

# column center faces_matrix
face_cent <- apply(face_matrix_6a, 2, function(col){col - mean(col)})

# pca on the centered data
pc_train <- prcomp(face_cent[ind_train_6a, ])


# use the first 25 PCs
loadings_train <- t(pc_train$rotation[, 1:25])

# calculate the scores
scores_train <- (face_cent[ind_train_6a, ] %*% t(loadings_train))
scores_test <- (face_cent[ind_test_6a, ] %*% t(loadings_train))

# run knn
estimates <- as.character(
  knn(train=scores_train,
      test=scores_test,
      cl=subjectViewLabels[ind_train_6a, "subject"],
      k=1)
)

estimatesTable <- data.frame(cbind(estimates, subjects=as.character(subjectViewLabels[ind_test_6a, "subject"])))
estimatesTable$correct <- estimatesTable$estimate==estimatesTable$subject

estimatesTable

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 6c
#################

# Use different lighting conditions

views_6c = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')

# load your data and save the images as face_matrix_6c

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#

fm_6c_size = dim(face_matrix_6c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6c = floor(fm_6c_size[1]*4/5)
ntest_6c = fm_6c_size[1]-ntrain_6c
set.seed(2) # Set pseudo-random numbers
# You are resetting so that if you have used a random number in between the last use of sample(), you will still get the same output
ind_train_6c = sample(1:fm_6c_size[1],ntrain_6c)
ind_test_6c = c(1:fm_6c_size[1])[-ind_train_6c]

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 6d
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


