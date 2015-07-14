#############################
# < Your Name Here >
# STAT S4240 
# Homework <HW Number> , Problem <Problem Number>
# < Homework Due Date >
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  
library(pixmap)

#################
# Problem 6a
#################

views_6a = c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00' )

# load the data and save it as a matrix with the name face_matrix_6a

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#

# Get the size of the matrix for use later
fm_6a_size = dim(face_matrix_6a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6a = floor(fm_6a_size[1]*4/5) # Number of training obs
ntest_6a = fm_6a_size[1]-ntrain_6a # Number of testing obs
set.seed(1) # Set pseudo-random numbers so everyone gets the same output
ind_train_6a = sample(1:fm_6a_size[1],ntrain_6a) # Training indices
ind_test_6a = c(1:fm_6a_size[1])[-ind_train_6a] # Testing indices

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 6b
#################

#----- START YOUR CODE BLOCK HERE -----#


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


