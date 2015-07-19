#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 2, Problem 2
# 2015-07-23
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_2")


# load libraries
library(pixmap)
library(data.table)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list <- 1:38
view_list <- c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00')






#################
# Problem 2a
#################

# preallocate an empty list
pic_data <- vector("list", length(pic_list)*length(view_list))
# preallocate an empty list to store the pgm for debugging
pic_data_pgm <- vector("list", length(pic_list)*length(view_list))

#----- START YOUR CODE BLOCK HERE -----#

# list directories/files within directory
dir_list <- dir(path="datasets/CroppedYale/", all.files=FALSE)

# Read in each pgm file to the pic_data list
pos <- 1
for(i in 1:length(pic_list)){
  for(j in 1:length(view_list)){
    
    # construct file path
    filename <- sprintf("datasets/CroppedYale/%s/%s_%s.pgm",
                        dir_list[pic_list[i]], dir_list[pic_list[i]], view_list[j])
    
    # read in pgm file and assign to list
    pic_data[pos] <- read.pnm(file=filename)
    
    # increment list index
    pos <- pos + 1
  }
}
rm(i, j, pos) # clear index variables


# initialize an empty matrix for all photos
faces_matrix <- vector()

# Convert each pgm file to a row of data
faces_matrix <- lapply(pic_data,
                       function(img){
                         as.vector(t(getChannels(img)), mode="list")
                       }
)

# row bind each vector (i.e., each photo) into one matrix
faces_matrix <- do.call(rbind, faces_matrix)

# check size of matrix
dim(faces_matrix)

#----- END YOUR CODE BLOCK HERE -----#




#################
# Problem 2b
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2c
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2d
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2e
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2f
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#



#################
# End of Script
#################

