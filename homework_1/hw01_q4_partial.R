#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 1, Problem 4
# 2015-07-13
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################



#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_1")

# load libraries
library(pixmap)



#################
# Problem 4a
#################

# paste or type in the given code here
face_01 <- read.pnm(file = "datasets/CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")

# now plot the data
plot(face_01)

# give it a nice title
title('hw01_01a: the first face')

# save the result
filename <- 'writeup/4_01a.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# extract the class and size

#----- START YOUR CODE BLOCK HERE -----#

class(face_01)

dim(getChannels(face_01))

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 4b
#################

# make face_01 into a matrix with the given command
face_01_matrix <- getChannels(face_01)

# load a second face
face_02 <- read.pnm(file = "datasets/CroppedYale/yaleB02/yaleB02_P00A-005E+10.pgm")
face_02_matrix <- getChannels(face_02)

# combine two faces into a single data matrix and make that a pixmap
faces_matrix <- cbind(face_01_matrix, face_02_matrix)
faces <- pixmapGrey(faces_matrix)

# plot to verify
plot(faces)

# find min and max values 

#----- START YOUR CODE BLOCK HERE -----#

range(faces_matrix)

testFacesMatrix <- faces_matrix
testFacesMatrix[1:50,1:50] <- 0 # test what color a 0 value is
testFacesMatrix[50:100,50:100] <- 1 # test what color a 1 value is
plot(pixmapGrey(testFacesMatrix)) # 0=black, 1=white

rm(testFacesMatrix)

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 4c
#################

# get directory structure
dir_list_1 <- dir(path="datasets/CroppedYale/", all.files=FALSE)
dir_list_2 <-  dir(path="datasets/CroppedYale/", all.files=FALSE, recursive=TRUE)

# find lengths

#----- START YOUR CODE BLOCK HERE -----#

length(dir_list_1)

length(dir_list_2)

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 4d
#################

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list <- c(05, 11, 31)
view_list <- c('P00A-005E+10', 'P00A-005E-10', 'P00A-010E+00')

# preallocate an empty list
pic_data <- vector("list", length(pic_list)*length(view_list))
# initialize an empty matrix of faces data
faces_matrix <- vector()

#----- START YOUR CODE BLOCK HERE -----#

# Read in each pgm file to the pic_data list
pos <- 1
for(i in 1:length(pic_list)){
  for(j in 1:length(view_list)){
    
    # construct file path
    filename <- sprintf("datasets/CroppedYale/%s/%s_%s.pgm",
                        dir_list_1[pic_list[i]], dir_list_1[pic_list[i]], view_list[j])
    
    # read in pgm file and assign to list
    pic_data[pos] <- read.pnm(file=filename)
    
    # increment list index
    pos <- pos + 1
  }
}
rm(i, j, pos) # clear index variables

# Convert each pgm file to a matrix
faces_matrix <- lapply(pic_data,
                       function(img){
                         getChannels(img)
                       }
)

# Combine the matrices into one matrix
faces_matrix <- rbind(
  cbind(faces_matrix[[1]], faces_matrix[[2]],faces_matrix[[3]]),
  cbind(faces_matrix[[4]], faces_matrix[[5]],faces_matrix[[6]]),
  cbind(faces_matrix[[7]], faces_matrix[[8]],faces_matrix[[9]])
)

#----- END YOUR CODE BLOCK HERE -----#

# now faces_matrix has been built properly.  plot and save it.
faces <- pixmapGrey(faces_matrix)
plot(faces)
# give it a nice title
title('hw01_01d: 3x3 grid of faces')
# save the result
filename <- 'writeup/4_01d.png'
dev.copy(device=png, file=filename, height=600, width=600)
dev.off()

#################
# End of Script
#################


