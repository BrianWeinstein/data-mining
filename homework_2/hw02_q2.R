#############################
# Brian Weinstein - bmw2148
# STAT S4240 002
# Homework 2, Problem 2
# 2015-07-23
#
# The following code performs PCA on the eigenfaces data.
#############################

#################
# Setup
#################

# set working directory
setwd("~/Documents/data-mining/homework_2")

# load libraries
library(pixmap)
library(data.table)
library(ggplot2)

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
                         as.vector(t(getChannels(img)), mode="any")
                       }
)

# row bind each vector (i.e., each photo) into one matrix
faces_matrix <- as.matrix(do.call(rbind, faces_matrix))

# check size of matrix
dim(faces_matrix)

#----- END YOUR CODE BLOCK HERE -----#




#################
# Problem 2b
#################

#----- START YOUR CODE BLOCK HERE -----#

# mean face
mean_face <- apply(faces_matrix, 2, mean)
mean_face <- matrix(mean_face, 192, 168, byrow=TRUE)
plot(pixmapGrey(mean_face))
dev.copy(device=png, file='writeup/2b_mean_face.png', height=192, width=168)
dev.off()
# plot(pixmapGrey(matrix(apply(faces_matrix, 2, mean), 400, 400*(168/192), byrow=TRUE)))
# dev.copy(device=png, file='writeup/2b_mean_face_fixed.png', height=400, width=400*(168/192))
# dev.off()

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2c
#################

#----- START YOUR CODE BLOCK HERE -----#

# column center faces_matrix
faces_centered <- apply(faces_matrix, 2, function(col){col - mean(col)})
#faces_centered <- scale((faces_matrix), center=TRUE, scale=FALSE)

# pca on the centered data
pc <- prcomp(faces_centered)

# calculate pve
pc.pve <- data.frame(pve=((pc$sdev)^2)/sum((pc$sdev)^2))

# plot pve
ggplot(pc.pve, aes(x=1:nrow(pc.pve), y=pve)) +
  geom_line() + geom_point() +
  xlab("principal component #") + ylab("pve") + theme_bw()
ggsave(filename='writeup/2c.png', width=5, height=3)

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2d
#################

#----- START YOUR CODE BLOCK HERE -----#

# plot the first 9 PCs
par(mfrow=c(3,3))
for(i in 1:9){
  
  eigenfaces <- pc$rotation[, i]
  
  eigenfaces <- matrix(eigenfaces, 192, 168, byrow=TRUE)
  plot(pixmapGrey(eigenfaces), main = as.character(i))
  
}
dev.copy(device=png, file='writeup/2d_pca_faces.png', height=800, width=800*(168/192))
dev.off()
par(mfrow=c(1,1))

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2e
#################

#----- START YOUR CODE BLOCK HERE -----#

# read in photo
b05 <- read.pnm(file = "datasets/CroppedYale/yaleB05/yaleB05_P00A+010E+00.pgm")
plot(pixmapGrey(getChannels(b05)))

# convert photo to vector and center
b05 <- c(as.vector(t(getChannels(b05))) - as.vector(t(mean_face)))

# use the first n eigenfaces
eigenfaces <- t(pc$rotation[, 1:120])

# calculate the scores
scores <- (eigenfaces %*% b05)

# compute the face as a lin comb of the first n eigenfaces
b05_comps <- lapply(1:120,
                    function(dim){
                      if(dim==1){
                        (t(eigenfaces[1:1, ]) * scores[1:1,]) + as.vector(t(mean_face))
                      } else{
                        (t(eigenfaces[1:dim, ]) %*% scores[1:dim,]) + as.vector(t(mean_face))
                      }
                    }
)

# plot the face using a lin comb of 5 eigenfaces
plot(pixmapGrey(t(matrix(b05_comps[[5]], 168, 192))))

# generate a plot of the first 24 face constructions
par(mfrow=c(5,5))
for(dim in 1:24){
  plot(pixmapGrey(t(matrix(b05_comps[[dim]], 168, 192))),
       main = as.character(dim))
}
plot.new() # to fill in the 25th slot
dev.copy(device=png, file='writeup/2e_faces24.png', height=1000, width=1000*(168/192))
dev.off()
par(mfrow=c(1,1))

# generate a plot of 120 face constructions in steps of 5
par(mfrow=c(5,5))
for(dim in c(1, seq(from=5, to=120, by=5))){
  plot(pixmapGrey(t(matrix(b05_comps[[dim]], 168, 192))),
       main = as.character(dim))
}
dev.copy(device=png, file='writeup/2e_faces120.png', height=1000, width=1000*(168/192))
dev.off()
par(mfrow=c(1,1))

#----- END YOUR CODE BLOCK HERE -----#



#################
# Problem 2f
#################

#----- START YOUR CODE BLOCK HERE -----#

# remove photos of subject 1 from the matrix
faces_matrix_wo1 <- faces_matrix[-(1:4), ]

# recenter the data
mean_face_wo1 <- apply(faces_matrix_wo1, 2, mean)
mean_face_wo1 <- matrix(mean_face_wo1, 192, 168, byrow=TRUE)
faces_centered_wo1 <- apply(faces_matrix_wo1, 2, function(col){col - mean(col)})

# pca on the centered data
pc_wo1 <- prcomp(faces_centered_wo1)






# read in photo
b01 <- read.pnm(file = "datasets/CroppedYale/yaleB01/yaleB01_P00A+010E+00.pgm")
b01orig <- b01 # for plotting later
plot(pixmapGrey(getChannels(b01)))

# convert photo to vector and center
b01 <- c(as.vector(t(getChannels(b01))) - as.vector(t(mean_face_wo1)))

# use the all eigenfaces
eigenfaces_wo1 <- t(pc_wo1$rotation)

# calculate the scores
scores_wo1 <- (eigenfaces_wo1 %*% b01)

# compute the face as a lin comb of all of the eigenfaces
b01_comps <- (t(eigenfaces_wo1) %*% scores_wo1) + as.vector(t(mean_face_wo1))

# plot the face using a lin comb of all of the eigenfaces
par(mfrow=c(1,2))
plot(pixmapGrey(getChannels(b01orig)))
plot(pixmapGrey(t(matrix(b01_comps, 168, 192))))
dev.copy(device=png, file='writeup/2f.png', height=500, width=500*(168/192))
dev.off()
par(mfrow=c(1,1))











#----- END YOUR CODE BLOCK HERE -----#



#################
# End of Script
#################

