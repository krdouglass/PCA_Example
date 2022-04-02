##################################################################
# The purpose of this R script is to demonstrate a Principal 
# Component Analysis (PCA). This was made mostly for my own reference 
# and a way to practice these functions, so it is far from perfect or comprehensive.  
# However, I will share it for others who may find it helpful.
#
# Script by: Kim Fake
#
#
###################################################################

# Make up some data

# let's make data regarding body size of 100 American robins
# including:
# head length = a measure from the back of the head to the tip of the beak
# tarsus length = the length of the tarsus bone in the leg
# wing chord = the length of the wing from the wrist join to the tip of the longest primary feather
bird <- c(1:100) # list of 100 American robins, these numbers ID individual American robins
set.seed(87) # set seed to make same randomly generated numbers each time the code is run
tarsus_length <- sort(round(runif(100, 31, 36), 1)) # length of the tarsus bone
head_length <- sort(round(runif(100, 46, 57), 1)) # length of their head
wing_chord <- sort(round(runif(100, 116, 136),0)) # length of the wing
data <- data.frame(bird,tarsus_length, head_length, wing_chord)

# lets say that we want to do a regression analysis with these measure included as predictors
# but they obviously are highly correlated which could cause multicollinearity issues with our model
# so we want to use a PCA to combine these measures into one variable that describes robin body size
# that generally describes the relative body size of each bird

# run pca
my_pca <- prcomp(~tarsus_length + head_length + wing_chord,
                 data = data,
                 center = TRUE,
                 scale. = TRUE)

# show the loadings of the PCA
print(my_pca)

# show us a summary of the PCA 
# including the proportion of variance for each PC
summary(my_pca)

# the proportion of variance for PC1 is 0.9943
# this means PC1 is capturing 99.43% of the variation in the data
# this is great because it is well above 80%
# so PC1 will make a great measure of relative body size of the American robins
# and can be used in future analyses as a measure of bird body size

# show a 2 dimensional plot with each measure
# on the x axis is PC1 and on the y axis is PC2
biplot(my_pca)

# as you can see all three measures are generally correlated
# which why we want to combine these measures into one variable 
# so American robins with a low PC1 value are small 
# and American robins with a PC1 value are big

# next lets make a data frame of the PC values
PCs = as.data.frame(my_pca$x)

# let's add back in the bird #'s that ID individuals
PCs$bird <- data$bird

# now we can merge the data frames 
# to add the PC values into our original data
data <- merge(data, PCs, by = "bird")
