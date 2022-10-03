# Phylogenetic convergence testing for linear data
# Stayton's distance-based metric
# Looping over all putative pairs of convergent taxa
# 2022
#-------------------------------------
# Load libraries
library(ape)
library(geomorph)
library(geiger)
library(phytools)
library(convevol)
library(tidyverse)

#------------------------------------------------------------
# Import data species means outputs and tree file
#------------------------------------------------------------
# species means PCA outputs matched and sorted with tips on the tree
snake2D <- read_csv("data/GMM/snake-data-pca-GMM.csv")

# Read in the tree
tree2D <- read.nexus("data/GMM/new_datedtree-GMM.nexus")

# Extract from the tree only those species which match with the data
sps <- name.check(tree2D, snake2D, data.names = snake2D$Species)
tree2D <- drop.tip(tree2D, sps$tree_not_data)

# Extract from the data species not in the tree
matches <- match(snake2D$Species, sps$data_not_tree, nomatch = 0)
snake2D <- snake2D[which(matches == 0), ]

# Look at the data
glimpse(snake2D)
str(tree2D)

# Create data object for convevol functions
# This selects just the eight PCs and make sure the dataframe is correct
pc.data <- as.matrix(snake2D[, 2:9])
rownames(pc.data) <- snake2D$Species

#----------------------------------------------
# Read in the putatively convergent pairs data
#----------------------------------------------
pairs <- read_csv("data/Linear/putatively-convergent-pairs.csv")

# exclude missing taxa
matches2 <- match(pairs$Taxa1, snake2D$Species, nomatch = 0)
pairs2 <- pairs[which(matches2 != 0), ]
matches3 <- match(pairs2$Taxa2, snake2D$Species, nomatch = 0)
pairs_gmm <- pairs2[which(matches3 != 0), ]

#----------------------------------------------
# Stayton's distance-based convergence metrics
#----------------------------------------------
# Create an output file. 
# There are four 'C' outputs C1-C4 and p values for each
# Need one row for each pair
output <- data.frame(array(dim = c(length(pairs_gmm$Taxa1), 11)))
names(output) <- c("Habit", "Taxa1", "Taxa2", "C1", "C2", "C3", "C4", "pC1", "pC2", "pC3", "pC4")

#------------------------------------------------------------------------------------------
# Calculate CONVRAT for each pair per habit type with simulations and save them to outputs
#------------------------------------------------------------------------------------------

# loop through each pair
for(i in 1:length(pairs_gmm$Taxa1)){

#-------------------------------------------
# Select tips that are putatively convergent
focal <- c(pairs_gmm$Taxa1[i], pairs_gmm$Taxa2[i])

# Run analyses
x <- convratsig(phyl = tree2D,
                phendata = pc.data,
                convtips = focal,
                nsim = 100)
output[i, "Habit"] <- pairs_gmm$Habit[i]
output[i, "Taxa1"] <- pairs_gmm$Taxa1[i]
output[i, "Taxa2"] <- pairs_gmm$Taxa2[i]
output[i, "C1"] <- x$ObservedCs[1]
output[i, "pC1"] <- x$Pvals[1]
output[i, "C2"] <- x$ObservedCs[2]
output[i, "pC2"] <- x$Pvals[2]
output[i, "C3"] <- x$ObservedCs[3]
output[i, "pC3"] <- x$Pvals[3]
output[i, "C4"] <- x$ObservedCs[4]
output[i, "pC4"] <- x$Pvals[4]

# Write to file
write_csv(output, file = "outputs/GMM/Tables/convevol-results-pairs-GMM.csv") 

}
