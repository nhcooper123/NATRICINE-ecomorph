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
# species means PCA outputs
snake <- read_csv("data/Linear/snakepca-LSR.csv")

# Read in the tree
tree <- read.nexus("data/Linear/new_datedtree-LM.nexus")

# Extract from the tree only those species which match with the data
sps <- name.check(tree, snake, data.names = snake$Species)
tree <- drop.tip(tree, sps$tree_not_data)

# Extract from the data species not in the tree
matches <- match(snake$Species, sps$data_not_tree, nomatch = 0)
snake <- snake[which(matches == 0), ]

# Look at the data
glimpse(snake)
str(tree)

# Create data object for convevol functions
# This selects just the ten PCs and make sure the data is in matrix format
pc.data <- as.matrix(snake[, 21:30])
rownames(pc.data) <- snake$Species

#----------------------------------------------
# Read in the putatively convergent pairs data
#----------------------------------------------
pairs <- read_csv("data/Linear/putatively-convergent-pairs.csv")

#----------------------------------------------
# Stayton's distance-based convergence metrics
#----------------------------------------------
# Create an output file. 
# There are four 'C' outputs C1-C4 and p values for each
# Need one row for each pair
output <- data.frame(array(dim = c(length(pairs$Taxa1), 11)))
names(output) <- c("Habit", "Taxa1", "Taxa2", "C1", "C2", "C3", "C4", "pC1", "pC2", "pC3", "pC4")

#------------------------------------------------------------------------------------------
# Calculate CONVRAT for each pair per habit type with simulations and save them to outputs
#------------------------------------------------------------------------------------------

# loop through each pair
for(i in 1:length(pairs$Taxa1)){

#-------------------------------------------
# Select tips that are putatively convergent
focal <- c(pairs$Taxa1[i], pairs$Taxa2[i])

# Run analyses
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = focal,
                nsim = 100)
output[i, "Habit"] <- pairs$Habit[i]
output[i, "Taxa1"] <- pairs$Taxa1[i]
output[i, "Taxa2"] <- pairs$Taxa2[i]
output[i, "C1"] <- x$ObservedCs[1]
output[i, "pC1"] <- x$Pvals[1]
output[i, "C2"] <- x$ObservedCs[2]
output[i, "pC2"] <- x$Pvals[2]
output[i, "C3"] <- x$ObservedCs[3]
output[i, "pC3"] <- x$Pvals[3]
output[i, "C4"] <- x$ObservedCs[4]
output[i, "pC4"] <- x$Pvals[4]

# Write to file
write_csv(output, file = "outputs/Linear/Tables/convevol-results-pairs.csv", append = TRUE) 

}
