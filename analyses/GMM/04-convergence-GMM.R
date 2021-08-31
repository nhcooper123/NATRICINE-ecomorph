# Phylogenetic convergence testing for 2D data
# Stayton's distance-based metric
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
tree2D <- read.nexus("data/GMM/new_datedtree.nexus")

# Extract from the tree only those species which match with the data
sps <- name.check(tree2D, snake2D, data.names = snake2D$Species)
tree2D <- drop.tip(tree2D, sps$tree_not_data)

# Look at the data
glimpse(snake2D)
str(tree2D)

#----------------------------------------------
# Stayton's distance-based convergence metrics
#----------------------------------------------
# Create data object for convevol functions
# This selects just the eight PCs and make sure the dataframe is correct
pc.data <- as.matrix(array(snake2D[, 2:9]))
rownames(pc.data) <- snake2D$Species

#--------------------------------------------------------------------------------
# Calculate CONVRAT for each habit type with simulations and save them to outputs
#### names provided for each categorical runs Burr AqB Ter Aq SAq
#--------------------------------------------------------------------------------
# Burrowing vs the rest
#-----------------------
# Select tips that are burrowing natricines
burrowers <- filter(snake2D, Habit == "Burrowing")
burrowers <- pull(burrowers, Species)

# Run analyses and 
ans <- convrat(phyl = tree2D,
               phendata = pc.data,
               convtips = burrowers)

# Look at output
ans

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputBU <- data.frame(array(dim = c(4, 3)))
names(outputBU) <- c("group", "obsC", "PVals")
rownames(outputBU) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
# Get significance values
x <- convratsig(phyl = tree2D,
                phendata = pc.data,
                convtips = burrowers,
                nsim = 100)
outputBU["group"] <- "burrowers"
outputBU["obsC"] <- x$ObservedCs
outputBU["PVals"] <- x$Pvals

# Write to file
write_csv(outputBU, file = "outputs/GMM/Tables/convevol-results-burrower-GMM.csv")

#--------------------------------------------------------------------------------
# Aquatic burrowing vs the rest
#-------------------------------
# Select tips that are aquatic burrowing natricines
Aquaburrowers <- filter(snake2D, Habit == "Aquatic burrower")
Aquaburrowers <- pull(Aquaburrowers, Species)

# Run analyses and 
ans <- convrat(phyl = tree2D,
               phendata = pc.data,
               convtips = Aquaburrowers)

# Look at output
ans

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputABU <- data.frame(array(dim = c(4, 3)))
names(outputABU) <- c("group", "obsC", "PVals")
rownames(outputABU) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
# Get significance values
x <- convratsig(phyl = tree2D,
                phendata = pc.data,
                convtips = Aquaburrowers,
                nsim = 100)
outputABU["group"] <- "Aquaburrowers"
outputABU["obsC"] <- x$ObservedCs
outputABU["PVals"] <- x$Pvals

outputABU

write_csv(outputABU, file = "outputs/GMM/Tables/convevol-results-aquaburrower-GMM.csv")

#--------------------------------------------------------------------------------
# Terrestrial vs the rest
#-------------------------------
# Select tips that are Terrestrial natricines
Terrestrial <- filter(snake2D, Habit == "Terrestrial")
Terrestrial <- pull(Terrestrial, Species)

# Run analyses and 
ans <- convrat(phyl = tree2D,
               phendata = pc.data,
               convtips = Terrestrial)

# Look at output
ans

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputTE <- data.frame(array(dim = c(4, 3)))
names(outputTE) <- c("group", "obsC", "PVals")
rownames(outputTE) <- c("C1", "C2", "C3", "C4")


### This takes a long time (days) #########
# Get significance values
# Takes a while to run
# Run CONVRAT and save outputs in the outputs folder
x <- convratsig(phyl = tree2D,
                phendata = pc.data,
                convtips = Terrestrial,
                nsim = 100)
outputTE["group"] <- "Terrestrial"
outputTE["obsC"] <- x$ObservedCs
outputTE["PVals"] <- x$Pvals

# Write to file
write_csv(outputTE, file = "outputs/GMM/Tables/convevol-results-terrestrial-GMM.csv")

#--------------------------------------------------------------------------------
# Aquatic vs the rest
#-------------------------------
# Select tips that are aquatic natricines
Aquatic <- filter(snake2D, Habit == "Aquatic")
Aquatic <- pull(Aquatic, Species)

# Run analyses and 
ans <- convrat(phyl = tree2D,
               phendata = pc.data,
               convtips = Aquatic)

# Look at output
ans

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputAQ <- data.frame(array(dim = c(4, 3)))
names(outputAQ) <- c("group", "obsC", "PVals")
rownames(outputAQ) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree2D,
                phendata = pc.data,
                convtips = Aquatic,
                nsim = 100)
outputAQ["group"] <- "Aquatic"
outputAQ["obsC"] <- x$ObservedCs
outputAQ["PVals"] <- x$Pvals

# Write to file
write_csv(outputAQ, file = "outputs/GMM/Tables/convevol-results-aquatic-GMM.csv")

#--------------------------------------------------------------------------------
# Semi Aquatic vs the rest
#-------------------------------
# Select tips that are semi aquatic natricines
semiaquatic <-filter(snake2D, Habit == "Semiaquatic")
semiaquatic <-pull(semiaquatic, Species)

# Run analyses and 
ans <- convrat(phyl = tree2D,
               phendata = pc.data,
               convtips = semiaquatic)

# Look at output
ans

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputSAQ <- data.frame(array(dim = c(4, 3)))
names(outputSAQ) <- c("group", "obsC", "PVals")
rownames(outputSAQ) <- c("C1", "C2", "C3", "C4")


### This takes a long time (days) #########
x <- convratsig(phyl = tree2D,
                phendata = pc.data,
                convtips = semiaquatic,
                nsim = 100)
outputSAQ["group"] <- "semiaquatic"
outputSAQ["obsC"] <- x$ObservedCs
outputSAQ["PVals"] <- x$Pvals

# Write to file
write_csv(outputSAQ, file = "outputs/GMM/Tables/convevol-results-semiaquatic-GMM.csv")
