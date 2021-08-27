# Phylogenetic convergence testing for linear data
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
snake <- read_csv("data/Linear/snakepca.csv")

# Read in the tree
tree <- read.nexus("data/Linear/new_datedtree-LM.nexus")

# Extract from the tree only those species which match with the data
sps <- name.check(tree, snake, data.names = snake$Species)
tree <- drop.tip(tree, sps$tree_not_data)

# Look at the data
glimpse(snake)
str(tree)

#----------------------------------------------
# Stayton's distance-based convergence metrics
#----------------------------------------------
# Create data object for convevol functions
# This selects just the seven PCs and make sure the dataframe is correct
pc.data <- as.matrix(array(snake[, 27:33]))
rownames(pc.data) <- snake$Species

#--------------------------------------------------------------------------------
# Calculate CONVRAT for each habit type with simulations and save them to outputs
#### names provided for each categorical runs Burr AqB Ter Aq SAq
#--------------------------------------------------------------------------------
# Burrowing vs the rest
#-----------------------
# Select tips that are burrowing natricines
burrowers <- filter(snake, Habit == "Burrowing")
burrowers <- pull(burrowers, Species)

# Run analyses and 
ans <- convrat(phyl = tree,
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
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = burrowers,
                nsim = 100)
outputBU["group"] <- "burrowers"
outputBU["obsC"] <- x$ObservedCs
outputBU["PVals"] <- x$Pvals

# Write to file
write_csv(outputBU, path = "outputs/Linear/Tables/convevol-results-burrower.csv") 

#--------------------------------------------------------------------------------
# Aquatic burrowing vs the rest
#-------------------------------
# Select tips that are aquatic burrowing natricines
Aquaburrowers <- filter(snake, Habit == "Aquatic burrower")
Aquaburrowers <- pull(Aquaburrowers, Species)

# Run analyses and 
ans <- convrat(phyl = tree,
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
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = Aquaburrowers,
                nsim = 100)
outputABU["group"] <- "Aquaburrowers"
outputABU["obsC"] <- x$ObservedCs
outputABU["PVals"] <- x$Pvals

outputABU

write_csv(outputABU, path = "outputs/Linear/Tables/convevol-results-aquaburrower.csv") 

#--------------------------------------------------------------------------------
# Terrestrial vs the rest
#-------------------------------
# Select tips that are Terrestrial natricines
Terrestrial <- filter(snake, Habit == "Terrestrial")
Terrestrial <- pull(Terrestrial, Species)

# Run analyses and 
ans <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = Terrestrial)

# Look at output
ans

### This takes a long time (days) #########
# Get significance values
# Takes a while to run
# Run CONVRAT and save outputs in the outputs folder
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = Terrestrial,
                nsim = 100)
outputTE["group"] <- "Terrestrial"
outputTE["obsC"] <- x$ObservedCs
outputTE["PVals"] <- x$Pvals

# Write to file
write_csv(outputTE, path = "outputs/Linear/Tables/convevol-results-terrestrial.csv") 

#--------------------------------------------------------------------------------
# Aquatic vs the rest
#-------------------------------
# Select tips that are aquatic natricines
Aquatic <- filter(snake, Habit == "Aquatic")
Aquatic <- pull(Aquatic, Species)

# Run analyses and 
ans <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = Aquatic)

# Look at output
ans

# Look at output
#pval
### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = Aquatic,
                nsim = 100)
outputAQ["group"] <- "Aquatic"
outputAQ["obsC"] <- x$ObservedCs
outputAQ["PVals"] <- x$Pvals

# Write to file
write_csv(outputAQ, path = "outputs/Linear/Tables/convevol-results-aquatic.csv")

#--------------------------------------------------------------------------------
# Semi Aquatic vs the rest
#-------------------------------
# Select tips that are semi aquatic natricines
semiaquatic <-filter(snake, Habit == "Semiaquatic")
semiaquatic <-pull(semiaquatic, Species)

# Run analyses and 
ans <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = semiaquatic)

# Look at output
ans

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = semiaquatic,
                nsim = 100)
outputSAQ["group"] <- "semiaquatic"
outputSAQ["obsC"] <- x$ObservedCs
outputSAQ["PVals"] <- x$Pvals

# Write to file
write_csv(outputSAQ, path = "outputs/Linear/Tables/convevol-results-semiaquatic.csv")
