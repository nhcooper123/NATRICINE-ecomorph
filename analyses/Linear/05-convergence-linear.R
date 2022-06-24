# Phylogenetic convergence testing for linear data
# Stayton's distance-based metric
# Modified 2022
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

#----------------------------------------------
# Stayton's distance-based convergence metrics
#----------------------------------------------
# Create data object for convevol functions
# This selects just the ten PCs and make sure the data is in matrix format
pc.data <- as.matrix(snake[, 21:30])
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
ans_BU <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = burrowers)

# Look at output
ans_BU

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
write_csv(outputBU, file = "outputs/Linear/Tables/convevol-results-burrower-LSR.csv") 

#--------------------------------------------------------------------------------
# Aquatic burrowing vs the rest
#-------------------------------
# Select tips that are aquatic burrowing natricines
Aquaburrowers <- filter(snake, Habit == "Aquatic burrower")
Aquaburrowers <- pull(Aquaburrowers, Species)

# Run analyses and 
ans_ABU <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = Aquaburrowers)

# Look at output
ans_ABU

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

write_csv(outputABU, file = "outputs/Linear/Tables/convevol-results-aquaburrower-LSR.csv") 

#--------------------------------------------------------------------------------
# Terrestrial vs the rest
#-------------------------------
# Select tips that are Terrestrial natricines
Terrestrial <- filter(snake, Habit == "Terrestrial")
Terrestrial <- pull(Terrestrial, Species)

# Run analyses and 
ans_TE <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = Terrestrial)

# Look at output
ans_TE

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputTE <- data.frame(array(dim = c(4, 3)))
names(outputTE) <- c("group", "obsC", "PVals")
rownames(outputTE) <- c("C1", "C2", "C3", "C4")

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
write_csv(outputTE, file = "outputs/Linear/Tables/convevol-results-terrestrial-LSR.csv") 

#--------------------------------------------------------------------------------
# Aquatic vs the rest
#-------------------------------
# Select tips that are aquatic natricines
Aquatic <- filter(snake, Habit == "Aquatic")
Aquatic <- pull(Aquatic, Species)

# Run analyses and 
ans_AQ <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = Aquatic)

# Look at output
ans_AQ

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputAQ <- data.frame(array(dim = c(4, 3)))
names(outputAQ) <- c("group", "obsC", "PVals")
rownames(outputAQ) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = Aquatic,
                nsim = 100)
outputAQ["group"] <- "Aquatic"
outputAQ["obsC"] <- x$ObservedCs
outputAQ["PVals"] <- x$Pvals

# Write to file
write_csv(outputAQ, file = "outputs/Linear/Tables/convevol-results-aquatic-LSR.csv")

#--------------------------------------------------------------------------------
# Semi Aquatic vs the rest
#-------------------------------
# Select tips that are semi aquatic natricines
semiaquatic <-filter(snake, Habit == "Semiaquatic")
semiaquatic <-pull(semiaquatic, Species)

# Run analyses and 
ans_SAQ <- convrat(phyl = tree,
               phendata = pc.data,
               convtips = semiaquatic)

# Look at output
ans_SAQ

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputSAQ <- data.frame(array(dim = c(4, 3)))
names(outputSAQ) <- c("group", "obsC", "PVals")
rownames(outputSAQ) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = semiaquatic,
                nsim = 100)
outputSAQ["group"] <- "semiaquatic"
outputSAQ["obsC"] <- x$ObservedCs
outputSAQ["PVals"] <- x$Pvals

# Write to file
write_csv(outputSAQ, file = "outputs/Linear/Tables/convevol-results-semiaquatic-LSR.csv")

# -------------------------------
# Diet
#--------------------------------------------------------------------------------
# Annelids vs the rest
#-------------------------------
# Select tips that are annelid eating natricines
annelid <-filter(snake, Diet == "annelids")
annelid <-pull(annelid, Species)

# Run analyses and 
ans_annelid <- convrat(phyl = tree,
                   phendata = pc.data,
                   convtips = annelid)

# Look at output
ans_annelid

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputannelid <- data.frame(array(dim = c(4, 3)))
names(outputannelid) <- c("group", "obsC", "PVals")
rownames(outputannelid) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = annelid,
                nsim = 100)
outputannelid["group"] <- "annelid"
outputannelid["obsC"] <- x$ObservedCs
outputannelid["PVals"] <- x$Pvals

# Write to file
write_csv(outputannelid, file = "outputs/Linear/Tables/convevol-results-annelid-LSR.csv")

#--------------------------------------------------------------------------------
# Molluscs vs the rest
#-------------------------------
# Select tips that are mollusc eating natricines
mollusc <-filter(snake, Diet == "molluscs")
mollusc <-pull(mollusc, Species)

# Run analyses and 
ans_mollusc <- convrat(phyl = tree,
                       phendata = pc.data,
                       convtips = mollusc)

# Look at output
ans_mollusc

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputmollusc <- data.frame(array(dim = c(4, 3)))
names(outputmollusc) <- c("group", "obsC", "PVals")
rownames(outputmollusc) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = mollusc,
                nsim = 100)
outputmollusc["group"] <- "mollusc"
outputmollusc["obsC"] <- x$ObservedCs
outputmollusc["PVals"] <- x$Pvals

# Write to file
write_csv(outputmollusc, file = "outputs/Linear/Tables/convevol-results-mollusc-LSR.csv")

#--------------------------------------------------------------------------------
# Anurans vs the rest
#-------------------------------
# Select tips that are anuran eating natricines
anurans <-filter(snake, Diet == "anurans")
anurans <-pull(anurans, Species)

# Run analyses and 
ans_anurans <- convrat(phyl = tree,
                       phendata = pc.data,
                       convtips = anurans)

# Look at output
ans_anurans

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputanurans <- data.frame(array(dim = c(4, 3)))
names(outputanurans) <- c("group", "obsC", "PVals")
rownames(outputanurans) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = anurans,
                nsim = 100)
outputanurans["group"] <- "anurans"
outputanurans["obsC"] <- x$ObservedCs
outputanurans["PVals"] <- x$Pvals

# Write to file
write_csv(outputanurans, file = "outputs/Linear/Tables/convevol-results-anurans-LSR.csv")

#--------------------------------------------------------------------------------
# Aquatic generalist vs the rest
#-------------------------------
# Select tips that are aquatic generalist natricines
AG <-filter(snake, Diet == "aquatic generalist")
AG <-pull(AG, Species)

# Run analyses and 
ans_AG <- convrat(phyl = tree,
                       phendata = pc.data,
                       convtips = AG)

# Look at output
ans_AG

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputAG <- data.frame(array(dim = c(4, 3)))
names(outputAG) <- c("group", "obsC", "PVals")
rownames(outputAG) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = AG,
                nsim = 100)
outputAG["group"] <- "aquatic generalist"
outputAG["obsC"] <- x$ObservedCs
outputAG["PVals"] <- x$Pvals

# Write to file
write_csv(outputAG, file = "outputs/Linear/Tables/convevol-results-AG-LSR.csv")

#--------------------------------------------------------------------------------
# Generalist vs the rest
#-------------------------------
# Select tips that are generalist natricines
G <-filter(snake, Diet == "generalist")
G <-pull(G, Species)

# Run analyses and 
ans_G <- convrat(phyl = tree,
                  phendata = pc.data,
                  convtips = G)

# Look at output
ans_G

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputG <- data.frame(array(dim = c(4, 3)))
names(outputG) <- c("group", "obsC", "PVals")
rownames(outputG) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = G,
                nsim = 100)
outputG["group"] <- "aquatic generalist"
outputG["obsC"] <- x$ObservedCs
outputG["PVals"] <- x$Pvals

# Write to file
write_csv(outputG, file = "outputs/Linear/Tables/convevol-results-G-LSR.csv")

#--------------------------------------------------------------------------------
# fish vs the rest
#-------------------------------
# Select tips that are fish eating natricines
fish <-filter(snake, Diet == "fish")
fish <-pull(fish, Species)

# Run analyses and 
ans_fish <- convrat(phyl = tree,
                 phendata = pc.data,
                 convtips = fish)

# Look at output
ans_fish

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputfish <- data.frame(array(dim = c(4, 3)))
names(outputfish) <- c("group", "obsC", "PVals")
rownames(outputfish) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = fish,
                nsim = 100)
outputfish["group"] <- "fish"
outputfish["obsC"] <- x$ObservedCs
outputfish["PVals"] <- x$Pvals

# Write to file
write_csv(outputfish, file = "outputs/Linear/Tables/convevol-results-fish-LSR.csv")

#--------------------------------------------------------------------------------
# lizards vs the rest
#-------------------------------
# Select tips that are lizard eating natricines
lizards <-filter(snake, Diet == "lizards")
lizards <-pull(lizards, Species)

# Run analyses and 
ans_lizards <- convrat(phyl = tree,
                    phendata = pc.data,
                    convtips = lizards)

# Look at output
ans_lizards

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputlizards <- data.frame(array(dim = c(4, 3)))
names(outputlizards) <- c("group", "obsC", "PVals")
rownames(outputlizards) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = lizards,
                nsim = 100)
outputfish["group"] <- "lizards"
outputfish["obsC"] <- x$ObservedCs
outputfish["PVals"] <- x$Pvals

# Write to file
write_csv(outputlizards, file = "outputs/Linear/Tables/convevol-results-lizards-LSR.csv")

#--------------------------------------------------------------------------------
# crayfish vs the rest
#-------------------------------
# Select tips that are crayfish eating natricines
crayfish<-filter(snake, Diet == "crayfish")
crayfish <-pull(crayfish, Species)

# Run analyses and 
ans_crayfish <- convrat(phyl = tree,
                       phendata = pc.data,
                       convtips = crayfish)

# Look at output
ans_crayfish

# Create an output file. There are four 'C' outputs C1-C4 and columns below
outputcrayfish <- data.frame(array(dim = c(4, 3)))
names(outputcrayfish) <- c("group", "obsC", "PVals")
rownames(outputcrayfish) <- c("C1", "C2", "C3", "C4")

### This takes a long time (days) #########
x <- convratsig(phyl = tree,
                phendata = pc.data,
                convtips = crayfish,
                nsim = 100)
outputfish["group"] <- "crayfish"
outputfish["obsC"] <- x$ObservedCs
outputfish["PVals"] <- x$Pvals

# Write to file
write_csv(outputcrayfish, file = "outputs/Linear/Tables/convevol-results-crayfish-LSR.csv")          
