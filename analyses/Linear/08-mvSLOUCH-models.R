# Fast mvSLOUCH code
# 2022

# See vignette here for more info:
# https://cran.r-project.org/web/packages/mvSLOUCH/vignettes/mvSLOUCH_Carnivorans.html
#------------------------
# Load libraries
library(foreach)
library(doParallel)
library(mvSLOUCH)
library(PCMBaseCpp)
library(ape)
library(rlist)
library(phytools)
library(tidyverse)
library(geiger)
#-----------------------------------------------------------------------
## Prepare the tree and the data
#-----------------------------------------------------------------------
# Prepare the tree and the data
# Read in data
ds <- read_csv("data/Linear/snakepca-LSR.csv")

# Make Habit into a factor
ds <- 
  ds %>%
  mutate(Habit = as.factor(Habit))

# Remove habits that are unknown
ds <- 
  ds %>%
  filter(Habit != "Unknown") %>%
  rename(habit = Habit)

# Look at the data
#head(ds)

# Read in the tree and tidy the species names. 
# Force it to be ultrametric and fully bifurcating.
tree <- read.nexus("data/Linear/new_datedtree-LM.nexus")

# Make ultrametric
tree <- force.ultrametric(tree)

# Check it has no polytomies
# is.binary(tree)

# Look at the tree summary
# str(tree)

#Remove species from the tree that are not in the data and vice versa.

# Match up species names using name.check
# Remember to check the outputs here to make sure there are no
# typos or taxonomic errors causing us to drop things unnecessarily
xx <- name.check(tree, ds, data.names = ds$Species)
# xx$tree_not_data
# xx$data_not_tree

# Drop extra species from the tree
mytree <- drop.tip(tree, xx$tree_not_data)

# Drop extra species from the data
mydata <- filter(ds, !Species %in% xx$data_not_tree)

# Check everything looks correct and numbers of species are matching
# glimpse(ds2)
# str(tree2)

# Subset data so it only contains the PCs and habit
pcdata <- mydata[, c(2, 21:35)]

# Add rownames as species names
rownames(pcdata) <- mydata$Species

#-----------------------------------------------------------------------
# Scale tree to height 1
#-----------------------------------------------------------------------
# Scale branch lengths by total tree height to make them a proportion of one:
tree_height <- mvSLOUCH::phyltree_paths(mytree)$tree_height
scaled_tree <- mytree
scaled_tree$edge.length <- scaled_tree$edge.length/tree_height
mvSLOUCH::phyltree_paths(scaled_tree)$tree_height
#This should now be 1

#-----------------------------------------------------------------------
# Regime specification 
#-----------------------------------------------------------------------
# Our hypothesized selective regime is habit. 
# mvSLOUCH implements an unordered parsimony algorithm for this purpose of reconstructing the regime 
# states on the phylogeny. 

# First store habitat categories in a new object where the order is specified 
# according to tree tip names:
regimes <- pcdata$habit[order(match(row.names(pcdata), scaled_tree$tip.label))]

# Run the parsimony reconstruction with deltran
regimesD <- mvSLOUCH::fitch.mvsl(scaled_tree, regimes, deltran = TRUE)

# Make pcdata (minus habit) into a matrix
mvData <- data.matrix(pcdata[,-1])
rownames(mvData) <- rownames(pcdata)

# mvSLOUCH works faster when the phylogeny object includes information on the paths and distances 
# for each node. This information can be obtained with the function mvSLOUCH::phyltree_paths():
mvStree <- mvSLOUCH::phyltree_paths(mytree) 

#--------------------------------------------------------------
# Fit models on PCs1-10
# Note that we can't use all PCs because solution is singular

# mvSLOUCH offers several options for parameter specification, 
# but none guarantees attaining the maximum likelihood peak.
# mvSLOUCH facilitates this process by offering a wrapper 
# function that runs different types of models on the data 
# from different starting points, estimate.evolutionary.model, 
# so we also use this to check we're reaching peaks.
#--------------------------------------------------------------

### Brownian motion
fitBM <- mvSLOUCH::BrownianMotionModel(mvStree, mvData[, 1:10])

## Check model reached a peak
checkBM <- mvSLOUCH::estimate.evolutionary.model(mvStree, mvData[, 1:10], 
                                                 repeats = 5, model.setups = "basic", doPrint = TRUE)
checkBM$BestModel$model

#--------------------------------------------------------------
### Single optimum OU
fitOU <- mvSLOUCH::ouchModel(mvStree, mvData[, 1:10])

## Check model reached a peak
checkOU <- mvSLOUCH::estimate.evolutionary.model(mvStree, mvData[, 1:10], 
                                                 repeats = 5, model.setups = "basic", doPrint = TRUE)
checkOU$BestModel$model

checkOU$BestModel$BestModel$ParamSummary$phyl.halflife$halflives
currently called OU1
#--------------------------------------------------------------
### Multi-rate Brownian motion models (BMS)
# Not possible in mvSLOUCH

### Multi-optima OU models (OUM) with constant alpha and sigma
### Multi-rate multi-optima OU motion models (OUMV)
### Multi-alpha multi-optima OU motion models (OUMA)
fitOUM <- mvSLOUCH::ouchModel(mvStree, mvData[, 1:10],
                                        regimes = regimesD$branch_regimes)

## Check model reached a peak
checkOUM <- mvSLOUCH::estimate.evolutionary.model(mvStree, mvData[, 1:10], 
                                                  repeats = 5, model.setups = "basic", doPrint = TRUE)
checkOUM$BestModel$model


#-----------------------------------------------------------------------
# Looking at the results
#-----------------------------------------------------------------------
# Extract AICc scores from the models
results <- c(AICc(fitBM), AICc(fitOU), AICc(fitBMS), AICc(fitOUM), AICc(fitOUMV))

# Get aic weights
aicw <- aicw(results)

# Look at the AIC weights to find the best fitting model
aicw

# Look at parameters of best fitting model

