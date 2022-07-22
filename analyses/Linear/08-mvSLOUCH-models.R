# Fast mvSLOUCH code
# 2022

# See vignette here for more info:
# https://cran.r-project.org/web/packages/mvSLOUCH/vignettes/mvSLOUCH_Carnivorans.html
#------------------------
# Load libraries
library(mvSLOUCH)
library(PCMBaseCpp)
library(ape)
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
# glimpse(mydata)
# str(mytree)

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

# Run the parsimony reconstruction with deltran and root = Terrestrial
# Need to root or you get ambiguous nodes
# Try three different root options
regimesT <- mvSLOUCH::fitch.mvsl(scaled_tree, regimes, deltran = TRUE, root = "Terrestrial")
regimesA <- mvSLOUCH::fitch.mvsl(scaled_tree, regimes, deltran = TRUE, root = "Aquatic")
regimesSA <- mvSLOUCH::fitch.mvsl(scaled_tree, regimes, deltran = TRUE, root = "Semiaquatic")

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

# Get AICc
fitBM$ParamSummary$aic.c 

# Get degrees of freedom
fitBM$ParamSummary$dof

# Information for supplemental info
# fitBM$ParamsInModel$vX0
# fitBM$ParamsInModel$Sxx
# fitBM$ParamSummary$StS

#--------------------------------------------------------------
### Single optimum OU
fitOU <- mvSLOUCH::ouchModel(mvStree, mvData[, 1:10])

## Check model reached a peak
checkOU <- mvSLOUCH::estimate.evolutionary.model(mvStree, mvData[, 1:10], 
                                                 repeats = 5, model.setups = "basic", doPrint = TRUE)
# Which is the best? 
checkOU$BestModel$model

# Get AICc
checkOU$BestModel$aic.c 

# Get degrees of freedom
checkOU$BestModel$BestModel$ParamSummary$dof

# Info for supplemental
# checkOU$BestModel$BestModel$ParamsInModel$vY0
# checkOU$BestModel$BestModel$ParamsInModel$A
# checkOU$BestModel$BestModel$ParamsInModel$Syy
# checkOU$BestModel$BestModel$ParamSummary$phyl.halflife
#--------------------------------------------------------------
### Multi-rate Brownian motion models (BMS)
# Not possible in mvSLOUCH

### Multi-optima OU models (OUM) with constant alpha and sigma
### Multi-rate multi-optima OU motion models (OUMV)
### Multi-alpha multi-optima OU motion models (OUMA)

## Fit for each of three root choices
fitOUM_T <- mvSLOUCH::ouchModel(mvStree, mvData[, 1:10],
                                        regimes = regimesT$branch_regimes, root.regime = "Terrestrial",
                                Atype = "Diagonal")

fitOUM_A <- mvSLOUCH::ouchModel(mvStree, mvData[, 1:10],
                                regimes = regimesA$branch_regimes, root.regime = "Aquatic",
                                Atype = "Diagonal")

fitOUM_SA <- mvSLOUCH::ouchModel(mvStree, mvData[, 1:10],
                                regimes = regimesSA$branch_regimes, root.regime = "Semiaquatic",
                                Atype = "Diagonal")

## Check models reached a peak
checkOUM_T <- mvSLOUCH::estimate.evolutionary.model(mvStree, mvData[, 1:10], 
                                                  regimes = regimesT$branch_regimes, root.regime = "Terrestrial",
                                                  repeats = 5, model.setups = "basic", doPrint = TRUE)

checkOUM_A <- mvSLOUCH::estimate.evolutionary.model(mvStree, mvData[, 1:10], 
                                                    regimes = regimesA$branch_regimes, root.regime = "Aquatic",
                                                    repeats = 5, model.setups = "basic", doPrint = TRUE)

checkOUM_SA <- mvSLOUCH::estimate.evolutionary.model(mvStree, mvData[, 1:10], 
                                                    regimes = regimesSA$branch_regimes, root.regime = "Semiaquatic",
                                                    repeats = 5, model.setups = "basic", doPrint = TRUE)

# Which is the best? 
checkOUM_T$BestModel$model
checkOUM_A$BestModel$model
checkOUM_SA$BestModel$model

# What is the AICc for the best?
checkOUM_T$BestModel$BestModel$aic.c 
checkOUM_A$BestModel$BestModel$aic.c 
checkOUM_SA$BestModel$BestModel$aic.c 

# Get degrees of freedom
checkOUM_T$BestModel$BestModel$ParamSummary$dof
checkOUM_A$BestModel$BestModel$ParamSummary$dof
checkOUM_SA$BestModel$BestModel$ParamSummary$dof

# Info for supplemental
# checkOUM_T$BestModel$BestModel$ParamsInModel$vY0
# checkOUM_T$BestModel$BestModel$ParamsInModel$A
# checkOUM_T$BestModel$BestModel$ParamsInModel$Syy
# checkOUM_T$BestModel$BestModel$ParamsInModel$mPsi
# checkOUM_T$BestModel$BestModel$ParamSummary$confidence.interval$regression.summary$mPsi.regression.confidence.interval$Lower.end
# checkOUM_T$BestModel$BestModel$ParamSummary$confidence.interval$regression.summary$mPsi.regression.confidence.interval$Estimated.Point
# checkOUM_T$BestModel$BestModel$ParamSummary$confidence.interval$regression.summary$mPsi.regression.confidence.interval$Upper.end
# checkOUM_T$BestModel$BestModel$ParamSummary$phyl.halflife
