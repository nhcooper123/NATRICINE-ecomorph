# mvMORPH evolutionary models
# 2022
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(geiger)
library(mvMORPH)
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
#-----------------------------------------------------------------------
## Estimate node values for habit categories
#-----------------------------------------------------------------------
# Make a new simple vector with just the habit character 
# and specie names as row names
habit <- mydata$habit
names(habit) <- mydata$Species

# Create sim map of tree with habit states
set.seed(42)
trees.SYM <- make.simmap(mytree, habit, model = "SYM", nsim = 1)

#subset data so it only contains the PCs
pcdata <- mydata[, 21:35]
rownames(pcdata) <- mydata$Species

#-----------------------------------------------------------------------
## Fitting models using `mvMORPH`
#-----------------------------------------------------------------------
# The models take a while to run. 
# It's important to carefully look at the parameters before interpreting 
# any of the results. But for simplicity I fit all the models first.
# Randomly choosing sim map 1

### Brownian motion
fitBM <- mvBM(trees.SYM, pcdata, model = "BM1", method = "sparse")

### Single optimum OU
fitOU <- mvOU(trees.SYM, pcdata, model = "OU1", method = "sparse")

### Multi-rate Brownian motion models (BMS)
# Uses the non-censored model of O'Meara et al. 2006 by default
fitBMS <- mvBM(trees.SYM, pcdata, model = "BMM", method = "sparse")

### Multi-optima OU models (OUM) with constant alpha and sigma
fitOUM <- mvOU(trees.SYM, pcdata, model = "OUM", method = "sparse", 
               param= list(alpha = "constraint", sigma = "constraint"))

### Multi-rate multi-optima OU motion models (OUMV)
fitOUMV <- mvOU(trees.SYM, pcdata, model = "OUM", method = "sparse", 
               param = list(sigma = "constraint"))

### Multi-alpha multi-optima OU motion models (OUMA)
#fitOUMA <- mvOU(trees.SYM, pcdata, model = "OUM", method = "sparse", 
                #param = list(alpha = "constraint"))

### Multi-rate multi-optima OU motion models (OUMVA)
fitOUMV <- mvOU(trees.SYM, pcdata, model = "OUM", method = "sparse")

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
