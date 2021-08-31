# OUwie evolutionary models for GMM data
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(phytools)
library(geiger)
library(OUwie)
#-----------------------------------------------------------------------
## Prepare the tree and the data
#-----------------------------------------------------------------------
# Prepare the tree and the data
# Read in data
ds <- read_csv("data/GMM/snake-data-pca-GMM.csv")

# Make Habit into a factor
ds <- 
  ds %>%
  mutate(Habit = as.factor(Habit))

# Look at the data
#head(ds)

# Read in the tree and tidy the species names. 
# Force it to be ultrametric and fully bifurcating.
tree <- read.nexus("data/GMM/new_datedtree.nexus")

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
#-----------------------------------------------------------------------
## Estimate node values for ecomorph categories
#-----------------------------------------------------------------------
# Make a new simple vector with just the ecomorph character 
# and specie names as row names
ecomorph <- mydata$Habit
names(ecomorph) <- mydata$Species

# Simulate 500 trees with the tips we have and see what the best node values
# are for each tree. We'll then take the best across these 500 models
trees.SYM <- make.simmap(mytree, ecomorph, model = "SYM", nsim = 500)
summary.SYM <- summary(trees.SYM)

# Get the max likelihood state at each node
best <- apply(summary.SYM$ace, 1, which.max)
# Look at the output
# best

# Assign to the tree node labels
# Don't just use best as this only has numbers not the category names
mytree$node.label <- levels(ds$Habit)[best]

#-----------------------------------------------------------------------
## Fitting models using `OUwie`
#-----------------------------------------------------------------------
# For `OUwie` first we need to set up a dataset with the species names first,
# the "regime" which in this case is the ecomorphs, and then 
# the continuous variable we think might be evolving differently in response 
# to different selective regimes. 

mydata_pc1 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC1)
mydata_pc2 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC2)
mydata_pc3 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC3)
mydata_pc4 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC4)
mydata_pc5 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC5)
mydata_pc6 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC6)
mydata_pc7 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC7)
mydata_pc8 <- data.frame(species = mydata$Species, 
                         regime = mydata$Habit, trait = mydata$PC8)

# The models take a while to run. 
# It's important to carefully look at the parameters before interpreting 
# any of the results. But for simplicity I fit all the models first.

### Brownian motion
BM_pc1 <- OUwie(mytree, mydata_pc1, model = "BM1")
BM_pc2 <- OUwie(mytree, mydata_pc2, model = "BM1")
BM_pc3 <- OUwie(mytree, mydata_pc3, model = "BM1")
BM_pc4 <- OUwie(mytree, mydata_pc4, model = "BM1")
BM_pc5 <- OUwie(mytree, mydata_pc5, model = "BM1")
BM_pc6 <- OUwie(mytree, mydata_pc6, model = "BM1")
BM_pc7 <- OUwie(mytree, mydata_pc7, model = "BM1")
BM_pc8 <- OUwie(mytree, mydata_pc8, model = "BM1")

### Single optimum OU
OU_pc1 <- OUwie(mytree, mydata_pc1, model = "OU1")
OU_pc2 <- OUwie(mytree, mydata_pc2, model = "OU1")
OU_pc3 <- OUwie(mytree, mydata_pc3, model = "OU1")
OU_pc4 <- OUwie(mytree, mydata_pc4, model = "OU1")
OU_pc5 <- OUwie(mytree, mydata_pc5, model = "OU1")
OU_pc6 <- OUwie(mytree, mydata_pc6, model = "OU1")
OU_pc7 <- OUwie(mytree, mydata_pc7, model = "OU1")
OU_pc8 <- OUwie(mytree, mydata_pc8, model = "OU1")

### Multi-rate Brownian motion models (BMS)
# Note that here we set the root to invoke the non-censored model of O'Meara et al. 2006 

BMV_pc1 <- OUwie(mytree, mydata_pc1, model = "BMS", root.station = FALSE)
BMV_pc2 <- OUwie(mytree, mydata_pc2, model = "BMS", root.station = FALSE)
BMV_pc3 <- OUwie(mytree, mydata_pc3, model = "BMS", root.station = FALSE)
BMV_pc4 <- OUwie(mytree, mydata_pc4, model = "BMS", root.station = FALSE)
BMV_pc5 <- OUwie(mytree, mydata_pc5, model = "BMS", root.station = FALSE)
BMV_pc6 <- OUwie(mytree, mydata_pc6, model = "BMS", root.station = FALSE)
BMV_pc7 <- OUwie(mytree, mydata_pc7, model = "BMS", root.station = FALSE)
BMV_pc8 <- OUwie(mytree, mydata_pc8, model = "BMS", root.station = FALSE)

### Multi-optima OU motion models (OUM)
OUM_pc1 <- OUwie(mytree, mydata_pc1, model = "OUM")
OUM_pc2 <- OUwie(mytree, mydata_pc2, model = "OUM")
OUM_pc3 <- OUwie(mytree, mydata_pc3, model = "OUM")
OUM_pc4 <- OUwie(mytree, mydata_pc4, model = "OUM")
OUM_pc5 <- OUwie(mytree, mydata_pc5, model = "OUM")
OUM_pc6 <- OUwie(mytree, mydata_pc6, model = "OUM")
OUM_pc7 <- OUwie(mytree, mydata_pc7, model = "OUM")
OUM_pc8 <- OUwie(mytree, mydata_pc8, model = "OUM")

### Multi-rate multi-optima OU motion models (OUMV)
OUMV_pc1 <- OUwie(mytree, mydata_pc1, model = "OUMV")
OUMV_pc2 <- OUwie(mytree, mydata_pc2, model = "OUMV")
OUMV_pc3 <- OUwie(mytree, mydata_pc3, model = "OUMV")
OUMV_pc4 <- OUwie(mytree, mydata_pc4, model = "OUMV")
OUMV_pc5 <- OUwie(mytree, mydata_pc5, model = "OUMV")
OUMV_pc6 <- OUwie(mytree, mydata_pc6, model = "OUMV")
OUMV_pc7 <- OUwie(mytree, mydata_pc7, model = "OUMV")
OUMV_pc8 <- OUwie(mytree, mydata_pc8, model = "OUMV")

#-----------------------------------------------------------------------
### Looking at the results
#-----------------------------------------------------------------------
# Extract AICc scores from the models
aic.scores_pc1 <- setNames(c(BM_pc1$AICc, OU_pc1$AICc, BMV_pc1$AICc,
                             OUM_pc1$AICc, OUMV_pc1$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))
aic.scores_pc2 <- setNames(c(BM_pc2$AICc, OU_pc2$AICc, BMV_pc2$AICc,
                             OUM_pc2$AICc, OUMV_pc2$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))
aic.scores_pc3 <- setNames(c(BM_pc3$AICc, OU_pc3$AICc, BMV_pc3$AICc,
                             OUM_pc3$AICc, OUMV_pc3$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))
aic.scores_pc4 <- setNames(c(BM_pc4$AICc, OU_pc4$AICc, BMV_pc4$AICc,
                             OUM_pc4$AICc, OUMV_pc4$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))
aic.scores_pc5 <- setNames(c(BM_pc5$AICc, OU_pc5$AICc, BMV_pc5$AICc,
                             OUM_pc5$AICc, OUMV_pc5$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))
aic.scores_pc6 <- setNames(c(BM_pc6$AICc, OU_pc6$AICc, BMV_pc6$AICc,
                             OUM_pc6$AICc, OUMV_pc6$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))
aic.scores_pc7 <- setNames(c(BM_pc7$AICc, OU_pc7$AICc, BMV_pc7$AICc,
                             OUM_pc7$AICc, OUMV_pc7$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))
aic.scores_pc8 <- setNames(c(BM_pc8$AICc, OU_pc8$AICc, BMV_pc8$AICc,
                             OUM_pc8$AICc, OUMV_pc8$AICc), 
                           c("BM", "OU", "BMV", "OUM", "OUMV"))

# Get aic weights
aicw_pc1 <- aicw(aic.scores_pc1)
aicw_pc2 <- aicw(aic.scores_pc2)
aicw_pc3 <- aicw(aic.scores_pc3)
aicw_pc4 <- aicw(aic.scores_pc4)
aicw_pc5 <- aicw(aic.scores_pc5)
aicw_pc6 <- aicw(aic.scores_pc6)
aicw_pc7 <- aicw(aic.scores_pc7)
aicw_pc8 <- aicw(aic.scores_pc8)

### PC1
# Look at the AIC weights to find the best fitting model
aicw_pc1
# Look at parameters of best fitting model
OUM_pc1

### PC2
aicw_pc2
OUM_pc2

### PC3
aicw_pc3
OUM_pc3

### PC4
aicw_pc4
OUM_pc4

### PC5
aicw_pc5
OUMV_pc5

### PC6
aicw_pc6
OU_pc6

### PC7
aicw_pc7
OUM_pc7

### PC8
aicw_pc8
OU_pc8