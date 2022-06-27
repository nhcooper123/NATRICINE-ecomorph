# Modified 2022
#-----------------------------------------------------------------------
# Which model of discrete trait evolution fits best for ecomorph?
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(geiger)
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
  filter(Habit != "Unknown")

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
# Make a new simple vector with just the ecomorph character 
# and specie names as row names
ecomorph <- mydata$Habit
names(ecomorph) <- mydata$Species

#-----------------------------------------------------------------------
# Fit the three main discrete models of evolution
#-----------------------------------------------------------------------
# These take a few minutes to run
er <- fitDiscrete(mytree, ecomorph, model = "ER")
sym <- fitDiscrete(mytree, ecomorph, model = "SYM")
ard <- fitDiscrete(mytree, ecomorph, model = "ARD")

# Which is the best fitting model?
# Extract AICc values for each model and give them names
aic.discrete <- setNames(c(er$opt$aic, sym$opt$aic, ard$opt$aic), 
                         c("equal", "symmetric", "different"))
# Compare AICw values
aicw(aic.discrete)
