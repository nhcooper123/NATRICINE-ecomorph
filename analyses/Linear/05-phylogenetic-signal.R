# Analyses using natricine linear morphology dataset
# Is shape related to phylogeny?
#----------------------------------

#----------------------------------
# Analysis prep
#----------------------------------
# Load libraries
library(ape)
library(tidyverse)
library(geiger)
library(phytools)
#library(phylobase)
#library(dispRity)
#library(BAMMtools)
#library(phytools)
library(geomorph)
#library(tibble)

# Import the PCA output data which phylogenetic data 
snake <- read_csv("data/Linear/snakepca.csv")

# Import the ultrametric tree
tree <- read.nexus("data/Linear/new_datedtree-LM.nexus")
# Check the tree has the right number of tips, and nodes
tree

#-----------------------------------
# Matching up the tree to the data
#-----------------------------------
# First replace spaces with _
snake <-
  snake %>%
  mutate(Species = str_replace_all(Species, " ", "_"))

# Now see if the names match using name.check
matches <- name.check(phy = tree, data = snake, data.names = snake$Species)

# Extract only the data for which genetic data is available
tree_new <- drop.tip(tree, matches$tree_not_data)

# Remove the missing species from the data
fix <- match(snake$Species, matches$data_not_tree, nomatch = 0)
snake_new <- subset(snake, fix == 0)

# Then order ds_new so it's the same order as the tree
snake_new <- snake_new[match(tree_new$tip.label, snake_new$Species),]

#-------------------------------------------------
# Estimating phylogenetic signal in each variable
#-------------------------------------------------

outputK <- data.frame(array(dim = c(21,3))) 
names(outputK) <- c("variable", "K", "p")

for(i in 6:26){
  measure <- pull(snake_new[, i])
  names(measure) <- snake_new$Species
  phsigK <- phylosig(tree = tree_new, measure, method = "K", test = TRUE, nsim = 10000)
  # Save outputs
  outputK$variable[i-5] <- names(snake_new[i])
  outputK$K[i-5] <- phsigK$K
  outputK$p[i-5] <- phsigK$P
}

# Save outputs
# Note that p values will differ each time as they are based on simulations
# write_csv(outputK, file = "outputs/Linear/phylo-signal-LM.csv")