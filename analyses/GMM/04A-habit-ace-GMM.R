# Ancestral state estimation for habit
# 2022

#-------------------------
# Load libraries
#-------------------------
library(ape)
library(tidyverse)
library(geiger)
library(here)

#------------------------------------------------------------
# Import data species means outputs and tree file
#------------------------------------------------------------
# species means PCA outputs matched and sorted with tips on the tree
snake2D <- read_csv("data/GMM/snake-data-pca-GMM.csv")

# Read in the tree
tree2D <- read.nexus("data/GMM/new_datedtree-GMM.nexus")

# Extract from the tree only those species which match with the data
sps <- name.check(tree2D, snake2D, data.names = snake2D$Species)
tree2D <- drop.tip(tree2D, sps$tree_not_data)

# Extract from the data species not in the tree
matches <- match(snake2D$Species, sps$data_not_tree, nomatch = 0)
snake2D <- snake2D[which(matches == 0), ]

# Look at the data
glimpse(snake2D)
str(tree2D)

# Drop species with no habit data
ds_new <- 
  snake2D %>%
  filter(Habit != "NA")

# Then order ds_new so it's the same order as the tree
ds_new <- ds_new[match(tree2D$tip.label, ds_new$Species),]

# Make sure ds_new is a dataframe
ds_new <- as.data.frame(ds_new)
#----------------------------------------------------------------
# Set up colour palettes
#----------------------------------------------------------------
mycolours_habit <- c("blue", "#51dacf", "#c8808b", "#85d272", "#5b5b5b")

#-----------
# Estimate ancestral states
ancestors_habit <- ace(ds_new$Habit, tree2D, type = "d")

# Plot habits 
plot.phylo(tree2D, label.offset = 2, show.tip.label = TRUE, type = "fan", no.margin = TRUE, cex = 0.5 )
tiplabels(offset = 1, pch = 21, bg = mycolours_habit[as.factor(ds_new$Habit)], cex = 1, adj = 1)
nodelabels(pie = ancestors_habit$lik.anc, piecol = mycolours_habit, cex = 0.3)
legend("topright", legend = levels(as.factor(ds_new$Habit)),
       fill = mycolours_habit, xpd = T, bty = "n", cex = 0.6, title = "habit")

# Save to file