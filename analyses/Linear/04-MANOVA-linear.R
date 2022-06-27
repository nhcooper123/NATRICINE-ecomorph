# Modified 2022
#-----------------------------------------------------------------------
#  MANOVAs for different categories of habit and diet
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(mvMORPH)
library(geiger)

#-----------------------------------------------------------------------
# Read in data and wrangle it
#-----------------------------------------------------------------------
pc_data <- read_csv("data/Linear/snakepca-LSR.csv")

# Read in the tree
tree <- read.nexus("data/Linear/new_datedtree-LM.nexus")

# Remove habits that are unknown
pc_data_habit <- 
  pc_data %>%
  filter(Habit != "Unknown") %>%
  rename(habit = Habit)

# Remove diet that are unknown *note the no caps "U"
pc_data_diet <- 
  pc_data %>%
  filter(Diet != "unknown") %>%
  rename(diet = Diet)

# Extract from the tree only those species which match with the data
sps <- name.check(tree, pc_data_habit, data.names = pc_data_habit$Species)
tree_habit <- drop.tip(tree, sps$tree_not_data)

sps2 <- name.check(tree, pc_data_diet, data.names = pc_data_diet$Species)
tree_diet <- drop.tip(tree, sps2$tree_not_data)

# Extract from the data species not in the tree
matches <- match(pc_data_habit$Species, sps$data_not_tree, nomatch = 0)
pc_data_habit <- pc_data_habit[which(matches == 0), ]
rownames(pc_data_habit) <- pc_data_habit$Species

matches2 <- match(pc_data_diet$Species, sps2$data_not_tree, nomatch = 0)
pc_data_diet <- pc_data_diet[which(matches2 == 0), ]
rownames(pc_data_diet) <- pc_data_diet$Species

#-----------------------------------------------------------------------
# PGLS + MANOVA on PCs
#-----------------------------------------------------------------------
# Habit
# FitGLS model
model1 <- mvgls(as.matrix(pc_data_habit[, 21:35]) ~ habit, data = pc_data_habit, tree = tree_habit, 
                model = "BM", method = "LOO")
# Test using permutations
model1_test <- manova.gls(model1, nperm = 1000, test = "Pillai")
model1_test

# Diet
# FitGLS model
model2 <- mvgls(as.matrix(pc_data_diet[, 21:35]) ~ diet, data = pc_data_diet, tree = tree_diet, 
                model = "BM", method = "LOO")
# Test using permutations
model2_test <- manova.gls(model2, nperm = 1000, test = "Pillai")
model2_test

# Habit & diet 
# FitGLS model
model3 <- mvgls(as.matrix(pc_data_diet[, 21:35]) ~ Habit + diet, data = pc_data_diet, 
                tree = tree_diet, model = "BM", method = "LOO")
# Test using permutations
model3_test <- manova.gls(model3, nperm = 1000, test = "Pillai")
model3_test