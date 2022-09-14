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
# species means PCA outputs
snake <- read_csv("data/Linear/snakepca-LSR.csv")

# Read in the tree
tree <- read.nexus("data/Linear/new_datedtree-LM.nexus")

# Drop species with no habit data
ds_new <- 
  snake %>%
  filter(Habit != "NA")

#----------------------------------------------------------------
# Match up the tree to the data
#--------------------------------------------------------------
# Now see if the names match using name.check
matches <- name.check(phy = tree, data = ds_new, data.names = ds_new$Species)

# Use drop.tip, and tell it to remove the species you found above that
# are not in the data
tree_new <- drop.tip(tree, matches$tree_not_data)

# Remove the missing species from the data
fix <- match(ds_new$Species, matches$data_not_tree, nomatch = 0)
ds_new <- subset(ds_new, fix == 0)

# Then order ds_new so it's the same order as the tree
ds_new <- ds_new[match(tree_new$tip.label, ds_new$Species),]

# Make sure ds_new is a dataframe
ds_new <- as.data.frame(ds_new)
#----------------------------------------------------------------
# Set up colour palettes
#----------------------------------------------------------------
mycolours_habit <- c("blue", "#51dacf", "#c8808b", "#85d272", "#5b5b5b")

#-----------
# Estimate ancestral states
ancestors_habit <- ace(ds_new$Habit, tree_new, type = "d")

# Plot habits 
plot.phylo(tree_new, label.offset = 2, show.tip.label = TRUE, type = "fan", no.margin = TRUE, cex = 0.5 )
tiplabels(offset = 1, pch = 21, bg = mycolours_habit[as.factor(ds_new$Habit)], cex = 1, adj = 1)
nodelabels(pie = ancestors_habit$lik.anc, piecol = mycolours_habit, cex = 0.3)
legend("topright", legend = levels(as.factor(ds_new$Habit)),
       fill = mycolours_habit, xpd = T, bty = "n", cex = 0.6, title = "habit")

# Save to file

#----------------------------------------------------------------
# Create list of possible pairs
#----------------------------------------------------------------
xx1 <- 
 ds_new %>%
 filter(Habit == "Burrowing") %>%
 select(Taxa1 = Species)  %>%
 mutate(Taxa2 = Taxa1) %>%
 expand.grid() %>%
 mutate(Habit = "Burrowing")

xx2 <- 
  ds_new %>%
  filter(Habit == "Semiaquatic") %>%
  select(Taxa1 = Species)  %>%
  mutate(Taxa2 = Taxa1) %>%
  expand.grid() %>%
  mutate(Habit = "Semiaquatic")

xx3 <- 
  ds_new %>%
  filter(Habit == "Terrestrial") %>%
  select(Taxa1 = Species)  %>%
  mutate(Taxa2 = Taxa1) %>%
  expand.grid() %>%
  mutate(Habit = "Terrestrial")

xx4 <- 
  ds_new %>%
  filter(Habit == "Aquatic") %>%
  select(Taxa1 = Species)  %>%
  mutate(Taxa2 = Taxa1) %>%
  expand.grid() %>%
  mutate(Habit = "Aquatic")

xx5 <- 
  ds_new %>%
  filter(Habit == "Aquatic Burrowing") %>%
  select(Taxa1 = Species)  %>%
  mutate(Taxa2 = Taxa1) %>%
  expand.grid() %>%
  mutate(Habit = "Aquatic Burrowing")

xx <- rbind(xx1, xx2, xx3, xx4, xx5)

#write_csv(file = here("possible-pairs.csv"), xx)
