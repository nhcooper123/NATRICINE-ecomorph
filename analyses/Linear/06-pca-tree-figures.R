# Modified 2022
#-----------------------------------------------------------------------
# Code for the PCA figures plus trees
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(patchwork)
library(geiger)
library(ggtree)
library(phytools)

# Add colour palettes
diet_colours <- c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")
habit_colours <- c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black")

#-----------------------------------------------------------------------
# Import data
data <- read_csv("data/Linear/snakepca-LSR.csv")

# Add species names as rownames
data <- data.frame(data)
rownames(data) <- data$Species

# Create subsets of the data with just the variables of interest
# Make sure they are factors
habit_data <- 
  data %>%
  mutate(habit = as.factor(Habit)) %>%
  mutate(diet = as.factor(Diet))
#------------------------------------------------------------------
# Read in the tree
tree <- read.nexus("data/Linear/new_datedtree-LM.nexus")

# Extract from the tree only those species which match with the data
sps <- name.check(tree, habit_data, data.names = rownames(habit_data)) 
tree <- drop.tip(tree, sps$tree_not_data)

#-----------------------------------------------------------------------
# PCA scatter plots for habit and diet
#-----------------------------------------------------------------------
plot_habit_PC12 <-
  ggplot(habit_data, aes(x = PC1, y = PC2, col = habit)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = habit_colours) +
  labs(x = "PC1 (36.2%)", y = "PC2 (16.9%)")+
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

plot_diet_PC12 <-
  ggplot(habit_data, aes(x = PC1, y = PC2, col = diet)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = diet_colours) +
  labs(x = "PC1 (36.2%)", y = "PC2 (16.9%)") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

#-----------------------------------------------------------------------
# Plot trees with traits at the tips
#-----------------------------------------------------------------------
# Create base tree to plot on
tree_base <- ggtree(tree, layout = "circular")

# Select just relevant info from the datasets
habit <- habit_data %>% dplyr::select(habit)
diet <- habit_data %>% dplyr::select(diet)

# Plot habit tree
tree1 <- 
  gheatmap(tree_base, habit, offset = 0, width =.1, colnames = FALSE) +
  scale_fill_manual(values = habit_colours, name = "habit", guide = guide_legend(order = 1))

# Plot diet tree
tree2 <- 
  gheatmap(tree_base, diet, offset = 0, width =.1, colnames = FALSE) +
  scale_fill_manual(values = diet_colours, name = "diet", guide = guide_legend(order = 1))

#-----------------------------------------------------------------------
# Four panel plot
(plot_habit_PC12 + tree1) / (plot_diet_PC12 + tree2)

# Save figure
# ggsave(filename = "outputs/Linear/Figures/PCA-ecomorph-diet-trees-LSR.png", width = 9)

#----------------------
# Phylomorphospace
# ---------------------
# Extract from the data species not in the tree
sps <- name.check(tree, habit_data, data.names = habit_data$Species)
matches <- match(habit_data$Species, sps$data_not_tree, nomatch = 0)
phendata <- habit_data[which(matches == 0), ]

# Set up colours
cols_habit <- setNames(habit_colours, levels(phendata$habit))
cols_diet <- setNames(diet_colours, levels(phendata$diet))

# Set up data for mapping
habit_col <- setNames(phendata$habit, phendata$Species)
diet_col <- setNames(phendata$diet, phendata$Species)

# Make simmaps
newtree_habit <- make.simmap(tree, habit_col, model = "SYM", nsim = 1)
newtree_diet <- make.simmap(tree, diet_col, model = "SYM", nsim = 1)

# Plots
phylomorphospace(newtree_habit, phendata[,21:22], label = "off", colors = cols_habit, 
                 node.by.map = TRUE, cex = 16, xlab = "PC1 (36.2%)", ylab = "PC2 (16.9%)")

# Export manually phylomorphospace-ecomorph-LSR.png

phylomorphospace(newtree_diet, phendata[,21:22], label = "off", colors = cols_diet, 
                 node.by.map = TRUE, cex = 16, xlab = "PC1 (36.2%)", ylab = "PC2 (16.9%)")
# Export manually phylomorphospace-diet-LSR.png