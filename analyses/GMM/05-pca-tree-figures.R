#-----------------------------------------------------------------------
# Code for the PCA figures plus trees GMM data
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(patchwork)
library(geiger)
library(ggtree)

# Add colour palettes
diet_colours <- c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")
habit_colours <- c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black")

#-----------------------------------------------------------------------
# Import data
data <- read_csv("data/GMM/snake-data-pca-GMM.csv")

# Add species names as rownames
data <- data.frame(data)
rownames(data) <- data$Species

# Create subsets of the data with just the variables of interest
# Make sure they are factors
habit_data <- 
  data %>%
  mutate(ecomorph = as.factor(Habit)) %>%
  mutate(diet = as.factor(Diet))
#------------------------------------------------------------------
# Read in the tree
tree <- read.nexus("data/GMM/new_datedtree-GMM.nexus")

# Extract from the tree only those species which match with the data
sps <- name.check(tree, habit_data, data.names = rownames(habit_data)) 
tree <- drop.tip(tree, sps$tree_not_data)

#-----------------------------------------------------------------------
# PCA scatter plots for habit and diet
#-----------------------------------------------------------------------
plot_habit_PC12 <-
  ggplot(habit_data, aes(x = PC1, y = PC2, col = ecomorph)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = habit_colours) +
  labs(x = "PC1 (42.3%)", y = "PC2 (29.2%)")+
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")+
  coord_fixed(ratio = 1)

plot_diet_PC12 <-
  ggplot(habit_data, aes(x = PC1, y = PC2, col = diet)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = diet_colours) +
  labs(x = "PC1 (42.3%)", y = "PC2 (29.2%)") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)

#-----------------------------------------------------------------------
# Plot trees with traits at the tips
#-----------------------------------------------------------------------
# Create base tree to plot on
tree_base <- ggtree(tree, layout = "circular")

# Select just relevant info from the datasets
habit <- habit_data %>% select(ecomorph)
diet <- habit_data %>% select(diet)

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
# ggsave(filename = "outputs/GMM/Figures/PCA-ecomorph-diet-trees-GMM.png", width = 9)
