#-----------------------------------------------------------------------
# Code for the supplemental PCA figures GMM
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(patchwork)

# Import data
data <- read_csv("data/GMM/snake-data-pca-GMM.csv")

#-----------------------------------------------------------------------
# Set up data for PC boxplots
#-----------------------------------------------------------------------
# Select only the first 8 PC's, reshape by ecomorph
ds2 <-
  data %>%
  #filter(Habit != "Unknown") %>%
  dplyr::select(Species, habit = Habit, PC1:PC8) %>%
  pivot_longer(PC1:PC8, "PC", "value") 

# Select only the first 8 PC's, reshape by diet
ds3 <-
  data %>%
  #filter(Diet != "Unknown") %>%
  dplyr::select(Species, diet = Diet, PC1:PC8) %>%
  pivot_longer(PC1:PC8, "PC", "value")

#-----------------------------------------------------------------------
# PC scores plots as boxplots
#-----------------------------------------------------------------------
# Plot for habit
habit_plot <- 
  ggplot(ds2, aes(x = PC, y = value, colour = habit)) +
  geom_jitter(alpha = 0.3) +
  theme_bw(base_size = 14) +
  geom_boxplot() +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black")) +
  ylim(-0.12,0.12) +
  coord_flip() +
  xlab("PC axis") +
  ylab("PC score")

# Save figure
# ggsave(habit_plot, filename = "outputs/GMM/Figures/PC1-PC8-ecomorph-GMM.png", width = 6)

# Plot for diet
diet_plot <-
  ggplot(ds3, aes(x = PC, y = value, colour = diet)) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot() +
  theme_bw(base_size = 14) +
  ylim(-0.12,0.12) +
  coord_flip() +
  xlab("PC axis") +
  ylab("PC score")

# Save figure
# ggsave(diet_plot, filename = "outputs/GMM/Figures/PC1-PC8-diet-GMM.png", width = 6)

#-----------------------------------------------------------------------
# PCA scatter plots for habit and diet PC1-3
#-----------------------------------------------------------------------
# Set up data
#-----------------------------------------------------------------------
# Create subsets of the data with just the variables of interest
habit_data <- 
  data %>%
  mutate(habit = as.factor(Habit))

diet_data <- 
  data %>%
  mutate(diet = as.factor(Diet))

#-----------------------------------------------------------------------
# PCA plots
#-----------------------------------------------------------------------
plot_habit_PC12 <-
  ggplot(habit_data, aes(x = PC1, y = PC2, col = habit)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black")) +
  labs(x = "PC1 (67.0%)", y = "PC2 (14.5%)")+
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)

plot_habit_PC23 <-
  ggplot(habit_data, aes(x = PC2, y = PC3, col = habit)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black")) +
  labs(x = "PC2 (14.5%)", y = "PC3 (5.53%)") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  coord_fixed(ratio = 1)


plot_diet_PC12 <-
  ggplot(diet_data, aes(x = PC1, y = PC2, col = diet)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")) +
  labs(x = "PC1 (67.0%)", y = "PC2 (14.5%)") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)

plot_diet_PC23 <-
  ggplot(diet_data, aes(x = PC2, y = PC3, col = diet)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")) +
  labs(x = "PC2 (14.5%)", y = "PC3 (5.53%)")+
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  coord_fixed(ratio = 1)

# Four panel plot
(plot_habit_PC12 + plot_habit_PC23) / (plot_diet_PC12 + plot_diet_PC23)

# Save figure
# ggsave(filename = "outputs/GMM/Figures/PC123-ecomorph-diet-GMM.png", height = 9, width = 13)
