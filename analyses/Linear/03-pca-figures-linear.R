# Modified 2022
#-----------------------------------------------------------------------
# Code for the supplemental PCA figures
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(patchwork)

# Import data
data <- read_csv("data/Linear/snakepca-LSR.csv")

#-----------------------------------------------------------------------
# Set up data for PC boxplots
#-----------------------------------------------------------------------
# Select only the first 10 PC's, reshape by ecomorph
ds2 <-
  data %>%
  #filter(Habit != "Unknown") %>%
  dplyr::select(Species, habit = Habit, PC1:PC10) %>%
  pivot_longer(PC1:PC10, "PC", "value") %>%
  # relevel to makes sure PC10 is last
  mutate(PC = factor(PC, levels = c("PC1", "PC2", "PC3", "PC4", "PC5",
                                    "PC6" ,"PC7", "PC8", "PC9", "PC10")))

# Select only the first 10 PC's, reshape by diet
ds3 <-
  data %>%
  #filter(Diet != "Unknown") %>%
  dplyr::select(Species, diet = Diet, PC1:PC10) %>%
  pivot_longer(PC1:PC10, "PC", "value") %>%
  # relevel to makes sure PC10 is last
  mutate(PC = factor(PC, levels = c("PC1", "PC2", "PC3", "PC4", "PC5",
                                    "PC6" ,"PC7", "PC8", "PC9", "PC10")))

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
  ylim(-10,15) +
  coord_flip() +
  xlab("PC axis") +
  ylab("PC score")

# Save figure
# ggsave(habit_plot, filename = "outputs/Linear/Figures/PC1-PC10-ecomorph-LM-LSR.png", width = 6)

# Plot for diet
diet_plot <-
  ggplot(ds3, aes(x = PC, y = value, colour = diet)) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")) +
  geom_jitter(alpha = 0.3) +
  geom_boxplot() +
  theme_bw(base_size = 14) +
  ylim(-10,15) +
  coord_flip() +
  xlab("PC axis") +
  ylab("PC score")

# Save figure
# ggsave(diet_plot, filename = "outputs/Linear/Figures/PC1-PC10-diet-LM-LSR.png", width = 6)

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
  labs(x = "PC1 (36.2%)", y = "PC2 (16.9%)")+
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)

plot_habit_PC23 <-
  ggplot(habit_data, aes(x = PC2, y = PC3, col = habit)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black")) +
  labs(x = "PC2 (16.9%)", y = "PC3 (15.5%)") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  coord_fixed(ratio = 1)

plot_diet_PC12 <-
  ggplot(diet_data, aes(x = PC1, y = PC2, col = diet)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")) +
  labs(x = "PC1 (36.2%)", y = "PC2 (16.9%)") +
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
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink", "black")) +
  labs(x = "PC2 (16.9%)", y = "PC3 (15.5%)")+
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
# ggsave(filename = "outputs/Linear/Figures/PC123-ecomorph-diet-LM-LSR.png", height = 9, width = 13)

#--------------------------
# PCA plot base for Fig 3
#--------------------------
# Fit convex hulls
hull <- habit_data %>%
  group_by(habit) %>%
  slice(chull(PC1, PC2))

ggplot(habit_data, aes(x = PC1, y = PC2, col = habit, fill = habit)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black")) +
  #scale_fill_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "black"), guide = "none") +
  labs(x = "PC1 (36.2%)", y = "PC2 (16.9%)")+
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1) +
  theme(legend.position = c(0.88,0.76)) #+ 
  #geom_polygon(data = hull, alpha = 0.2)

# ggsave(filename = "outputs/Linear/Figures/Figure3-legend.png", height = 9, width = 9)
