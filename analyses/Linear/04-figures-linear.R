#-----------------------------------------------------------------------
# Code for the PCA figures
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(patchwork)

# Import data
data <- read_csv("data/Linear/snakepca.csv")

# Exclude unknown category in Habit and select only the first 7 PC's
ds2 <-
  data %>%
  filter(Habit != "Unknown") %>%
  dplyr::select(Species, Habit, PC1:PC7) %>%
  pivot_longer(PC1:PC7, "PC", "value")

#-----------------------------------------------------------------------
# PC scores plots as boxplots
#-----------------------------------------------------------------------
# Plot for habit
ds2_plot <- 
  ggplot(ds2, aes(x = PC, y = value, colour = Habit)) +
  scale_fill_manual(values=c("#007ba3", "#002397", "#92002e", "#868893", "#928600"))+
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600")) +
  ylim(-0.2,0.2) +
  coord_flip() +
  xlab("PC axis") +
  ylab("PC score")

# Plot for diet
ds3 <-
  data %>%
  filter(Diet != "unknown") %>%
  dplyr::select(Species, Diet, PC1:PC7) %>%
  pivot_longer(PC1:PC7, "PC", "value")

ds3_plot <-
  ggplot(ds3, aes(x = PC, y = value, colour = Diet)) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink")) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw(base_size = 14) +
  ylim(-0.2,0.2) +
  coord_flip() +
  xlab("PC axis") +
  ylab("PC score")

# Plot for origin
ds4 <-
  data %>%
  filter(Origin != "unknown") %>%
  dplyr::select(Species, Origin, PC1:PC7) %>%
  pivot_longer(PC1:PC7, "PC", "value")

ds4_plot <-
  ggplot(ds4, aes(x = PC, y = value, colour = Origin)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("#ff6364", "#64e291", "#0d8eae", "#e0b04c")) +
  ylim(-0.2,0.2) +
  coord_flip() +
  xlab("PC axis") +
  ylab("PC score")

# three panel figure
(ds2_plot + ds3_plot + ds4_plot)

# Save figure
# ggsave(filename = "outputs/Linear/PCs17-diet-habit-origin-LM.png", width = 12, height = 6)
#-----------------------------------------------------------------------
# PCA scatter plots for habit and diet
#-----------------------------------------------------------------------
plot_habit_PC12 <-
  ggplot(data, aes(x = PC1, y = PC2, col = Habit)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "red", "green", "yellow", "black")) +
  labs(x = "PC1 (67.0%)", y = "PC2 (14.5%)")+
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

plot_habit_PC23 <-
  ggplot(data, aes(x = PC2, y = PC3, col = Habit)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "red", "green", "yellow", "black")) +
  labs(x = "PC2 (14.5%)", y = "PC3 (5.53%)") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14)

# Diet remove unknowns
data2diet <-
  data %>%
  filter(Diet != "unknown")

plot_diet_PC12 <-
  ggplot(data2diet, aes(x = PC1, y = PC2, col = Diet)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink")) +
  labs(x = "PC1 (67.0%)", y = "PC2 (14.5%)") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

plot_diet_PC23 <-
  ggplot(data2diet, aes(x = PC2, y = PC3, col = Diet)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#007ba3", "#002397", "#92002e", "#868893", "#928600", "tan1", "yellow", "deeppink")) +
  labs(x = "PC2 (14.5%)", y = "PC3 (5.53%)")+
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        legend.text = element_text(face = 'italic')) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  geom_vline(xintercept = 0, linetype = 2, size = 0.5, col = "grey") +
  theme_bw(base_size = 14)

# Four panel plot
(plot_habit_PC12 + plot_habit_PC23) / (plot_diet_PC12 + plot_diet_PC23)

# Save figure
# ggsave(filename = "outputs/Linear/PC123-diet-habit-LM.png", height = 9, width = 13)
