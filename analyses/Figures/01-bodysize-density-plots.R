# Create body-size density plots

# Load libraries
library(tidyverse)
library(patchwork)
library(cowplot)

# Import data
# Includes 1374 individuals 185 species including four outgroups
ds <- read_csv("data/Linear/metadata-data-natricine-LM.csv")
glimpse(ds)

# Remove unknown habit species
ds <- 
  ds %>%
  filter(Habit != "Unknown")
  
# Split the dataset by Origin
ds_a <- filter(ds, Origin == "Asian")
ds_b <- filter(ds, Origin == "American")
ds_c <- filter(ds, Origin == "African")
ds_d <- filter(ds, Origin == "Asian-European")

# Plot for Asia
plot_a <-
  # Basic density plot coloured by ecotype
  ggplot(ds_a, aes(x = SVL, colour = Habit)) +
  geom_density() +
  # Get rid of the grey background
  # And increase the axes label sizes
  theme_bw(base_size = 14) +
  # Split the plots by ecotype
  facet_wrap(~ Habit, nrow = 1) +
  # Remove the legend as it isn't needed
  theme(legend.position = "none") +
  # add custom colors
  scale_color_manual(values=c("#007ba3", "#002397", "#92002e", "#868893", "#928600")) +
  # Change x axis label
  xlab("Snout-vent length (mm)") +
  # Remove grey background where ecotype name is 
  theme(strip.background = element_rect(colour = "black", fill = NA)) +
  # Add sublabel
  # Set the x and y axes limits so they are 
  # the same for each ecotype
  xlim(0, 1200) +
  ylim(0, 0.010) +
  # Angle the x ticks labels so they are readable
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for North and Central America
plot_b <-
  ggplot(ds_b, aes(x = SVL, colour = Habit)) +
  geom_density() +
  theme_bw(base_size = 14) +
  scale_color_manual(values=c("#007ba3", "#92002e", "#868893", "#928600")) +
  facet_wrap(~ Habit, nrow = 1) +
  theme(legend.position = "none") +
  xlab("Snout-vent length (mm)") +
  theme(strip.background = element_rect(colour = "black", fill = NA)) +
  xlim(0, 1200) +
  ylim(0, 0.010) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# subsaharan Africa
plot_c <-
  ggplot(ds_c, aes(x = SVL, colour = Habit)) +
  geom_density() +
  theme_bw(base_size = 14) +
  #color
  scale_color_manual(values=c("#007ba3", "#868893", "#928600")) +
  facet_wrap(~ Habit, nrow = 1) +
  theme(legend.position = "none") +
  xlab("Snout-vent length (mm)") +
  theme(strip.background = element_rect(colour = "black", fill = NA)) +
  xlim(0, 1200) +
  ylim(0, 0.010) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Eurasia
plot_d <-
  ggplot(ds_d, aes(x = SVL, colour = Habit)) +
  geom_density() +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("#007ba3", "#868893")) +
  facet_wrap(~ Habit, nrow = 1) +
  theme(legend.position = "none") +
  xlab("Snout-vent length (mm)") +
  theme(strip.background = element_rect(colour = "black", fill = NA)) +
  xlim(0, 1200) +
  ylim(0, 0.010) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4 panel figure
plot_grid(plot_a, plot_b, plot_c, plot_d,
          nrow = 4,
          labels = "AUTO",
          label_size = 12)

# Save (this seems big but looks nice pasted into a document)
ggsave("outputs/SVL-densityplot.png", width = 12, height = 8)