# Phylogenetic convergence testing for linear data
# Stayton's distance-based metric
# Summaries from looping over all putative pairs of convergent taxa
# 2022
#-------------------------------------
# Load libraries
library(tidyverse)
library(patchwork)

remove_x <- 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#------------------------------------------------------------
# Import results
#------------------------------------------------------------
results <- read_csv("outputs/GMM/Tables/convevol-results-pairs-GMM.csv") 

#------------------------------------------------------------
# Summaries
#------------------------------------------------------------  

sumDat <- 
  results %>%
  group_by(Habit) %>%
  summarise(across(C1:pC4, median))

results <- 
  results %>%
  mutate(converge1 = case_when(pC1 < 0.05 ~ "Yes", pC1 >= 0.05 ~ "No")) %>%
  mutate(converge2 = case_when(pC2 < 0.05 ~ "Yes", pC2 >= 0.05 ~ "No")) %>%
  mutate(converge3 = case_when(pC3 < 0.05 ~ "Yes", pC3 >= 0.05 ~ "No")) %>%
  mutate(converge4 = case_when(pC4 < 0.05 ~ "Yes", pC4 >= 0.05 ~ "No")) %>%
  mutate(overall = case_when(pC1 < 0.05 | pC2 < 0.05 | pC4 < 0.05 | pC4 < 0.05 ~ 1)) %>%
  mutate(overall = case_when(is.na(overall) ~ 0, !is.na(overall) ~ 1))

significant <- 
results %>%
  group_by(Habit) %>%
  summarise(yes = sum(overall),
            N = n(),
            no = N - yes,
            percent = yes/N * 100)
#------------------------------------------------------------
# Plots - C values
#------------------------------------------------------------  

c1 <-
  ggplot(results, aes(x = C1)) +
  geom_density() +
  geom_vline(data = sumDat, aes(xintercept = C1), colour = "red") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

c2 <-
  ggplot(results, aes(x = C2)) +
  geom_density() +
  geom_vline(data = sumDat, aes(xintercept = C2), colour = "red") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

c3 <-
  ggplot(results, aes(x = C3)) +
  geom_density() +
  geom_vline(data = sumDat, aes(xintercept = C3), colour = "red") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

c4 <-
  ggplot(results, aes(x = C4)) +
  geom_density() +
  geom_vline(data = sumDat, aes(xintercept = C4), colour = "red") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

# Plot them all
c1/c2/c3/c4  

#------------------------------------------------------------
# Plots - p values
#------------------------------------------------------------  

pc1 <-
  ggplot(results, aes(x = pC1)) +
  geom_density() +
  geom_vline(xintercept = 0.05, colour = "blue", lty = "dotted") +
  geom_vline(data = sumDat, aes(xintercept = pC1), colour = "red") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

pc2 <-
  ggplot(results, aes(x = pC2)) +
  geom_density() +
  geom_vline(xintercept = 0.05, colour = "blue", lty = "dotted") +
  geom_vline(data = sumDat, aes(xintercept = pC2), colour = "red") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

pc3 <-
  ggplot(results, aes(x = pC3)) +
  geom_density() +
  geom_vline(data = sumDat, aes(xintercept = pC3), colour = "red") +
  geom_vline(xintercept = 0.05, colour = "blue", lty = "dotted") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

pc4 <-
  ggplot(results, aes(x = pC4)) +
  geom_density() +
  geom_vline(data = sumDat, aes(xintercept = pC4), colour = "red") +
  geom_vline(xintercept = 0.05, colour = "blue", lty = "dotted") +
  facet_wrap(~Habit, ncol = 5) +
  theme_bw((base_size = 14))

# Plot them all
pc1/pc2/pc3/pc4  

#------------------------------------------------------------
# Plots - converged
#------------------------------------------------------------  

con1 <-
  ggplot(results, aes(x = converge1, fill = converge1)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Habit, ncol = 5, scales = "free_y") +
  theme_bw((base_size = 14))+
  scale_fill_manual(values = c("darkgreen", "cornflowerblue")) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_x_discrete(labels = c("no", "yes")) +
  theme(legend.position = "none") +
  remove_x

con2 <-
  ggplot(results, aes(x = converge2, fill = converge2)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Habit, ncol = 5, scales = "free_y") +
  theme_bw((base_size = 14))+
  scale_fill_manual(values = c("darkgreen", "cornflowerblue")) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_x_discrete(labels = c("no", "yes")) +
  theme(legend.position = "none") +
  remove_x

con3 <-
  ggplot(results, aes(x = converge3, fill = converge3)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Habit, ncol = 5, scales = "free_y") +
  theme_bw((base_size = 14))+
  scale_fill_manual(values = c("darkgreen", "cornflowerblue")) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_x_discrete(labels = c("no", "yes")) +
  theme(legend.position = "none") +
  remove_x

con4 <-
  ggplot(results, aes(x = converge4, fill = converge4)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Habit, ncol = 5, scales = "free_y") +
  theme_bw((base_size = 14)) +
  scale_fill_manual(values = c("darkgreen", "cornflowerblue")) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_x_discrete(labels = c("no", "yes")) +
  theme(legend.position = "none") +
  remove_x

# Plot them all
con1/con2/con3/con4  

#--------------------------------------------------
# Overall results
#--------------------------------------------------
all <-
  ggplot(results, aes(x = as.character(overall), fill = as.character(overall))) +
  geom_histogram(stat = "count") +
  facet_wrap(~Habit, ncol = 5, scales = "free_y") +
  theme_bw((base_size = 14)) +
  scale_fill_manual(values = c("darkgreen", "cornflowerblue")) +
  theme(strip.background = element_rect(fill = "white")) +
  scale_x_discrete(labels = c("no", "yes")) +
  theme(legend.position = "none") +
  xlab("significantly convergent (p < 0.05)")

# Plot them all
con1/con2/con3/con4/all + plot_annotation(tag_levels = "A") 

#----------------------------------------------------
# Stacked bars
#----------------------------------------------------
stackedds <- 
  significant %>%
  pivot_longer(cols = c(yes, no), names_to = "significant", values_to = "number") %>%
  mutate(percent2 = number/N * 100) %>%
  mutate(Habit = factor(Habit, levels = c("Aquatic", "Semiaquatic", "Aquatic Burrowing", "Burrowing", "Terrestrial")))

ggplot(stackedds, aes(y = percent2, x = Habit, fill = significant)) + 
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c("darkgreen", "cornflowerblue")) +
  coord_flip() +
  theme_bw(base_size = 14) +
  ylab("significantly convergent (%)") +
  xlab("")
