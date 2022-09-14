# Phylogenetic convergence testing for linear data
# Stayton's distance-based metric
# Summaries from looping over all putative pairs of convergent taxa
# 2022
#-------------------------------------
# Load libraries
library(tidyverse)

#------------------------------------------------------------
# Import results
#------------------------------------------------------------
results <- read_csv("outputs/Linear/Tables/convevol-results-pairs.csv") 

#------------------------------------------------------------
# Summaries
#------------------------------------------------------------  

sumDat <- 
  results %>%
  group_by(Habit) %>%
  summarise(across(C1:pC4, median))
  


#------------------------------------------------------------
# Plots
#------------------------------------------------------------  

ggplot(results, aes(x = C1)) +
  geom_density() +
  facet_wrap(~Habit, ncol = 4)