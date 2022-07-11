# Title: Head shape variation in natricine snakes correlates with habit and diet
# V. Deepak & Natalie Cooper, 2021 
# Extract species means for natricine linear morphology dataset
# Modified 2022

#---------------------------
# Load libraries
#---------------------------
library(tidyverse)
library(psych)
#-----------------------------------------------------------
# Import dataset
#-----------------------------------------------------------
# Import the data (with labels in first row). 
# Includes 1346 individuals 206 taxa 
ds <- read_csv("data/Linear/metadata-data-natricine-LM.csv")

#----------------------------------------------
# Get log10 shape ratios (LSR) for each specimen
# Take species means of LSR
#----------------------------------------------
snake <- 
  ds %>%
  # Next get the geometric mean of variables JL:EE only, as these
  # are the variables used in the PCA
  # need to use rowwise as we want geometric means per row
  rowwise() %>% 
  mutate(geo_mean = geometric.mean(c(JL, HLRA, HLV, HWLSL, HWFSL, HW, 
                                     HHP, HH, ES, EN, ED, NL, EL, NN, EE))) %>%
  # Now get log shape ratios by dividing each variable JL:EE by the geometric mean,
  # and logging the ratio.
  # vars tells it what variables to use
  # list contains the function, and we will call the outputs _LSR (log shape ratio)
  # the ~ tells it we are going to use a function, and will replace the . with each
  # variable in turn.
  mutate_at(vars(JL:EE), list(LSR = ~ log10( . / geo_mean))) %>%
  # Finally let's get the species means using summarise
  # we will group by the other variables we want to keep in the dataset
  group_by(Species, Habit, Diet, Origin, Lineage) %>%
  # Then get the means of the residuals columns
  summarise_at(vars(JL_LSR:EE_LSR), mean, na.rm = TRUE)

#----------------------------------
# Export the file (using write_csv)
#----------------------------------
# write_csv(snake, file = "data/Linear/linear-species-means-LSR.csv")
