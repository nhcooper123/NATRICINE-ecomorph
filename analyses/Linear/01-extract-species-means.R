# Title: Head shape variation in natricine snakes correlates with habit and diet
# V. Deepak & Natalie Cooper, 2021 
# Extract species means for natricine linear morphology dataset

#---------------------------
# Load libraries
#---------------------------
library(tidyverse)

#-----------------------------------------------------------
# Import dataset
#-----------------------------------------------------------
# Import the data (with labels in first row). 
# Includes 1346 individuals 206 taxa 
ds <- read_csv("data/Linear/metadata-data-natricine-LM.csv")

#----------------------------------------------
# Get log10 values of variables
# Regress against SVL to reduce size influence 
# and focus on shape, and extract residuals
# Take species means of residuals
#----------------------------------------------
snake <- 
  ds %>%
  # Take all variables from JL to TL and log transform
  # But exclude DSR
  # This creates the new variables JL_log10 to TL_log10
  # vars tells it what variables to use
  # inside the list is the function, and what we'd like it to call the new variables
  mutate_at(vars(JL:TL, -DSR), list(log10 = log10)) %>%
  # Now get residuals from regressions for each _log10 variable (excluding SVL itself)
  # And also DSR which we did not log10 transform
  # again vars tells it what variables to use
  # list contains the function, and we will call the outputs _res
  # the ~ tells it we are going to use a function, and will replace the . with each
  # variable in turn.
  mutate_at(vars(JL_log10:TL_log10, DSR, -SVL_log10), 
            list(res = ~residuals(lm(. ~ SVL_log10, data = ds, 
                                     na.action = na.exclude)))) %>%
  # Finally let's get the species means using summarise
  # we will group by the other variables we want to keep in the dataset
  group_by(Species,Habit,Diet, Origin, Lineage) %>%
  # Then get the means of the residuals columns
  summarise_at(vars(JL_log10_res:DSR_res), mean, na.rm = TRUE)

#----------------------------------
# Export the file (using write_csv)
#----------------------------------
# write_csv(snake, file = "data/Linear/linear-species-means.csv")
