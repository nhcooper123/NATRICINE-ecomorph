#-----------------------------------------------------------------------
#  ANOVAs and MANOVAs for different categories of habit, origin and diet
#-----------------------------------------------------------------------
# Load libraries
library(geomorph)
library(tidyverse)
library(ape)
library(geiger)
library(broom)

# Fitting ANOVAs function
fit.anova <- function(pc, data, grouping.var) {
  pc_ <- pull(data, pc)
  grouping.var_ <- pull(data, grouping.var)
  model <- lm(pc_ ~ grouping.var_, data = data)
  out <- tidy(anova(model))
  return(out)
}
#-----------------------------------------------------------------------
# Read in data and wrangle it
#-----------------------------------------------------------------------
pc_data <- read_csv("data/GMM/snake-data-pca-GMM.csv")

# Remove habits that are unknown
pc_data_habit <- 
  pc_data %>%
  filter(Habit != "Unknown")

# Remove origin that are unknown
pc_data_origin <- 
  pc_data %>%
  filter(Origin != "Unknown")

# Remove diet that are unknown *note the no caps "U"
pc_data_diet <- 
  pc_data %>%
  filter(Diet != "unknown")

#-----------------------------------------------------------------------
# Manova on PC scores 1-8 (these make up 95% variance in head shape)
#-----------------------------------------------------------------------
# Habit & Genera
model1 <- manova(as.matrix(pc_data_habit[, 2:9]) ~ Habit*Genera, data = pc_data_habit)
# Look at overall model significance
anova(model1)

# Habit & Origin
model2 <- manova(as.matrix(pc_data_habit[, 2:9]) ~ Habit*Origin, data = pc_data_habit)
# Look at overall model significance
anova(model2)

# Habit & diet 
#diet data is not complete unknown categories to be removed before analysis
model3 <- manova(as.matrix(pc_data_habit[, 2:9]) ~ Habit*Diet, data = pc_data_habit)
# Look at overall model significance
anova(model3)

#-----------------------------------------------------------------------
# ANOVAs on individual PCs
#-----------------------------------------------------------------------
# Example for one model
# Fit model
modela <- lm(PC1 ~ Habit, data = pc_data_habit)
# Check diagnostics
par(mfrow = c(2,2))
plot(modela)
par(mfrow = c(1,1))
# Look at overall result, i.e. is PC1 correlated with Habit?
anova(modela)
# Look at differences across Habit groups
summary(modela)

#--------------------------------------------------------------------------
# ANOVAs for each individual PC can be automated with the code below
#--------------------------------------------------------------------------
# List names of first 8 PCs
pc_list_habit <- names(pc_data_habit)[2:9]

# Create an output file
output <- data.frame(array(dim = c(8, 5)))
names(output) <- c("PC", "df1", "df2", "F", "p")

# Run ANOVAs
for (i in seq_along(pc_list_habit)){
  pc <- pc_list_habit[i]
  x <- fit.anova(pc, pc_data_habit, "Habit")
  output[i, "PC"] <- pc_list_habit[i]
  output[i, "df1"] <- x$df[1]
  output[i, "df2"] <- x$df[2]
  output[i, "F"] <- x$statistic[1]
  output[i, "p"] <- x$p.value[1]
}

# Add Bonferonni correction to p values
output$p_bonferonni <- p.adjust(output$p, method = "bonferroni")

# Write results out
write_csv(output, path = "outputs/GMM/ANOVA-results-habit-GMM.csv") 

#--------------------------------------------
# Fit ANOVAs for each individual PC origin 
#--------------------------------------------
# List names of first 8 PCs
pc_list_Origin <- names(pc_data_origin)[2:9]

# Create an output file
output <- data.frame(array(dim = c(8, 5)))
names(output) <- c("PC", "df1", "df2", "F", "p")

# Run ANOVAs
for (i in seq_along(pc_list_Origin)){
  pc <- pc_list_Origin[i]
  x <- fit.anova(pc, pc_data_origin, "Origin")
  output[i, "PC"] <- pc_list_Origin[i]
  output[i, "df1"] <- x$df[1]
  output[i, "df2"] <- x$df[2]
  output[i, "F"] <- x$statistic[1]
  output[i, "p"] <- x$p.value[1]
}

# Add Bonferonni correction to p values
output$p_bonferonni <- p.adjust(output$p, method = "bonferroni")

# Write results out
write_csv(output, path = "outputs/GMM/ANOVA-results-origin-GMM.csv") 

#--------------------------------------------
# Fit ANOVAs for each individual PC diet 
#--------------------------------------------
# List names of first 8 PCs
pc_list_diet <- names(pc_data_diet)[2:9]

# Create an output file
output <- data.frame(array(dim = c(8, 5)))
names(output) <- c("PC", "df1", "df2", "F", "p")

# Run ANOVAs
for (i in seq_along(pc_list_diet)){
  pc <- pc_list_diet[i]
  x <- fit.anova(pc, pc_data_diet, "Origin")
  output[i, "PC"] <- pc_list_diet[i]
  output[i, "df1"] <- x$df[1]
  output[i, "df2"] <- x$df[2]
  output[i, "F"] <- x$statistic[1]
  output[i, "p"] <- x$p.value[1]
}

# Add Bonferonni correction to p values
output$p_bonferonni <- p.adjust(output$p, method = "bonferroni")

# Write results out
write_csv(output, path = "outputs/GMM/ANOVA-results-diet-GMM.csv") 
