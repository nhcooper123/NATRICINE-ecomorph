#-----------------------------------------------------------------------
#  ANOVAs and MANOVAs for different categories of habit and diet
#-----------------------------------------------------------------------
# Load libraries
library(tidyverse)
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

# Remove diet that are unknown *note the no caps "U"
pc_data_diet <- 
  pc_data %>%
  filter(Diet != "unknown")

# Numbers in each analyses
length(unique(pc_data$Species))
length(unique(pc_data_habit$Species))
length(unique(pc_data_diet$Species))

#-----------------------------------------------------------------------
# Manova on PC scores 1-8 (these make up 95% variance in head shape)
#-----------------------------------------------------------------------
# Habit
model1 <- manova(as.matrix(pc_data_habit[, 2:9]) ~ Habit, data = pc_data_habit)
# Look at overall model significance
anova(model1)

# Diet
model2 <- manova(as.matrix(pc_data_diet[, 2:9]) ~ Diet, data = pc_data_diet)
# Look at overall model significance
anova(model2)

# Habit & diet 
model3 <- manova(as.matrix(pc_data_diet[, 2:9]) ~ Habit*Diet, data = pc_data_diet)
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
# write_csv(output, file = "outputs/GMM/Tables/ANOVA-results-ecomorph-GMM.csv") 

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
  x <- fit.anova(pc, pc_data_diet, "Diet")
  output[i, "PC"] <- pc_list_diet[i]
  output[i, "df1"] <- x$df[1]
  output[i, "df2"] <- x$df[2]
  output[i, "F"] <- x$statistic[1]
  output[i, "p"] <- x$p.value[1]
}

# Add Bonferonni correction to p values
output$p_bonferonni <- p.adjust(output$p, method = "bonferroni")

# Write results out
# write_csv(output, file = "outputs/GMM/Tables/ANOVA-results-diet-GMM.csv") 
