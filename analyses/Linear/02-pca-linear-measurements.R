# Title: Head shape variation in natricine snakes correlates with habit and diet
# V. Deepak & Natalie Cooper, 2021 
# PCA analyses using natricine linear morphology dataset

#-------------------------------
# Load libraries and functions
#--------------------------------
library(geomorph)
library(tidyverse)
library(reshape2)
library(cowplot)
#-------------------------------------------------------------
# Import the species mean data
#------------------------------------------------------------
snake <- read.csv("data/Linear/linear-species-means.csv")

# Remove last three variables which we are not using in this analysis
snake <- snake[, -c(23:26)] 

# Look at the data
glimpse(snake)

#----------------------------------
# PCA analysis
#----------------------------------
# PCA using R function prcomp() 
# scale. = TRUE is highly advisable, but default is FALSE.
# use select to choose correct columns and remove TL
rlog.pca <- prcomp(select(snake, JL_log10_res:EE_log10_res, -BWH_log10_res, -BWM_log10_res), 
                   center = TRUE, scale. = TRUE)

# Combine with the rest of the snake data
snakepca <- cbind(snake, rlog.pca$x)

# Look at the cumulative proportion of variance explained
summary(rlog.pca)
plot(rlog.pca)
rlog.pca

# Export the PC scores
# write_csv(snakepca, path = "data/Linear/snakepca.csv")

#----------------------------------
# PCA loadings plots
#----------------------------------
# Reshape data for plotting
plotdf <- 
  melt((rlog.pca$rotation[, 1:9]), value.name = "Values", variable.name = "PC", 
       na.rm = TRUE)

# PC loadings for each PC stacked one on top of the other
# Up to 95% cumulative variance explained
ggplot(plotdf, aes(x = Var1, y = Values)) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_grid(Var2 ~ ., scales = "free") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5, col = "grey")

# Save plot
#ggsave("outputs/Linear/Figures/PC-loadings.png", height = 10)