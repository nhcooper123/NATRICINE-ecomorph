#-----------------------------------------------------------------------
#  2D geometric morphometric analysis 
#  Ecomorphology landmarks 1-9 and 28 semi-landmarks for 240 individuals
#-----------------------------------------------------------------------
# Load libraries
library(geomorph)
library(tidyverse)
library(ape)
library(geiger)

# Read in TPS data 
skull <- readland.tps(file = "data/GMM/natricine_landmarks.TPS",
                      specID = "imageID",
                      readcurves = TRUE,
                      warnmsg = TRUE)

## Delete semilandmarks which overlap with the landmarks
# make a vector of the landmark numbers to delete
delete = c(10, 11, 12, 13, 14, 29, 30, 45) 
# delete selected landmarks
skull.new <- skull[-delete,,] 

# Look at landmarks
plot(skull.new[,,1])
text(skull.new[,,1], rownames(skull.new[,,1]), adj = c(0,2), cex = 0.6)

# Read in curves
curves <- as.matrix(read.csv("data/GMM/sliders-natricine-landmarks.csv", header = TRUE))

# GPA
gpa_specimens <- gpagen(skull.new, curves, ProcD = TRUE)

# Read in info on genera, species, habit, diet, etc.
info <- read.csv("data/GMM/metadata-natricine.csv", header=TRUE, row.names=1)

# Sort dataset by the order of specimens in the GPA
info <- info[match(dimnames(gpa_specimens$coords)[[3]], info$image.ID), ]

#--------------------------------------------------------------
# Group GPA coordinates by species and get species mean shapes
#--------------------------------------------------------------
# Create a 2D array that aggregate can work on
two.d.coords <- two.d.array(gpa_specimens$coords)

# Aggregate by species
species.means <- aggregate(two.d.coords ~ info$Species, FUN = mean)

#------------------------------------------------------------------------------------#
# Match up the data and the phylogeny
#------------------------------------------------------------------------------------#
# Read in tree file (Note that this is sorted for 2D dataset. This is different
# from the tree used in linear data)
tree <- read.nexus("data/GMM/new_datedtree.nexus")

# Check names match
match.species <- name.check(phy = tree, data = species.means, 
                            data.names = species.means$`info$Species`) 
match.species

# Drop tips not found in the data
tree_new <- drop.tip(tree, match.species$tree_not_data)

# Look at tree
plot(tree_new)

# Drop species in the data not found in tree
fix <- match(species.means$`info$Species`, match.species$data_not_tree, nomatch = 0)
species.means <- subset(species.means, fix == 0)

# Add species names as rownames
rownames(species.means) <- species.means$`info$Species`

# Delete species column
species.means <- species.means[, -1]

#-----------------------------------------------------------
# PCA
#-----------------------------------------------------------
# Make species.means into a 3D array again
mean.coords <- arrayspecs(species.means, dim(gpa_specimens$coords)[1], dim(gpa_specimens$coords)[2])

# Sort dataset by the order of species in the species means array
mydata <- info[match(dimnames(mean.coords)[[3]], info$Species), ]

# Put it all together for further analyses
species.data <- list(coords = mean.coords,
                     species = mydata$Species,
                     habit = mydata$Habit)

# GPA
gpa_species <- gpagen(mean.coords, ProcD = TRUE)

# PCA 
pca.landmarks <- gm.prcomp(gpa_species$coords)

# How many PCs to include up to 95%?
summary(pca.landmarks)

#-----------------------------------------------------------
# Merge PC scores and metadata
#-----------------------------------------------------------
# Extract PC scores and make .pts file name into
# a taxon column for combining with the metadata
pc_scores <- data.frame(Species = rownames(pca.landmarks$x), 
                        pca.landmarks$x)

# Make column names begin with PC not Comp
colnames(pc_scores) <- gsub("Comp", "PC", colnames(pc_scores))

# Remove the un-needed columns
# (note sometimes the select function shows an error so fixed with prefix dplyr::)
info2 <-
  mydata %>%
  dplyr::select(-Specimen.no, -image.ID) %>%
  distinct()

# Merge with metadata
pc_data <- full_join(pc_scores, info2, by = "Species")

# Write to file
write_csv(pc_data, path = "data/GMM/snake-data-pca-GMM.csv") 

#----------------------------------------------------------------------------------#
# Estimate phylogenetic signal in GPA
#----------------------------------------------------------------------------------#
## Testing for phylogenetic signal
#To do this we will use the Kmult statistic from Adams (2014), which is a multivariate 
#version of the the K-statistic (Blomberg et al 2003) for high-dimensional multivariate data. 

physignal(A = mean.coords, tree_new, iter = 999)
