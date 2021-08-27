# Title: Head shape variation in natricine snakes correlates with habit and diet
# V. Deepak & Natalie Cooper, 2021 
# Add missing species to the tree

#--------------------------
# Load packages
#--------------------------
library(ape)
library(geiger)
library(phytools)
library(tidyverse)
#--------------------------
# Load functions
#--------------------------
# INPUTS
# tree = name of tree
# species.to.add = name of species to add
# closest.relative = name of closest relative
# branch.length = branch length separating the relative and species
# data = dataset of species that are not in the tree

# Identify nodes to add a tip to
IdentifyNode <- function(tree, closest.relative){
  which(tree$tip.label == closest.relative) 
} 

# Add species to tree using bind.tree after identifying the node to add them to
AddSpeciesToTree <- function(tree, species.to.add, closest.relative, branch.length){
  add.to.node <- IdentifyNode(tree, closest.relative)
  tree.to.add <- paste("(", species.to.add, ":", branch.length, ");", sep="")
  tree.to.add <- read.tree(text = tree.to.add) 
  tree <- bind.tree(tree, tree.to.add, where = add.to.node, position = branch.length)
  return(tree)
}

AddAllSpeciesToTree <- function(tree, data, species.col, relative.col, branch.col){
  # Identify missing species column number
  species.col.no <- which(names(data) == species.col)
  relative.col.no <- which(names(data) == relative.col)
  branch.col.no <- which(names(data) == branch.col)
    # Loop through each species in turn
    for(i in seq_along(data[, species.col.no])){
      species.to.add <- data[i, species.col.no]
      closest.relative <- data[i, relative.col.no]
      branch.length <- data[i, branch.col.no]
      try(tree <- AddSpeciesToTree(tree, species.to.add, closest.relative, branch.length), silent = TRUE)
    }
  return(tree)
}

##Tips## If there are duplicate names but different lineages to retain
## change the name with a suffix 'eg. _1' in the nexus file and then read back in
#-----------------------------------------------
# Read in tree and fix labels
#-----------------------------------------------
tree <- read.nexus("data/Linear/datedtree.nexus")

# Fix up species names
# 1. Remove the seq_ tag from the start of the name
tree$tip.label <- str_remove(tree$tip.label, "seq_[:alpha:]+_\\d+_")
# 2. Remove the punctuation in some of the names
tree$tip.label <- str_remove_all(tree$tip.label, "[\']")

#-----------------------------------------------------------------------------#
# Natricine tree to add many additional species which are missing in the tree
#-----------------------------------------------------------------------------#
# Read in phylogeny in this case tree_new

# Read in data to add
# This has species names ("binomial"), the tip to add the 
# species to ("relative") and the branch length ("branch")
data.to.add <- read.csv("data/missing_taxa_data.csv") 

# Add missing species to the tree
new_tree<- AddAllSpeciesToTree(tree, data.to.add, "binomial", 
                                     "relative", "branch")

# This will not work if branch lengths are shorter than the divergence
# branch lengths that you have given the new species. Also if you need 
# to add fossils to other fossil branches you may need to do this a 
# bit at a time rather than as a function that adds them all at once

#----------------------------------
# Export the tree file (write.nexus)
#----------------------------------
# write.nexus(new_tree, file = "data/Linear/new_datedtree-LM.nexus")
