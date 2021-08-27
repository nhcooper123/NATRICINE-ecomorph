# NATRICINE-ecomorph
Code for head shape convergence paper from V.Deepak's NATRICINE MSCA project

*This README is a work in progress...*

Author(s): V.Deepak and Natalie Cooper.

This repository contains all the code and data used in the manuscript [Link to final published pdf will be here]().

To cite the paper: 
> 

To cite this repo: 
> 


![alt text](https://github.com/nhcooper123/natricine/raw/master/outputs/Linear/Figures/PCA-ecomorph-diet-trees.png)

------

## Data

All data needed to run the analyses are in the `data/` folder. These are in two folders, one for the main analyses (Linear) and another for the supplementary GMM analyses. The full dataset and sets of trees from the paper are available on the NHM Data Portal [here]( https://doi.org/10.5519/0070625). If you use these data please cite the data portal:

> Deepak V, Cooper N, Gower DJ. 2020. Dataset: NATRICINE. Natural History Museum Data Portal (data.nhm.ac.uk). https://doi.org/10.5519/0070625.

------
## Analyses
All code used to run analyses and make figures is included in the `analyses/` folder. Before starting remember to open an RStudio project from that folder. Most code was written by V Deepak, and tidied by Natalie Cooper.

### Running the analyses 
The main analyses are in the following scripts within the `analyses/Linear` folder.

-  *00-add-missing-tips-to-tree.R*. This takes the dated tree and adds additional taxa to it to create the tree (`new_datedtree-LM.nexus`) used in the analyses.
-  *01-extract-species-means.R*. This takes the raw specimen level data, size corrects them by regressing each against log10 body size and extracting the residuals, and then extracts species mean values.     
-  *02-pca-linear-measurements.R*. This runs a PCA on the species level head shape variables, to produce the data (`snakepca.csv`) used in all analyses.
-  *03-figures-linear.R*. This creates the PC figures for the appendix.
-  *04-ANOVA-MANOVA-GMM.R*. This performs the MANOVAs and ANOVAs of head shape versu ecomorph and diet.
-  *05-convergence-linear.R*. This looks for head shape convergence in ecomorphs using C1-C4 metrics.         
-  *06-pca-tree-figures.R*. This creates the PCs and phylogeny plot for the paper.
-  *07-discrete-trait-models.R*. This estimates the best model of trait evolution for ecomorph for use in script 08.
-  *08-OUwie-models.R*. This fits the more complex evolutionary models.

We also repeated several analyses using landmarks that represent the head shape of the snakes. These analyses are found in the `analyses/GMM` folder.

-------
## Other folders

* `/outputs` contains the figures and tables. These are in two folders, one for the main analyses (Linear) and another for the supplementary GMM analyses.
