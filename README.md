# NATRICINE-ecomorph
Code for head shape convergence paper from V.Deepak's NATRICINE MSCA project

*This README is a work in progress...*

Author(s): V.Deepak and Natalie Cooper.

This repository contains all the code and data used in the paper [Link to final published pdf will be here]().

To cite the paper: 
> Deepak. V., Gower, D.J. & Cooper, N. 2022. Diet and habit explain head-shape convergences in natricine snakes. Journal of Evolutionary Biology. In press.

To cite this repo: 
> Deepak. V., & Cooper, N. 2022. GitHub: nhcooper123/NATRICINE-ecomorph: Code for the paper. Zenodo. DOI: 10.5281/zenodo.7323121.

[![DOI](https://zenodo.org/badge/299321129.svg)](https://zenodo.org/badge/latestdoi/299321129)

![alt text](https://github.com/nhcooper123/natricine/raw/master/outputs/Linear/Figures/PC123-ecomorph-diet-LM-LSR.png)

------

## Data

All data needed to run the analyses are in the `data/` folder. These are in two folders, one for the main analyses (Linear) and another for the supplementary GMM analyses. The full dataset and sets of trees from the paper are available on the NHM Data Portal [here]( https://doi.org/10.5519/0070625). If you use these data please cite the data portal:

> Deepak V, Cooper N, Gower DJ. 2020. Dataset: NATRICINE. Natural History Museum Data Portal (data.nhm.ac.uk). https://doi.org/10.5519/0070625.

------
## Analyses
All code used to run analyses and make figures is included in the `analyses/` folder. Before starting remember to open an RStudio project from that folder. Most code was written by V Deepak, and modified by Natalie Cooper.

### Running the analyses 
The main analyses from the paper are in the following scripts within the `analyses/Linear` folder.

-  *00-add-missing-tips-to-tree.R*. This takes the dated tree and adds additional taxa to it to create the tree (`new_datedtree-LM.nexus`) used in the analyses.
-  *01-extract-species-means.R*. This takes the raw specimen level data, size corrects them using log shape ratios, and then extracts species mean values.     
-  *02-pca-linear-measurements.R*. This runs a PCA on the species level head shape variables, to produce the data (`snakepca.csv`) used in all analyses.
-  *03-pca-figures-linear.R*. This creates the PC figures for the appendix.
-  *04-MANOVA-linear.R*. This performs the MANOVAs of head shape versus putative ecomorph and diet.
-  *05-convergence-linear.R*. This looks for head shape convergence in putative ecomorphs using C1-C4 metrics. Note that this code takes a very long time to run, i.e. days-weeks for some groups. Original code from v1 of the paper. In the published version thsi is replaced by code in 5A, 5B and 5C.
-  *05A-habit-ace.R*. This estimates ancestral states for habit/putative ecomorph across the phylogeny, for use in script 5B.
-  *05B-convergence-linear-pairs.R*. This looks for head shape convergence in putative ecomorphs using C1-C4 metrics. Metrics are calculated for each pair of taxa that have independently evolved the same putative ecomorph. Takes around a week to run.
-  *05C-convergence-results.R*. Summarises the results outputs from 5B.
-  *06-pca-tree-figures.R*. This creates the PCs and phylogeny plots, and phylomorphospace plots for the paper.
-  *07-discrete-trait-models.R*. This estimates the best model of trait evolution for putative ecomorph.
-  *08-mvSLOUCH-models.R*. This fits the more complex evolutionary models.

We also repeated these analyses using landmarks that represent the head shape of the snakes. These analyses are found in the `analyses/GMM` folder.

-------
## Other folders

* `/outputs` contains the figures and tables. These are in two folders, one for the main analyses (Linear) and another for the supplementary GMM analyses.

-------
## Session Info
For reproducibility purposes, here is the output of `devtools::session_info()` used to perform the analyses in the publication.

    ─ Session info ──────────────────────────────────────────
    setting  value
    version  R version 4.2.0 (2022-04-22)
    os       macOS Big Sur 11.4
    system   x86_64, darwin17.0
    ui       RStudio
    language (EN)
    collate  en_IE.UTF-8
    ctype    en_IE.UTF-8
    tz       Europe/Dublin
    date     2022-11-15
    rstudio  2022.07.2+576 Spotted Wakerobin (desktop)
    pandoc   1.19.1 @ /usr/local/bin/pandoc

    ─ Packages ──────────────────────────────────────────────
    package           * version    date (UTC) lib source
    abind             * 1.4-5      2016-07-21 [1] CRAN (R 4.2.0)
    ape               * 5.6-2      2022-03-02 [1] CRAN (R 4.2.0)
    aplot               0.1.6      2022-06-03 [1] CRAN (R 4.2.0)
    assertthat          0.2.1      2019-03-21 [1] CRAN (R 4.2.0)
    backports           1.4.1      2021-12-13 [1] CRAN (R 4.2.0)
    base64enc           0.1-3      2015-07-28 [1] CRAN (R 4.2.0)
    bit                 4.0.4      2020-08-04 [1] CRAN (R 4.2.0)
    bit64               4.0.5      2020-08-30 [1] CRAN (R 4.2.0)
    broom               0.8.0      2022-04-13 [1] CRAN (R 4.2.0)
    cachem              1.0.6      2021-08-19 [1] CRAN (R 4.2.0)
    callr               3.7.0      2021-04-20 [1] CRAN (R 4.2.0)
    cellranger          1.1.0      2016-07-27 [1] CRAN (R 4.2.0)
    cli                 3.3.0      2022-04-25 [1] CRAN (R 4.2.0)
    cluster           * 2.1.3      2022-03-28 [1] CRAN (R 4.2.0)
    clusterGeneration   1.3.7      2020-12-15 [1] CRAN (R 4.2.0)
    coda                0.19-4     2020-09-30 [1] CRAN (R 4.2.0)
    codetools           0.2-18     2020-11-04 [1] CRAN (R 4.2.0)
    colorspace          2.0-3      2022-02-21 [1] CRAN (R 4.2.0)
    combinat            0.0-8      2012-10-29 [1] CRAN (R 4.2.0)
    convevol          * 1.3        2018-11-04 [1] CRAN (R 4.2.0)
    corpcor           * 1.6.10     2021-09-16 [1] CRAN (R 4.2.0)
    cowplot           * 1.1.1      2020-12-30 [1] CRAN (R 4.2.0)
    crayon              1.5.1      2022-03-26 [1] CRAN (R 4.2.0)
    data.table          1.14.2     2021-09-27 [1] CRAN (R 4.2.0)
    DBI                 1.1.3      2022-06-18 [1] CRAN (R 4.2.0)
    dbplyr              2.2.0      2022-06-05 [1] CRAN (R 4.2.0)
    deSolve             1.32       2022-04-14 [1] CRAN (R 4.2.0)
    devtools          * 2.4.4      2022-07-20 [1] CRAN (R 4.2.0)
    digest              0.6.29     2021-12-01 [1] CRAN (R 4.2.0)
    dotCall64           1.0-1      2021-02-11 [1] CRAN (R 4.2.0)
    dplyr             * 1.0.9      2022-04-28 [1] CRAN (R 4.2.0)
    ellipsis            0.3.2      2021-04-29 [1] CRAN (R 4.2.0)
    expm                0.999-6    2021-01-13 [1] CRAN (R 4.2.0)
    fansi               1.0.3      2022-03-24 [1] CRAN (R 4.2.0)
    fastmap             1.1.0      2021-01-25 [1] CRAN (R 4.2.0)
    fastmatch           1.1-3      2021-07-23 [1] CRAN (R 4.2.0)
    forcats           * 0.5.1      2021-01-27 [1] CRAN (R 4.2.0)
    fs                  1.5.2      2021-12-08 [1] CRAN (R 4.2.0)
    geiger            * 2.0.10     2022-06-03 [1] CRAN (R 4.2.0)
    generics            0.1.3      2022-07-05 [1] CRAN (R 4.2.0)
    geomorph          * 4.0.3      2022-03-02 [1] CRAN (R 4.2.0)
    ggfun               0.0.6      2022-04-01 [1] CRAN (R 4.2.0)
    ggplot2           * 3.3.6      2022-05-03 [1] CRAN (R 4.2.0)
    ggplotify           0.1.0      2021-09-02 [1] CRAN (R 4.2.0)
    ggtree            * 3.4.0      2022-04-26 [1] Bioconductor
    glassoFast          1.0        2018-07-30 [1] CRAN (R 4.2.0)
    glue                1.6.2      2022-02-24 [1] CRAN (R 4.2.0)
    gridGraphics        0.5-1      2020-12-13 [1] CRAN (R 4.2.0)
    gtable              0.3.0      2019-03-25 [1] CRAN (R 4.2.0)
    haven               2.5.0      2022-04-15 [1] CRAN (R 4.2.0)
    here              * 1.0.1      2020-12-13 [1] CRAN (R 4.2.0)
    hms                 1.1.1      2021-09-26 [1] CRAN (R 4.2.0)
    htmltools           0.5.2      2021-08-25 [1] CRAN (R 4.2.0)
    htmlwidgets         1.5.4      2021-09-08 [1] CRAN (R 4.2.0)
    httpuv              1.6.5      2022-01-05 [1] CRAN (R 4.2.0)
    httr                1.4.3      2022-05-04 [1] CRAN (R 4.2.0)
    igraph              1.3.2      2022-06-13 [1] CRAN (R 4.2.0)
    jpeg                0.1-9      2021-07-24 [1] CRAN (R 4.2.0)
    jsonlite            1.8.0      2022-02-22 [1] CRAN (R 4.2.0)
    knitr               1.39       2022-04-26 [1] CRAN (R 4.2.0)
    later               1.3.0      2021-08-18 [1] CRAN (R 4.2.0)
    lattice             0.20-45    2021-09-22 [1] CRAN (R 4.2.0)
    lazyeval            0.2.2      2019-03-15 [1] CRAN (R 4.2.0)
    lifecycle           1.0.1      2021-09-24 [1] CRAN (R 4.2.0)
    lubridate           1.8.0      2021-10-07 [1] CRAN (R 4.2.0)
    magrittr            2.0.3      2022-03-30 [1] CRAN (R 4.2.0)
    maps              * 3.4.0      2021-09-25 [1] CRAN (R 4.2.0)
    MASS              * 7.3-56     2022-03-23 [1] CRAN (R 4.2.0)
    Matrix            * 1.4-1      2022-03-23 [1] CRAN (R 4.2.0)
    matrixcalc          1.0-5      2021-07-28 [1] CRAN (R 4.2.0)
    memoise             2.0.1      2021-11-26 [1] CRAN (R 4.2.0)
    mime                0.12       2021-09-28 [1] CRAN (R 4.2.0)
    miniUI              0.1.1.1    2018-05-18 [1] CRAN (R 4.2.0)
    mnormt              2.1.0      2022-06-07 [1] CRAN (R 4.2.0)
    modelr              0.1.8      2020-05-19 [1] CRAN (R 4.2.0)
    munsell             0.5.0      2018-06-12 [1] CRAN (R 4.2.0)
    mvMORPH           * 1.1.6      2022-05-09 [1] CRAN (R 4.2.0)
    mvSLOUCH          * 2.7.4      2022-05-04 [1] CRAN (R 4.2.0)
    mvtnorm             1.1-3      2021-10-08 [1] CRAN (R 4.2.0)
    nlme                3.1-157    2022-03-25 [1] CRAN (R 4.2.0)
    numDeriv            2016.8-1.1 2019-06-06 [1] CRAN (R 4.2.0)
    ouch                2.18       2022-05-16 [1] CRAN (R 4.2.0)
    patchwork         * 1.1.1      2020-12-17 [1] CRAN (R 4.2.0)
    pbmcapply           1.5.1      2022-04-28 [1] CRAN (R 4.2.0)
    PCMBase             1.2.12     2021-06-07 [1] CRAN (R 4.2.0)
    PCMBaseCpp        * 0.1.9      2020-03-23 [1] CRAN (R 4.2.0)
    phangorn            2.9.0      2022-06-16 [1] CRAN (R 4.2.0)
    phytools          * 1.0-3      2022-04-05 [1] CRAN (R 4.2.0)
    pillar              1.8.0      2022-07-18 [1] CRAN (R 4.2.0)
    pkgbuild            1.3.1      2021-12-20 [1] CRAN (R 4.2.0)
    pkgconfig           2.0.3      2019-09-22 [1] CRAN (R 4.2.0)
    pkgload             1.3.0      2022-06-27 [1] CRAN (R 4.2.0)
    plotrix             3.8-2      2021-09-08 [1] CRAN (R 4.2.0)
    plyr                1.8.7      2022-03-24 [1] CRAN (R 4.2.0)
    prettyunits         1.1.1      2020-01-24 [1] CRAN (R 4.2.0)
    processx            3.6.1      2022-06-17 [1] CRAN (R 4.2.0)
    profvis             0.3.7      2020-11-02 [1] CRAN (R 4.2.0)
    promises            1.2.0.1    2021-02-11 [1] CRAN (R 4.2.0)
    ps                  1.7.1      2022-06-18 [1] CRAN (R 4.2.0)
    psych             * 2.2.5      2022-05-10 [1] CRAN (R 4.2.0)
    purrr             * 0.3.4      2020-04-17 [1] CRAN (R 4.2.0)
    quadprog            1.5-8      2019-11-20 [1] CRAN (R 4.2.0)
    R6                  2.5.1      2021-08-19 [1] CRAN (R 4.2.0)
    Rcpp              * 1.0.9      2022-07-08 [1] CRAN (R 4.2.0)
    readr             * 2.1.2      2022-01-30 [1] CRAN (R 4.2.0)
    readxl              1.4.0      2022-03-28 [1] CRAN (R 4.2.0)
    remotes             2.4.2      2021-11-30 [1] CRAN (R 4.2.0)
    reprex              2.0.1      2021-08-05 [1] CRAN (R 4.2.0)
    reshape2          * 1.4.4      2020-04-09 [1] CRAN (R 4.2.0)
    rgl               * 0.109.2    2022-06-10 [1] CRAN (R 4.2.0)
    rlang               1.0.4      2022-07-12 [1] CRAN (R 4.2.0)
    rprojroot           2.0.3      2022-04-02 [1] CRAN (R 4.2.0)
    RRPP              * 1.3.0      2022-06-21 [1] CRAN (R 4.2.0)
    rstudioapi          0.13       2020-11-12 [1] CRAN (R 4.2.0)
    rvest               1.0.2      2021-10-16 [1] CRAN (R 4.2.0)
    scales              1.2.0      2022-04-13 [1] CRAN (R 4.2.0)
    scatterplot3d       0.3-41     2018-03-14 [1] CRAN (R 4.2.0)
    sessioninfo         1.2.2      2021-12-06 [1] CRAN (R 4.2.0)
    shiny               1.7.2      2022-07-19 [1] CRAN (R 4.2.0)
    spam                2.8-0      2022-01-06 [1] CRAN (R 4.2.0)
    stringi             1.7.6      2021-11-29 [1] CRAN (R 4.2.0)
    stringr           * 1.4.0      2019-02-10 [1] CRAN (R 4.2.0)
    subplex           * 1.8        2022-04-12 [1] CRAN (R 4.2.0)
    tibble            * 3.1.8      2022-07-22 [1] CRAN (R 4.2.0)
    tidyr             * 1.2.0      2022-02-01 [1] CRAN (R 4.2.0)
    tidyselect          1.1.2      2022-02-21 [1] CRAN (R 4.2.0)
    tidytree            0.3.9      2022-03-04 [1] CRAN (R 4.2.0)
    tidyverse         * 1.3.1      2021-04-15 [1] CRAN (R 4.2.0)
    treeio              1.20.0     2022-04-26 [1] Bioconductor
    tzdb                0.3.0      2022-03-28 [1] CRAN (R 4.2.0)
    urlchecker          1.0.1      2021-11-30 [1] CRAN (R 4.2.0)
    usethis           * 2.1.6      2022-05-25 [1] CRAN (R 4.2.0)
    utf8                1.2.2      2021-07-24 [1] CRAN (R 4.2.0)
    vctrs               0.4.1      2022-04-13 [1] CRAN (R 4.2.0)
    vroom               1.5.7      2021-11-30 [1] CRAN (R 4.2.0)
    withr               2.5.0      2022-03-03 [1] CRAN (R 4.2.0)
    xfun                0.31       2022-05-10 [1] CRAN (R 4.2.0)
    xml2                1.3.3      2021-11-30 [1] CRAN (R 4.2.0)
    xtable              1.8-4      2019-04-21 [1] CRAN (R 4.2.0)
    yulab.utils         0.0.4      2021-10-09 [1] CRAN (R 4.2.0)

    [1] /Library/Frameworks/R.framework/Versions/4.2/Resources/library

-------
## Checkpoint for reproducibility
To rerun all the code with packages as they existed on CRAN at time of our analyses we recommend using the `checkpoint` package, and running this code prior to the analysis:

```{r}
checkpoint("2022-10-08")
```
