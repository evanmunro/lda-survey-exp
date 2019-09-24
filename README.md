### Estimation, Figures, and Tables for "Dynamic Hierarchical Latent Variable Models for Categorical Survey Data"
#### Author: Evan Munro

This repository contains R code for the analysis behind the figures and tables for the following working paper. The code structure is still at an early stage and will be revised extensively in the near future. 

[Munro, Evan and Serena Ng. "Dynamic Hierarchical Latent Variable Models for Categorical Survey Data". 2019.](http://www.evanmunro.ca/files/munro_ng_2019.pdf)

#### 1. Install software

First, it is necessary to install the [dhlvm](www.github.com/evanmunro/dhlvm) package from Github using the R package devtools. This package was written by the author for Bayesian estimation of the two models described in the paper. The package is still in development and the installation process has only been tested on UNIX-based systems.

```
devtools::install_github("evanmunro/dhlvm")
```  

The scripts also require the following R packages, which are available on CRAN:
```
MCMCpack, xtable 
```

#### 2. Download data

The cleaned DHS data is not currently public. Raw DHS data can be downloaded from the [DHS Program](https://dhsprogram.com/data/). 

The Michigan survey response data is available for download at the [Survey of Consumers SDA Archive](https://data.sca.isr.umich.edu/sda-public/cgi-bin/hsda?harcsda+sca) and then clicking Download -> Customized Subset. Then, select all variables and download as a CSV:

The Card data is available in the data folder in the repository. 

#### 3. Model Estimation 

#### 4. Replication of Figure and Tables 

