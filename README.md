### Replication code for "Latent Dirichlet Analysis of Categorical Survey Data"
#### Author: Evan Munro and Serena Ng

This repository contains data and code for a working paper with Serena Ng.

It contains four folders and associated R files.
 - `data` contains:
    1. Raw original data and associated description files in text format for Card and Michigan data
    2. Data cleaning scripts
    3. Cleaned data used for model estimation in Rdata format
 - `estimation` Contains scripts to re-estimate the models that are analyzed in the paper
 - `posteriors` Contains saved model outputs that are used to create the tables for the paper
 - `analysis` Contains scripts to replicate all of the tables from the saved model posteriors
 - `utils.R` Contains some helper functions for common tasks in cleaning and analysis

#### Requirements

To estimate the models yourself, it is is necessary to install the R package
[dhlvm](www.github.com/evanmunro/dhlvm) from Github using `devtools`:

```
devtools::install_github("evanmunro/dhlvm")
```  

The project scripts also require the following R packages, which are available on CRAN:
```
MCMCpack, xtable
```

#### Data cleaning

Rdata files for each of the datasets are available in the
`data` folder of the repository.

The original sources of the data are as follows:

 - The raw DHS data can be downloaded from [DHS Program](https://dhsprogram.com/data/)
 - The Michigan data can be downloaded from the [Survey of Consumers SDA Archive](https://data.sca.isr.umich.edu/sda-public/cgi-bin/hsda?harcsda+sca)
 - The Card data is available on his website. The additional variables are available from the [NLSYM]().
