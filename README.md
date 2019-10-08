### Replication code for "Latent Dirichlet Analysis of Categorical Survey Data"
#### Author: Evan Munro and Serena Ng

This repository contains data and code for a working paper with Serena Ng.

It contains three folders:  
 - `data` contains:
    1. Raw data and associated description files in text format for Michigan data
    2. Cleaned DHS data in RData format (data was originally obtained in cleaned form)
    3. Recoded Card data in RData format and description files in text format
    4. README with sources and data guide
 - `posteriors` Contains saved model posterior means that are used to create the tables for the paper
 - `simulations` Contains `mcmc_simulations.R` for the simulations recorded in Table 1 in the paper

And some scripts:
- `utils.R` contain utility functions for plotting data
- `card_analysis.R`, `mich_analysis.R`, and `dhs_analysis.R` produce tables and figures
for the application section of the paper, either from the saved model posteriors or from
a newly estimated model. Set `re-estimate=T` in order to re-estimate the models (can be
computationally intensive).

#### Requirements

It is is necessary to install the R package [dhlvm](www.github.com/evanmunro/dhlvm) for the models in the paper from Github using `devtools`:

```
devtools::install_github("evanmunro/dhlvm")
```  

The scripts also require the following R packages, which are available on CRAN:
```
MCMCpack, reshape2, ggplot2
```
