### Replication code for "Latent Dirichlet Analysis of Categorical Survey Data"
#### Author: Evan Munro and Serena Ng

This repository contains data and code for a working paper with Serena Ng.

It contains two data folders:  
 - `data` contains:
    1. Raw original data and associated description files in text format for Card and Michigan data
    2. Cleaned DHS data in RData format (data was originally obtained in cleaned form)
 - `posteriors` Contains saved model posterior means that are used to create the tables for the paper

And three sets of scripts:
- `simulation.R` runs the monte carlo simulations from the paper
- `estimation.R` estimates three models from the paper and saves the model posteriors
- `card_analysis.R`, `mich_analysis.R`, and `dhs_analysis.R` produce tables and figures
for the application section of the paper from the saved model posteriors.

Since the `posteriors` folder includes the saved posteriors used in the current paper draft, it is not necessary to run the estimation code if you are just interested in replicating figures and tables exactly as in the paper.

#### Requirements

It is is necessary to install the R package [dhlvm](www.github.com/evanmunro/dhlvm) for the models in the paper from Github using `devtools`:

```
devtools::install_github("evanmunro/dhlvm")
```  

The scripts also require the following R packages, which are available on CRAN:
```
MCMCpack, xtable, future.apply, reshape2, ggplot2
```
