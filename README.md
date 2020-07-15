### Replication code for "Latent Dirichlet Analysis of Categorical Survey Expectations"
#### Author: Evan Munro and Serena Ng

This repository contains data and code for our [paper](https://arxiv.org/abs/1910.04883), which uses hierarchical bayesian latent class models to summarize and interpret heterogeneity in categorical subjective expectations data.

 - The main repository folder contains the replication scripts 
 - `data` contains:
    1. Raw data and associated description files in text format for Michigan data
    2. Card data in text and RData format and description files in text format
    3. README with sources and data guide
 - `posteriors` Contains saved model posterior means that are used to create the tables for the paper


## Replication Instructions

#### Setup

1. Clone our Github repository and change working directory to the repository.

```
git clone https://github.com/evanmunro/lda-survey-exp
cd lda-survey-exp

```

2. Install the R package written for the models in the paper [dhlvm](www.github.com/evanmunro/dhlvm) from Github using the R package `devtools`:

```
devtools::install_github("evanmunro/dhlvm")
```  

2. Make sure the following R packages are installed through CRAN

```
MCMCpack, reshape2, ggplot2
```

#### Reproducing Tables and Figures

```
Rscript exhibits.R
```

will reproduce Tables 1-3 from the data files and saved model posteriors.

#### Reproducing Data Cleaning, Model Estimation and Simulations  

1. `Rscript clean_card.R` processes the raw data from David Card's website and saves a cleaned version as `data/card_data.RData`
2. `Rscript mich_analysis.R` re-estimates LDA-DE on the Michigan data and saves the model posterior
in `posteriors/mich_estimate.RData`
3. `Rscript dhs_analysis.R` re-estimates LDA-E on the Card data and saves the model posterior in
`posteriors/card_estimate.RData`
4. `Rscript mcmc_simulations.R` runs the simulations described verbally in Section 4.3 in the paper
