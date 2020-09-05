### Replication code for "Latent Dirichlet Analysis of Categorical Survey Responses"
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

The replication code requires both an R and a Julia installation. Julia is required only
for the plot in Figure 2.

1. Clone our Github repository and change working directory to the repository.

```
git clone https://github.com/evanmunro/lda-survey-exp
cd lda-survey-exp

```

2. Install the R package written for the models in the paper [dhlvm](http://github.com/evanmunro/dhlvm) from Github using the R package `devtools`:

```
devtools::install_github("evanmunro/dhlvm")
```  

3. Make sure the following required R packages are installed through CRAN

```
reshape2, ggplot2, future.apply, kableExtra, stargazer
```

4. Make sure the following Julia packages are installed
```
Plots, Distributions
```

#### Reproducing Tables and Figures

Tables and Figures are saved in `exhibits/`. To reproduce Figure 2, run:
```
julia exhibits.jl
```

which is saved as `dirichlet_density.pdf`.

To reproduce Table 1 and Figures 3-5, run:

```
Rscript exhibits.R
```

Figure 3 combines `mich1_pi_ev.pdf`, `mich3_pi_ev.pdf`, and `mich4_pi_ev.pdf`.
Figure 4 combines  `beta_PAGO.pdf`,`beta_PEXP.pdf`, `beta_BAGO.pdf`, `beta_BUS12.pdf`, `beta_DUR.pdf`, and `beta_UNEMP.pdf`  
Figure 5 combines `beta_rotter_G.pdf`, `beta_rotter_H.pdf`, and `beta_rotter_K.pdf`.

Table 1 is saved in TeX format as `table1.txt`.


#### Reproducing Data Cleaning, Model Estimation and Simulations  

1. `Rscript mich_analysis.R` re-estimates LDA-DS on the Michigan data and saves the model posterior
in `posteriors/mich_estimate.RData`
2. `Rscript dhs_analysis.R` re-estimates LDA-S on the Card data and saves the model posterior in
`posteriors/card_estimate.RData`
3. `Rscript simulations.R` runs the simulations described verbally in Section 4.3 in the paper
