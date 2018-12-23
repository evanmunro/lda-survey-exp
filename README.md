### Estimation, Figures, and Tables for "Markov Models for Multinomial Time Series"
#### Author: Evan Munro

This repository contains R code and Jupyter notebooks to replicate the estimation, figures, and tables for the working paper.

[Munro, Evan. "Markov Models for Multinomial Time Series". 2018.](www.evanmunro.ca/files/discreteTS.pdf)

#### 1. Install software

First, it is necessary to install the [dhlvm](www.github.com/evanmunro/dhlvm) package from Github using the R package devtools. This package was written by the author for Bayesian estimation of the two models described in the paper. The package is still in development and the installation process has only been tested on UNIX-based systems.

```
devtools::install_github("evanmunro/dhlvm")
```  

The replication scripts also require the following R packages, which are available on CRAN:
```
ggplot2,
```

#### 2. Download data

The Michigan survey response data is available for download at the [Survey of Consumers SDA Archive](https://data.sca.isr.umich.edu/sda-public/cgi-bin/hsda?harcsda+sca) and then clicking Download -> Customized Subset. Then, enter the following variables in the "Individual Variables" box and download as a CSV:

```
YYYYMM PAGO PEXP BUS12 BUS5 DUR
```


The GPSS data is not public. The raw data used for this paper is available for Stanford researchers at this [link](https://sul-datasets.stanford.edu/gallup/GPSS/). It may also be available for researchers at other institutions through their library. The filepaths of the three relevant files are as follows:

```
"GPSS -- 03 Mar Environment - 2017/GPSS Environment Final GA Aggregate.dta"
"GPSS -- 05 May Values and Beliefs 2017/GPSS Values Final GA Aggregate.dta"
"GPSS -- 11 Nov Health Care - 2017/GPSS Health Final GA Aggregate.dta"
```

#### 3. Run Jupyter Notebooks

Documentation and code for data cleaning, model estimation, and figure plotting are included in the two Jupyter notebooks in the repository. Executing the notebooks requires [installing](https://jupyter.readthedocs.io/en/latest/install.html) Jupyter, and [installing](https://github.com/IRkernel/IRkernel) the R kernel for Jupyter. The Jupyter installation also requires a current Python installation.

If there are issues with the Jupyter installation, the code contents of the notebook are also included as regular R scripts in the repository.
