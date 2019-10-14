This folder contains raw data for the Michigan and Card application in the `card` and `michigan`
folders.

The original sources of the data are as follows:

 - The raw DHS data can be downloaded from [DHS Program](https://dhsprogram.com/data/)
 - The Michigan data can be downloaded from the [Survey of Consumers SDA Archive](https://data.sca.isr.umich.edu/sda-public/cgi-bin/hsda?harcsda+sca)
 - The original Card dataset is available [here](http://davidcard.berkeley.edu/data_sets.html). The additional variables are available from the [NLS](https://www.nlsinfo.org/investigator/pages/login.jsp).

 We do not yet have permission to share the DHS data in cleaned form, so it is not currently available in this repository.

### DHS Data

The data contains 3 categorical variables:
1. `water_qual`, rated from 1 to 4
2. `toilet_qual`, rated from 1 to 5
3. `floor_qual`, rated from 1 to 3

It also contains 7 binary variables, where a value of 2 indicates ownership and
1 indicates lack of ownership: `electric`, `radio`, `tv`, `fridge`, `motorbike`, `car`
and `phone`.

The group indicator for the individual respondents is their country `cname`.
The countries are: Colombia (CO), Guyana (GT), Haiti (HT), Nicaragua (NI), and Peru (PE).

### Michigan Survey of Consumers Data

The model is estimated on the following 14 variables, which were downloaded directly
from the Michigan website to `mich_raw.csv`
```
PAGO, PEXP, RINC, BAGO, BEXP, BUS12, BUS5, UNEMP, GOVT, RATEX, PX1Q1, DUR, HOM, CAR
```
Each question and the raw responses are described in detail in `michigan/codebook.txt`

The group indicator is `YYYYMM`, the month of that the individual responded to the survey,
and the variable `EDUC` which is described in the codebook as well.'

The `michigan` folder also contains some CSV files which are used to analyze the Michigan data further:
- Economic policy uncertainty index : `epu.csv`, Source: Baker, Bloom and Davis website
- Unemployment rate: `UNRATE.csv`, Source: FRED
- Michigan ICS: `UMCSENT.csv`, Source: FRED
- S&P 500 Monthly Returns: `sp500_shiller.csv`, Source: Robert Shiller [website](http://www.econ.yale.edu/~shiller/data.htm)

### NLSYM Data

The dataset contains the following variables from Card's original analysis,
which are described in `codebook_card.txt` and were downloaded from this [link]().
```
ID, LWAGE76, BLACK,  EXP76, EXP762, SMSA76R, REG76R, ED76,
DADED, MOMED, NODADED, NOMOMED, LIBCRD14, SINMOM14
```
The group indicator for the model is created from interacting `LIBCRD14` and `SINMOM14`.
It also contains 14 additional variables from the NLSYM that were recoded. The recode is summarized as follows.

Responses 1 to 4 for the 11 rotter scale variables (`rotter_A` to `rotter_K`) correspond to:
1. Most internal
2. Internal
3. External
4. Most External

Responses 1 to 6 for `subj_liked` and `subj_disliked` correspond to:
1. Foreign Languages or Humanities
2. Social Science
3. Science
4. Math
5. Commercial, Vocational or Other
6. None or Missing

The responses for `attitude_hs` are as follows:

1. Like it very much
2. Like it fairly well
3. Dislike it somewhat or dislike it very much
4. Missing

with the actual questions, and question-specific interpretation of the responses available from by searching the NLSYM question code in `codebook_additional.txt`.

The mapping from NLS question codes to variable names is in the following table:

| NLSYM_id | var_name    |
|----------|-------------|
| R0492100 | rotter_A    |
| R0492300 | rotter_B    |
| R0492500 | rotter_C    |
| R0492700 | rotter_D    |
| R0492900 | rotter_E    |
| R0493100 | rotter_F    |
| R0493300 | rotter_G    |
| R0493500 | rotter_H    |
| R0493700 | rotter_I    |
| R0493900 | rotter_J    |
| R0494100 | rotter_K    |
| R0011600 | subj_liked  |
| R0011800 | subj_dislik |
| R0012800 | attitude_hs |
