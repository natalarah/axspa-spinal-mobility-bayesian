# Bayesian Modeling of Spinal Mobility Variables for Predicting Perceived Treatment Control in Axial Spondyloarthritis

## Overview

This repository contains the analysis code and anonymized data associated with the paper:

> Lara-Herrera CN, Martínez-Munoz JC, Beltrán-Ostos A, Bautista-Molano W. *Bayesian modeling of spinal mobility variables for predicting perceived treatment control in axial spondyloarthritis*. [Journal name], [year].

The study presents an exploratory framework for identifying spinal mobility (SM) variables associated with patients' perceived treatment control in axial spondyloarthritis (axSpA), using optical motion capture data and Bayesian logistic regression.

## Repository Structure
├── run_analysis.R # Main entry point — loads data and runs full analysis
├── best_subset_bayes.R # Experiments 2 and 3: best subset selection and Bayesian modeling
├── best_subset_pca.R # Experiment 1: PCA logistic regression
├── Results_Bayes.R # Final Bayesian model fitting and evaluation
├── Data/
│ ├── patients.csv # Anonymized data for axSpA patients (n=20)
│ └── controls.csv # Anonymized data for healthy controls (n=20)
└── README.md
## Requirements

- R (>= 4.0)
- R packages: `tidyverse`, `brms`, `caret`, `combinat`, `loo`

Install all required packages with:

```r
install.packages(c("tidyverse", "brms", "caret", "combinat", "loo"))
```

## How to Reproduce the Results

1. Clone this repository:
```bash
git clone https://github.com/natalarah/axspa-spinal-mobility-bayesian.git
```

2. Open `run_analysis.R` and update the file paths to match your local directory.

3. Run `run_analysis.R` — this script will:
   - Load and prepare the data from `Data/patients.csv` and `Data/controls.csv`
   - Run the best subset selection across 2,048 candidate models (Experiment 2)
   - Fit Bayesian logistic regression models for the best subset per number of variables (Experiment 3)

4. To fit and evaluate the final 6-covariate Bayesian model reported in the paper, run the following block in R:

```r
library(brms)
library(loo)

df_priors <- datos %>% mutate(CONTROLLED = if_else(CONTROLLED == 'SI', 0, 1))

formula_final <- as.formula('CONTROLLED ~ 1 + Mass_kg + CERV_ROT_LEF + 
                              CERV_ROT_RIG + CERV_FRONT_FLEX + 
                              LAT_CERV_FLEX_LEF + INTERMAL_DISTANCE')

priors_final <- get_prior(formula_final, data = df_priors, family = bernoulli())

bayesian_model <- brm(formula_final,
                      data = df_priors,
                      family = bernoulli(),
                      cores = 2,
                      prior = priors_final,
                      seed = 123)

summary(bayesian_model)

# Leave-one-out cross-validation
loo_result <- loo(bayesian_model, reloo = TRUE)
print(loo_result)
```

## Data

The dataset includes 40 participants (20 axSpA patients, 20 healthy controls matched by age and sex). Spinal mobility was assessed using an OptiTrack® optical motion capture system with 28 reflective markers. The outcome variable (`CONTROLLED`) is a single-item patient-reported measure of perceived treatment control.

Patient identity has been removed. The data files contain only numeric identifiers and biomechanical measurements.

## Notes

- Fitting the Bayesian models requires substantial RAM (>4 GB recommended). 
- The best subset selection loop (2,048 models) may take several minutes to complete.
- Model fitting was performed with `brms` version 2.23.0 and R version 4.6.1.
- Ethics approval: Hospital Militar Central de Bogotá, approval No. 2017-097.

## Authors

- Claudia Natalia Lara-Herrera — Universidad ECCI, Bogotá (corresponding author)
- Juan Camilo Martínez-Munoz — Vanderbilt University / UNODC
- Adriana Beltrán-Ostos — Universidad El Bosque
- Wilson Bautista-Molano — Fundación Santa Fe de Bogotá
