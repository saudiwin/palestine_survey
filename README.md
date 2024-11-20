README
================

## About This Repo

This repository contains code and data to reproduce the results from
**Palestinian Attitudes Towards Settlers in the West Bank** by Tesea
Conte and Robert Kubinec. A description of the files is as follows:

1.  `run_palestine.R` This R script loads the data, cleans it and either
    fits models or uses saved models to create plots for the paper.

2.  `data` This folder contains the Qualtrics raw survey data in CSV
    form.

3.  `*.rds` All of these files are saved `brms` and `rstanarm` model
    objects used to fit models in the paper and perform MRP.
