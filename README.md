# Data Science Hub

This repository is my working hub for data science projects, reusable analysis patterns, experiment templates, and model starter code.

## Repository Sections

| Section | Purpose |
|---|---|
| [`Phenotypic_Association_Analysis_in_HairEyeColor`](./Phenotypic_Association_Analysis_in_HairEyeColor/) | Completed R/Quarto statistical analysis project |
| [`Causal_MLB_Pitch_Clock_Impact`](./Causal_MLB_Pitch_Clock_Impact/) | Causal inference project estimating the impact of MLB's 2023 pitch clock on game duration |
| [`model_templates`](./model_templates/) | Reusable machine learning templates to copy into future projects |

## Current Projects

### Causal Impact of the MLB Pitch Clock

A causal inference project using Retrosheet game logs to estimate the impact of MLB's 2023 pitch clock on game duration.

**Tools:** Python, pandas, statsmodels, seaborn  
**Methods:** interrupted time-series style pre/post design, adjusted regression, fixed effects  
**Headline result:** post-2023 games are roughly 29-31 minutes shorter on average, depending on specification.

### Phenotypic Association Analysis in `HairEyeColor`

A graduate-level R analysis using contingency-table inference, effect sizes, and publication-style visualizations.

**Tools:** R, Quarto, ggplot2, dplyr, tidyr  
**Outputs:** final PDF report, derived statistical tables, and high-resolution figures

## Reusable Model Templates

The [`model_templates`](./model_templates/) folder contains ready-to-adapt starting points for:

- Classification
- Regression
- Clustering
- Time-series forecasting
- PCA / dimensionality reduction
- Shared ML helper utilities

These are designed to be copied into new project folders and modified for the dataset, target variable, metrics, and business problem at hand.

## Portfolio Goal

This repo is meant to grow into a clean, practical data science portfolio showing:

- Exploratory data analysis
- Statistical inference
- Machine learning workflows
- Reusable algorithm patterns
- Data visualization
- Business and research storytelling
