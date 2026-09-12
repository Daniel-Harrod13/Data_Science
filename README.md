# Data Science Hub

This repository is my working hub for data science projects, reusable analysis patterns, experiment templates, and model starter code.

## Repository Sections

| Section | Purpose |
|---|---|
| [`Phenotypic_Association_Analysis_in_HairEyeColor`](./Phenotypic_Association_Analysis_in_HairEyeColor/) | Completed R/Quarto statistical analysis project |
| [`Causal_MLB_Pitch_Clock_Impact`](./Causal_MLB_Pitch_Clock_Impact/) | Causal inference project estimating the impact of MLB's 2023 pitch clock on game duration |
| [`Uplift_Modeling_Random_Forest`](./Uplift_Modeling_Random_Forest/) | Causal machine learning project estimating heterogeneous treatment effects with a Random Forest T-learner |
| [`Surrogate_Index_Aquarium_Store`](./Surrogate_Index_Aquarium_Store/) | Surrogate-index project estimating long-term treatment effects from short-term aquarium-store behaviors |
| [`model_templates`](./model_templates/) | Reusable machine learning templates to copy into future projects |

## Current Projects

### Causal Impact of the MLB Pitch Clock

A causal inference project using Retrosheet game logs to estimate the impact of MLB's 2023 pitch clock on game duration.

**Tools:** Python, pandas, statsmodels, seaborn  
**Methods:** interrupted time-series style pre/post design, adjusted regression, fixed effects  
**Headline result:** post-2023 games are roughly 29-31 minutes shorter on average, depending on specification.

### Uplift Modeling with Random Forests

A causal machine learning project that simulates a treatment/control marketing experiment and estimates customer-level conditional average treatment effects using a Random Forest T-learner.

**Tools:** Python, pandas, scikit-learn, matplotlib  
**Methods:** simulated experiment, T-learner, separate treated/control outcome models, CATE estimation, uplift decile analysis  
**Current results:** holdout CATE correlation of 0.910 and R-squared of 0.821. The model ranks customers into a targeting policy where the top 30% by predicted uplift are labeled `Would treat` and the rest are `Excluded`.

### Surrogate Index for Aquarium Store Treatment Effects

A causal/business analytics project that estimates the long-term impact of an aquarium-store treatment before the final 180-day outcome is available.

**Tools:** Python, pandas, scikit-learn, matplotlib  
**Methods:** surrogate-index modeling, randomized treatment/control experiment simulation, short-term behavioral proxies, long-term value prediction  
**Current results:** surrogate-index validation R-squared of 0.899 and correlation of 0.950. The estimated treatment effect on the surrogate index is about $51.93, close to the simulated true 180-day ATE of about $56.91.

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
- Causal inference and uplift modeling
- Machine learning workflows
- Reusable algorithm patterns
- Data visualization
- Business and research storytelling
