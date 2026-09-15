# Data Science Hub

This repository is my working hub for data science projects, reusable analysis patterns, experiment templates, and model starter code.

## Repository Sections

| Section | Purpose |
|---|---|
| [`Phenotypic_Association_Analysis_in_HairEyeColor`](./Phenotypic_Association_Analysis_in_HairEyeColor/) | Completed R/Quarto statistical analysis project |
| [`Causal_MLB_Pitch_Clock_Impact`](./Causal_MLB_Pitch_Clock_Impact/) | Causal inference project estimating the impact of MLB's 2023 pitch clock on game duration |
| [`Uplift_Modeling_Random_Forest`](./Uplift_Modeling_Random_Forest/) | Causal machine learning project estimating heterogeneous treatment effects with a Random Forest T-learner |
| [`Surrogate_Index_Aquarium_Store`](./Surrogate_Index_Aquarium_Store/) | Surrogate-index project estimating long-term treatment effects from short-term aquarium-store behaviors |
| [`DML_Healthcare_Wait_Time_Linear_Trap`](./DML_Healthcare_Wait_Time_Linear_Trap/) | Double Machine Learning project showing how linear fixed effects can mis-size nonlinear healthcare wait-time interventions |
| [`Bayesian_Healthcare_AB_Testing_False_Positive_Trap`](./Bayesian_Healthcare_AB_Testing_False_Positive_Trap/) | Monte Carlo study of optimistic historical priors in low-traffic healthcare A/B tests |
| [`AirForce_Recruiting_Switchback_Experiment`](./AirForce_Recruiting_Switchback_Experiment/) | Switchback experiment simulation measuring paid-media incrementality for Air Force recruiting campaigns |
| [`Aquarium_Store_Sequential_Testing`](./Aquarium_Store_Sequential_Testing/) | Sequential A/B testing simulation comparing peeking risk, O’Brien–Fleming, and Pocock boundaries for an aquarium-store promotion |
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

### Double Machine Learning vs Fixed Effects: Healthcare Wait Times

A causal ML simulation showing how fixed effects can reduce confounding but still mis-size opportunity when treatment effects are nonlinear.

**Tools:** Python, pandas, scikit-learn, statsmodels, matplotlib  
**Methods:** simulated observational healthcare operations data, fixed effects OLS, cross-fitted nuisance models, local Double Machine Learning, nonlinear wait-time dose response  
**Current results:** for patients waiting more than 30 days, Local DML estimates a 20% wait-time reduction would improve adherence by about 12.17 points per targeted patient, closer to the simulated oracle value of 13.59 than the fixed-effects linear estimate of 9.94.

### Bayesian Healthcare A/B Testing: False-Positive Trap

A synthetic Monte Carlo project comparing a one-sided frequentist z-test with Bayesian shipping rules that borrow from genuine historical +2 percentage-point winners in appointment-reminder experiments with 300 patients per arm.

**Tools:** Python, NumPy, pandas, SciPy, seaborn, matplotlib  
**Methods:** binomial A/B simulation, normal-normal conjugate updating, empirical optimistic priors, prior-mean haircut sensitivity, long-run decision operating characteristics  
**Current results:** the one-sided 5% benchmark and the no-history Bayesian rule have comparable directional thresholds. Bayesian performance varies substantially with historical sample size and prior haircut; the full power/risk grid is reported rather than treating any single prior as universally preferred.

### Air Force Recruiting Switchback Experiment

A simulated public-sector marketing analytics project that tests whether reduced paid-media bids preserve total qualified recruiting leads while lowering cost.

**Tools:** Python, pandas, statsmodels, matplotlib  
**Methods:** region-day switchback experiment, fixed effects regression, permutation/randomization inference, platform attribution vs first-party outcome measurement  
**Current results:** reduced-bid days save about $669 per region-day, while platform-attributed leads fall by about 21.74 but first-party total qualified leads fall by only about 2.04. Cost per qualified lead drops from about $33.35 to $18.22.

### Aquarium Store Sequential Testing

An original synthetic promotion experiment comparing fixed-horizon testing, unadjusted peeking, and calibrated group-sequential boundaries for customer spending.

**Tools:** Python, NumPy, pandas, SciPy, matplotlib

**Methods:** correlated Gaussian information increments, independent Monte Carlo boundary calibration and validation, two-sided first-crossing decisions, power and stopping-time analysis

**Current results:** daily peeking produces a 28.20% false-positive rate; O’Brien–Fleming and Pocock hold it near 5%. For a +$4 effect, Pocock stops about four days earlier on average, with lower detection probability at the same maximum sample size.

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
