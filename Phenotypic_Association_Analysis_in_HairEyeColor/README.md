# Project 1: Academic Analysis of `HairEyeColor` in R

This project delivers a graduate-level data science analysis using the built-in R dataset `datasets::HairEyeColor`.
The work focuses on contingency-table inference, interpretable effect sizes, and publication-quality visualizations.

## Project Structure

- `project_1_report.qmd` - final written Quarto report
- `R/analysis.R` - statistical pipeline and outputs
- `R/figures.R` - reusable academic plotting functions
- `data/derived/` - generated tables and test results
- `figures/` - generated high-resolution figures

## Research Questions

1. Are hair color and eye color independent in the full sample?
2. Does the hair-eye relationship differ by sex?
3. Which hair-eye combinations deviate most from independence?

## How to Run

From the `project_1` directory:

1. Run the analysis pipeline:

   `Rscript R/analysis.R`

2. Render the report:

   `quarto render project_1_report.qmd`

The pipeline creates publication-ready figures and derived statistical tables used by the report.

## Required R Packages

- `ggplot2`
- `dplyr`
- `tidyr`
- `scales`
- `knitr` (for report rendering)

Install them if needed:

`install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "knitr"))`
