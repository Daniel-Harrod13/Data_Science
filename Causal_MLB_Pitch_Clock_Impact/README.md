# Causal Impact of the MLB Pitch Clock

This project estimates how MLB's 2023 pitch clock rule change affected game duration, with a secondary look at stolen bases during the same rule-change era.

## Why This Project Is Interesting

The pitch clock was one of MLB's most visible modern rule changes. It was designed to speed up games and improve pace of play. Because the rule started league-wide in 2023, the project uses a before/after interrupted time-series style design rather than a classic treated/control difference-in-differences design.

## Causal Question

> What was the impact of the 2023 MLB pitch clock on average game duration?

## Data Source

Regular-season MLB game logs from Retrosheet:

- https://www.retrosheet.org/gamelogs/index.html

Included seasons:

- 2021
- 2022
- 2023
- 2024

## Methods

- Naive pre/post comparison
- Adjusted OLS regression
- Month and ballpark fixed effects
- Controls for total runs, game length in outs, attendance, and day/night status
- Time-series visualization around the 2023 rule-change boundary

## Project Structure

```text
Causal_MLB_Pitch_Clock_Impact/
├── data/
│   ├── raw/
│   └── processed/
├── docs/
│   ├── assumptions.md
│   ├── causal_question.md
│   ├── identification_strategy.md
│   └── variable_dictionary.md
├── reports/
│   ├── figures/
│   └── tables/
├── src/
│   ├── data_cleaning.py
│   └── estimate_effect.py
├── requirements.txt
└── README.md
```

## How to Run

From the repository root:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r Causal_MLB_Pitch_Clock_Impact/requirements.txt
python Causal_MLB_Pitch_Clock_Impact/src/data_cleaning.py
python Causal_MLB_Pitch_Clock_Impact/src/estimate_effect.py
```

## Outputs

Generated tables:

- `reports/tables/yearly_summary.csv`
- `reports/tables/causal_estimates.csv`

Generated figures:

- `reports/figures/monthly_game_duration.png`
- `reports/figures/duration_distribution_pre_post.png`
- `reports/figures/monthly_stolen_bases.png`

## Interpretation Note

Because the pitch clock was implemented league-wide, there is no untreated MLB control group. Results should be interpreted as a carefully adjusted before/after estimate consistent with the rule's effect, not as a perfect randomized causal estimate.
