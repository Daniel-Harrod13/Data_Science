# DML Healthcare Wait-Time Results

Scenario: estimate the long-term adherence impact of reducing appointment wait times above 30 days by 20%.

Linear slope estimates for the effect of one additional wait day on 180-day adherence:

- Naive OLS slope: -1.475
- Fixed Effects OLS slope: -1.246

Cross-fitted nuisance model quality:

- Outcome nuisance R-squared: 0.439
- Treatment nuisance R-squared: 0.486

Opportunity sizing: average adherence-score gain per targeted patient from a 20% wait-time reduction among patients currently waiting more than 30 days:

| Method | Avg gain per patient | Total gain |
|---|---:|---:|
| Naive OLS | 11.767 | 160,391.9 |
| Fixed Effects OLS | 9.937 | 135,450.1 |
| Local DML | 12.169 | 165,870.7 |
| Oracle True DGP | 13.594 | 185,304.1 |

Local DML absolute error vs oracle average gain: 1.426 adherence points per patient.

Interpretation: fixed effects reduce confounding, but the linear specification spreads benefit across all wait times. Local DML better captures that operational value is concentrated among patients facing long waits, especially beyond the 30-day cliff.
