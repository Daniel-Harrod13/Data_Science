# Aquarium Promotion: Simulation Results

All results are synthetic, not store observations.

Seed: 20260915; independent calibration paths: 500,000; evaluation paths per scenario: 100,000.

## Calibrated two-sided boundaries

| Day | O’Brien–Fleming | Pocock |
|---|---:|---:|
| 6 | 4.5672 | 2.4117 |
| 12 | 3.2295 | 2.4117 |
| 18 | 2.6369 | 2.4117 |
| 24 | 2.2836 | 2.4117 |
| 30 | 2.0425 | 2.4117 |

## Operating characteristics

Rejection under the null is a false positive. Detection below means a rejection in the correct direction; wrong-direction rates are in the CSV. Customers and stopping days average over every trial, including non-rejections.

| Scenario | Method | Rejection % (95% MC interval) | Correct detection % | Mean day | Treatment customers |
|---|---|---:|---:|---:|---:|
| No effect | Fixed horizon | 5.07 (4.94–5.21) | — | 30.00 | 1200 |
| No effect | Naive daily peeking | 28.19 (27.92–28.47) | — | 23.94 | 958 |
| No effect | Naive five looks | 14.22 (14.01–14.44) | — | 27.80 | 1112 |
| No effect | O'Brien–Fleming | 5.04 (4.90–5.18) | — | 29.78 | 1191 |
| No effect | Pocock | 5.06 (4.92–5.19) | — | 29.26 | 1170 |
| Modest improvement | Fixed horizon | 16.48 (16.25–16.71) | 16.31 | 30.00 | 1200 |
| Modest improvement | Naive daily peeking | 40.62 (40.32–40.93) | 35.38 | 22.05 | 882 |
| Modest improvement | Naive five looks | 27.27 (27.00–27.55) | 25.85 | 26.28 | 1051 |
| Modest improvement | O'Brien–Fleming | 15.97 (15.74–16.20) | 15.77 | 29.26 | 1171 |
| Modest improvement | Pocock | 13.16 (12.95–13.37) | 12.75 | 28.40 | 1136 |
| Large improvement | Fixed horizon | 97.36 (97.26–97.46) | 97.36 | 30.00 | 1200 |
| Large improvement | Naive daily peeking | 98.80 (98.73–98.86) | 98.22 | 7.86 | 314 |
| Large improvement | Naive five looks | 98.17 (98.09–98.26) | 98.16 | 12.10 | 484 |
| Large improvement | O'Brien–Fleming | 97.04 (96.94–97.15) | 97.04 | 19.34 | 773 |
| Large improvement | Pocock | 94.76 (94.62–94.90) | 94.76 | 15.23 | 609 |
| Harm | Fixed horizon | 83.48 (83.25–83.71) | 83.48 | 30.00 | 1200 |
| Harm | Naive daily peeking | 91.26 (91.08–91.43) | 90.18 | 11.82 | 473 |
| Harm | Naive five looks | 87.72 (87.52–87.93) | 87.67 | 16.56 | 662 |
| Harm | O'Brien–Fleming | 82.34 (82.11–82.58) | 82.34 | 23.46 | 938 |
| Harm | Pocock | 75.10 (74.83–75.37) | 75.09 | 20.43 | 817 |

## Interpretation

- Compare the sequential rules to the fixed-horizon benchmark at approximately equal false-positive risk. Naive peeking's apparent detection advantage comes with inflated risk.
- O’Brien–Fleming protects the final analysis with a high early threshold; Pocock offers easier early detection but a higher final threshold.
- A negative crossing supports harm, not merely lack of benefit. No crossing is inconclusive; this design has no futility boundary.
- Confidence intervals quantify evaluation Monte Carlo error conditional on the calibrated boundaries; they exclude calibration uncertainty.
- These are efficacy/harm boundaries for spending, not a comprehensive safety system or proof of profitability.
