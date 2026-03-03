# Presentation Page: IBM Predictive Maintenance ML

This page is a presentation-ready summary of the project and the latest model run.

## Slide 1: Business Problem

- Goal: predict which industrial assets are likely to fail in the next 7 days.
- Why it matters: fewer unplanned outages, better maintenance scheduling, and lower downtime cost.
- Decision output: prioritized alert queue for operations teams.

## Slide 2: Dataset and Approach

- Synthetic industrial telemetry generated across equipment types and operating modes.
- Time-aware validation split (`DayIndex`) to mimic real production forecasting.
- Models compared: Logistic Regression and Random Forest.
- Chosen metric family: ROC-AUC/PR-AUC + threshold-based precision/recall.

## Slide 3: Current Run Results

From `artifacts/metrics.json`:

- Selected model: `logistic_regression`
- CV ROC-AUC:
  - Logistic Regression: `0.702`
  - Random Forest: `0.696`
- Decision threshold: `0.69`
- Holdout metrics:
  - ROC-AUC: `0.698`
  - PR-AUC: `0.387`
  - Precision: `0.760`
  - Recall: `0.006`
  - Recall at precision >= 70%: `0.006`

## Slide 4: How to Explain the Result

- The model ranks risk reasonably (ROC-AUC ~0.70), which is useful for prioritization.
- Threshold is set high to keep precision strong and avoid too many false alarms.
- Very low recall at this threshold indicates a strict alert policy; in real operations, threshold tuning should be tied to maintenance team capacity and business risk tolerance.

## Slide 5: Next Improvements

- Tune threshold by weekly alert capacity target (e.g., top N assets/day).
- Add cost-based objective (false negative downtime cost vs false positive inspection cost).
- Add drift monitoring for telemetry distributions over time.
- Compare with boosted tree models and calibrated probabilities.

## 30-Second Interview Close

"I built this as an operations-oriented ML system, not just a classifier: a time-aware validation setup, ranking and decision metrics, and an alert workflow that can plug into maintenance dispatch. The next step is threshold and cost optimization with live feedback from field teams."
