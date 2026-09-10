# Reusable Model Templates

This folder is a grab-and-adapt library for common data science workflows. Each template is intentionally written as a practical starting point: update the data path, target column, feature choices, and scoring metric for the dataset you are working with.

## Folder Map

| Folder | Purpose |
|---|---|
| `classification/` | Binary or multiclass prediction problems |
| `regression/` | Continuous outcome prediction |
| `clustering/` | Unsupervised customer/entity segmentation |
| `time_series/` | Forecasting and time-aware validation |
| `dimensionality_reduction/` | PCA-style feature compression and visualization |
| `utilities/` | Shared helper functions for loading data, splitting, metrics, and saving artifacts |

## Typical Adaptation Steps

1. Copy a template into a project folder.
2. Change `DATA_PATH` and `TARGET_COLUMN`.
3. Review numeric/categorical feature handling.
4. Pick the metric that matches the business problem.
5. Run the script and inspect generated metrics/artifacts.
6. Turn the results into a short project narrative: problem, method, result, limitation, next step.

## Recommended Project Pattern

```text
project_name/
├── data/
│   ├── raw/
│   └── processed/
├── notebooks/
├── src/
├── artifacts/
├── reports/
└── README.md
```

Keep raw data, trained models, and generated artifacts out of Git when they are large or sensitive.
