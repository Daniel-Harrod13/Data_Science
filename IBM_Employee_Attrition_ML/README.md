# IBM Interview-Ready ML Project: Employee Attrition Prediction

This project is designed as an interview-style end-to-end machine learning case study for IBM-like HR analytics use cases:

- **Business problem:** Identify employees at risk of leaving.
- **ML objective:** Binary classification (`Attrition` = 1/0).
- **Outcome:** A trained model + reproducible artifacts + subgroup metrics for responsible AI discussion.

## Project Structure

```
IBM_Employee_Attrition_ML/
├── data/
│   └── processed/
├── artifacts/
├── src/
│   ├── train.py
│   └── predict.py
└── requirements.txt
```

## Why This Is Interview-Ready

- Uses a **proper ML pipeline** (`ColumnTransformer` + preprocessing + estimator).
- Compares multiple models with **cross-validated ROC-AUC**.
- Tracks **test metrics** and **subgroup metrics** (`Gender`, `OverTime`) to discuss fairness/operational risks.
- Exports reusable artifacts:
  - `artifacts/model.joblib`
  - `artifacts/metrics.json`
  - `artifacts/feature_importance.csv`
  - `artifacts/predictions.csv`

## Setup

From repository root:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r IBM_Employee_Attrition_ML/requirements.txt
```

## Train

### Option A: Use synthetic data (default)

```bash
python IBM_Employee_Attrition_ML/src/train.py
```

This generates `data/processed/synthetic_attrition.csv` and trains the model.

### Option B: Use your own CSV

Your data must include the target column:

- `Attrition` (0 or 1)

Then run:

```bash
python IBM_Employee_Attrition_ML/src/train.py --input /absolute/path/to/your_data.csv
```

## Predict on New Data

```bash
python IBM_Employee_Attrition_ML/src/predict.py \
  --input /absolute/path/to/new_employees.csv
```

Predictions are written to:

- `IBM_Employee_Attrition_ML/artifacts/predictions_new_data.csv`

## Talking Points for IBM Interviews

- How you mapped a business retention problem to a measurable ML objective.
- Why ROC-AUC was used for model selection on imbalanced classes.
- How pipeline-based preprocessing avoids train/test leakage.
- How subgroup slices can reveal unequal performance and inform mitigation.
- How this can be productionized (scheduled scoring + monitoring drift + retraining cadence).

## Interview Notebook

Use the guided walkthrough notebook for a live interview narrative:

- `IBM_Employee_Attrition_ML/notebooks/interview_walkthrough.ipynb`
