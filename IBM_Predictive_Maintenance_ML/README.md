# IBM Interview-Ready ML Project: Predictive Maintenance

This project simulates an IBM-style industrial analytics use case: predicting which assets are likely to fail in the next 7 days.

- **Business problem:** Reduce unplanned downtime and maintenance cost.
- **ML objective:** Binary classification (`FailureNext7Days` = 1/0).
- **Output:** Ranked risk scores and actionable alert queue.

## Project Structure

```
IBM_Predictive_Maintenance_ML/
├── data/
│   └── processed/
├── artifacts/
├── notebooks/
│   └── interview_walkthrough.ipynb
├── src/
│   ├── train.py
│   └── predict.py
└── requirements.txt
```

## Why This Is Interview-Ready

- End-to-end reproducible training and inference scripts.
- Time-aware train/test split for a realistic ops setting.
- Cross-validated model comparison (ROC-AUC).
- Business-relevant evaluation:
  - ROC-AUC
  - PR-AUC
  - Recall at precision >= 70%
- Explainability + subgroup checks for reliability discussions.

## Setup

From repository root:

```bash
conda create -y -n ibm_maintenance_ml python=3.12
conda activate ibm_maintenance_ml
pip install -r IBM_Predictive_Maintenance_ML/requirements.txt
pip install jupyter ipykernel
python -m ipykernel install --user --name ibm_maintenance_ml --display-name "Python (ibm_maintenance_ml)"
```

## Train

### Option A: Synthetic telemetry (default)

```bash
python IBM_Predictive_Maintenance_ML/src/train.py
```

### Option B: Your own data

Your CSV should include `FailureNext7Days` for training.

```bash
python IBM_Predictive_Maintenance_ML/src/train.py --input /absolute/path/to/maintenance_data.csv
```

## Predict on New Data

```bash
python IBM_Predictive_Maintenance_ML/src/predict.py \
  --input /absolute/path/to/new_telemetry.csv
```

## Notebook Walkthrough

Use this for interview presentation:

- `IBM_Predictive_Maintenance_ML/notebooks/interview_walkthrough.ipynb`
- `IBM_Predictive_Maintenance_ML/PRESENTATION_README.md` (presentation page with current run results)

## IBM-Focused Talking Points

- How predictive maintenance ties directly to enterprise cost and uptime KPIs.
- Why thresholding must align with operational capacity (alert fatigue trade-off).
- How to productionize with scheduled scoring, drift monitoring, and retraining.
- How model outputs can feed a maintenance dispatch workflow.
