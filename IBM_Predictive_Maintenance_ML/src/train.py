import argparse
import json
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    average_precision_score,
    precision_score,
    recall_score,
    roc_auc_score,
)
from sklearn.model_selection import StratifiedKFold, cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler


RANDOM_STATE = 42
TARGET_COL = "FailureNext7Days"


def generate_synthetic_maintenance_data(
    n_assets: int = 400, n_days: int = 180, random_state: int = RANDOM_STATE
) -> pd.DataFrame:
    rng = np.random.default_rng(random_state)
    equipment_types = np.array(["Compressor", "Turbine", "Pump", "Conveyor"])
    sites = np.array(["Austin", "Dallas", "Chicago", "Seattle"])
    operating_modes = np.array(["Normal", "HighLoad", "Eco"])

    rows = []
    for asset_idx in range(n_assets):
        asset_id = f"A{asset_idx:04d}"
        equipment_type = rng.choice(equipment_types, p=[0.30, 0.22, 0.28, 0.20])
        site = rng.choice(sites)
        asset_age = int(rng.integers(1, 18))
        base_vibration = float(rng.normal(2.1, 0.5))
        base_temp = float(rng.normal(58, 7))
        base_pressure = float(rng.normal(100, 10))
        days_since_maintenance = int(rng.integers(3, 45))

        for day in range(n_days):
            mode = rng.choice(operating_modes, p=[0.55, 0.30, 0.15])
            operating_hours = float(np.clip(rng.normal(16, 3), 6, 24))

            mode_temp_adj = 6 if mode == "HighLoad" else (-2 if mode == "Eco" else 0)
            mode_vibration_adj = 0.4 if mode == "HighLoad" else (-0.2 if mode == "Eco" else 0)

            sensor_vibration = float(
                max(0.1, base_vibration + 0.005 * day + mode_vibration_adj + rng.normal(0, 0.25))
            )
            sensor_temp = float(max(20, base_temp + 0.06 * day + mode_temp_adj + rng.normal(0, 1.6)))
            sensor_pressure = float(max(30, base_pressure + 0.02 * day + rng.normal(0, 4)))
            maintenance_flag = int(rng.random() < 0.015 or days_since_maintenance > 60)
            days_since_maintenance = 0 if maintenance_flag else days_since_maintenance + 1

            logit = (
                -6.2
                + 0.75 * (equipment_type == "Turbine")
                + 0.50 * (mode == "HighLoad")
                + 0.42 * sensor_vibration
                + 0.028 * sensor_temp
                + 0.020 * days_since_maintenance
                + 0.050 * asset_age
                + 0.018 * operating_hours
                - 0.040 * maintenance_flag * 10
            )
            failure_next_7d = int(rng.binomial(1, 1.0 / (1.0 + np.exp(-logit))))

            rows.append(
                {
                    "AssetID": asset_id,
                    "DayIndex": day,
                    "EquipmentType": equipment_type,
                    "Site": site,
                    "OperatingMode": mode,
                    "AssetAgeYears": asset_age,
                    "OperatingHours": operating_hours,
                    "SensorVibration": sensor_vibration,
                    "SensorTemp": sensor_temp,
                    "SensorPressure": sensor_pressure,
                    "DaysSinceMaintenance": days_since_maintenance,
                    "MaintenanceEventToday": maintenance_flag,
                    TARGET_COL: failure_next_7d,
                }
            )

    return pd.DataFrame(rows)


def build_preprocessor(X: pd.DataFrame) -> ColumnTransformer:
    numeric_cols = X.select_dtypes(include=["int64", "float64"]).columns.tolist()
    categorical_cols = X.select_dtypes(include=["object", "category", "string"]).columns.tolist()

    numeric_pipe = Pipeline(
        steps=[("imputer", SimpleImputer(strategy="median")), ("scaler", StandardScaler())]
    )
    categorical_pipe = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="most_frequent")),
            ("onehot", OneHotEncoder(handle_unknown="ignore")),
        ]
    )

    return ColumnTransformer(
        transformers=[("num", numeric_pipe, numeric_cols), ("cat", categorical_pipe, categorical_cols)]
    )


def candidate_models() -> dict:
    return {
        "logistic_regression": LogisticRegression(max_iter=2000, random_state=RANDOM_STATE),
        "random_forest": RandomForestClassifier(
            n_estimators=300,
            max_depth=10,
            min_samples_leaf=5,
            random_state=RANDOM_STATE,
        ),
    }


def select_model(X_train: pd.DataFrame, y_train: pd.Series, preprocessor: ColumnTransformer):
    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)
    best_name = ""
    best_score = -1.0
    best_pipeline = None
    scores = {}

    for name, model in candidate_models().items():
        pipeline = Pipeline(steps=[("prep", preprocessor), ("model", model)])
        auc = cross_val_score(pipeline, X_train, y_train, cv=cv, scoring="roc_auc", n_jobs=1)
        scores[name] = float(np.mean(auc))
        if scores[name] > best_score:
            best_name = name
            best_score = scores[name]
            best_pipeline = pipeline

    best_pipeline.fit(X_train, y_train)
    return best_name, scores, best_pipeline


def recall_at_min_precision(
    y_true: pd.Series, y_prob: np.ndarray, min_precision: float = 0.70
) -> tuple[float, float]:
    best_recall = 0.0
    selected_threshold = 0.5
    for threshold in np.arange(0.05, 0.96, 0.01):
        y_pred = (y_prob >= threshold).astype(int)
        prec = precision_score(y_true, y_pred, zero_division=0)
        rec = recall_score(y_true, y_pred, zero_division=0)
        if prec >= min_precision and rec >= best_recall:
            best_recall = float(rec)
            selected_threshold = float(round(threshold, 2))
    return best_recall, selected_threshold


def subgroup_metrics(
    frame: pd.DataFrame, y_true: pd.Series, y_prob: np.ndarray, group_col: str, threshold: float
) -> dict:
    results = {}
    y_pred = (y_prob >= threshold).astype(int)
    if group_col not in frame.columns:
        return results

    for value in frame[group_col].dropna().unique():
        mask = frame[group_col] == value
        if mask.sum() < 50:
            continue
        results[str(value)] = {
            "n_samples": int(mask.sum()),
            "precision": float(precision_score(y_true[mask], y_pred[mask], zero_division=0)),
            "recall": float(recall_score(y_true[mask], y_pred[mask], zero_division=0)),
        }
    return results


def extract_feature_importance(pipeline: Pipeline) -> pd.DataFrame:
    prep = pipeline.named_steps["prep"]
    model = pipeline.named_steps["model"]
    feature_names = prep.get_feature_names_out()

    if hasattr(model, "feature_importances_"):
        values = model.feature_importances_
    elif hasattr(model, "coef_"):
        values = np.abs(model.coef_.ravel())
    else:
        values = np.zeros(len(feature_names))

    return (
        pd.DataFrame({"feature": feature_names, "importance": values})
        .sort_values("importance", ascending=False)
        .reset_index(drop=True)
    )


def main():
    parser = argparse.ArgumentParser(description="Train predictive maintenance failure model.")
    parser.add_argument("--input", type=str, default="", help="Optional CSV input path.")
    parser.add_argument("--artifacts-dir", type=str, default="artifacts")
    args = parser.parse_args()

    project_root = Path(__file__).resolve().parents[1]
    artifacts_dir = project_root / args.artifacts_dir
    artifacts_dir.mkdir(parents=True, exist_ok=True)

    if args.input:
        data = pd.read_csv(Path(args.input).expanduser().resolve())
    else:
        data = generate_synthetic_maintenance_data()
        processed_dir = project_root / "data" / "processed"
        processed_dir.mkdir(parents=True, exist_ok=True)
        data.to_csv(processed_dir / "synthetic_maintenance.csv", index=False)

    if TARGET_COL not in data.columns:
        raise ValueError(f"Dataset must include target column '{TARGET_COL}'.")

    # Time-aware split: train on earlier days, test on later days.
    split_day = int(data["DayIndex"].quantile(0.80))
    train_data = data[data["DayIndex"] <= split_day].copy()
    test_data = data[data["DayIndex"] > split_day].copy()

    train_data = train_data.drop(columns=["AssetID"])
    test_data = test_data.drop(columns=["AssetID"])

    X_train = train_data.drop(columns=[TARGET_COL])
    y_train = train_data[TARGET_COL].astype(int)
    X_test = test_data.drop(columns=[TARGET_COL])
    y_test = test_data[TARGET_COL].astype(int)

    preprocessor = build_preprocessor(X_train)
    best_name, cv_scores, model_pipeline = select_model(X_train, y_train, preprocessor)

    y_prob = model_pipeline.predict_proba(X_test)[:, 1]
    roc_auc = float(roc_auc_score(y_test, y_prob))
    pr_auc = float(average_precision_score(y_test, y_prob))
    recall70, threshold = recall_at_min_precision(y_test, y_prob, min_precision=0.70)

    y_pred = (y_prob >= threshold).astype(int)
    test_precision = float(precision_score(y_test, y_pred, zero_division=0))
    test_recall = float(recall_score(y_test, y_pred, zero_division=0))

    metrics_payload = {
        "selected_model": best_name,
        "cv_roc_auc": cv_scores,
        "decision_threshold": threshold,
        "test_metrics": {
            "roc_auc": roc_auc,
            "pr_auc": pr_auc,
            "precision": test_precision,
            "recall": test_recall,
            "recall_at_precision_70": recall70,
        },
        "subgroup_metrics": {
            "EquipmentType": subgroup_metrics(X_test, y_test, y_prob, "EquipmentType", threshold),
            "OperatingMode": subgroup_metrics(X_test, y_test, y_prob, "OperatingMode", threshold),
        },
    }

    scored = X_test.copy()
    scored["actual_failure"] = y_test.values
    scored["failure_probability"] = y_prob
    scored["predicted_failure"] = y_pred
    scored["alert"] = np.where(y_prob >= threshold, "ALERT", "OK")

    alerts = scored[scored["alert"] == "ALERT"].sort_values("failure_probability", ascending=False)
    feature_importance = extract_feature_importance(model_pipeline)

    with open(artifacts_dir / "metrics.json", "w", encoding="utf-8") as f:
        json.dump(metrics_payload, f, indent=2)
    scored.to_csv(artifacts_dir / "scored_test.csv", index=False)
    alerts.to_csv(artifacts_dir / "alerts.csv", index=False)
    feature_importance.to_csv(artifacts_dir / "feature_importance.csv", index=False)
    joblib.dump(model_pipeline, artifacts_dir / "model.joblib")

    print("Training complete.")
    print(f"Selected model: {best_name}")
    print(f"Test ROC-AUC: {roc_auc:.3f}")
    print(f"Decision threshold: {threshold}")
    print(f"Alerts generated: {len(alerts)}")
    print(f"Artifacts written to: {artifacts_dir}")


if __name__ == "__main__":
    main()
