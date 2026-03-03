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
    accuracy_score,
    f1_score,
    precision_score,
    recall_score,
    roc_auc_score,
)
from sklearn.model_selection import StratifiedKFold, cross_val_score, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler


RANDOM_STATE = 42
TARGET_COL = "Attrition"


def generate_synthetic_data(n_rows: int = 2000, random_state: int = RANDOM_STATE) -> pd.DataFrame:
    rng = np.random.default_rng(random_state)

    departments = np.array(["Sales", "Research & Development", "Human Resources"])
    job_roles = np.array(
        [
            "Sales Executive",
            "Research Scientist",
            "Laboratory Technician",
            "Manager",
            "Human Resources",
            "Manufacturing Director",
        ]
    )
    overtime_options = np.array(["Yes", "No"])
    gender_options = np.array(["Male", "Female"])
    marital_options = np.array(["Single", "Married", "Divorced"])

    data = pd.DataFrame(
        {
            "Age": rng.integers(18, 61, size=n_rows),
            "Department": rng.choice(departments, size=n_rows, p=[0.35, 0.55, 0.10]),
            "JobRole": rng.choice(job_roles, size=n_rows),
            "MonthlyIncome": rng.normal(6500, 2500, size=n_rows).clip(1200, 25000).round(0),
            "YearsAtCompany": rng.integers(0, 41, size=n_rows),
            "OverTime": rng.choice(overtime_options, size=n_rows, p=[0.28, 0.72]),
            "DistanceFromHome": rng.integers(1, 31, size=n_rows),
            "JobSatisfaction": rng.integers(1, 5, size=n_rows),
            "TrainingTimesLastYear": rng.integers(0, 7, size=n_rows),
            "WorkLifeBalance": rng.integers(1, 5, size=n_rows),
            "Gender": rng.choice(gender_options, size=n_rows),
            "MaritalStatus": rng.choice(marital_options, size=n_rows, p=[0.36, 0.48, 0.16]),
        }
    )

    logit = (
        -1.4
        + 2.0 * (data["OverTime"] == "Yes").astype(float)
        + 1.1 * (data["MaritalStatus"] == "Single").astype(float)
        + 0.030 * data["DistanceFromHome"]
        - 0.045 * data["Age"]
        - 0.00020 * data["MonthlyIncome"]
        - 0.050 * data["YearsAtCompany"]
        - 0.500 * data["WorkLifeBalance"]
        - 0.450 * data["JobSatisfaction"]
        + 0.150 * (data["TrainingTimesLastYear"] <= 1).astype(float)
    )
    probability = 1.0 / (1.0 + np.exp(-logit))
    data[TARGET_COL] = rng.binomial(1, probability, size=n_rows).astype(int)
    return data


def build_preprocessor(X: pd.DataFrame) -> ColumnTransformer:
    numeric_features = X.select_dtypes(include=["int64", "float64"]).columns.tolist()
    categorical_features = X.select_dtypes(include=["object", "category", "string"]).columns.tolist()

    numeric_transformer = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler()),
        ]
    )
    categorical_transformer = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="most_frequent")),
            ("onehot", OneHotEncoder(handle_unknown="ignore")),
        ]
    )
    return ColumnTransformer(
        transformers=[
            ("num", numeric_transformer, numeric_features),
            ("cat", categorical_transformer, categorical_features),
        ]
    )


def candidate_models() -> dict:
    return {
        "logistic_regression": LogisticRegression(max_iter=2000, random_state=RANDOM_STATE),
        "random_forest": RandomForestClassifier(
            n_estimators=300, max_depth=8, min_samples_leaf=5, random_state=RANDOM_STATE
        ),
    }


def select_model(X_train: pd.DataFrame, y_train: pd.Series, preprocessor: ColumnTransformer):
    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)
    best_name = None
    best_score = -np.inf
    best_pipeline = None
    scores = {}

    for name, model in candidate_models().items():
        pipeline = Pipeline(steps=[("prep", preprocessor), ("model", model)])
        auc_scores = cross_val_score(pipeline, X_train, y_train, cv=cv, scoring="roc_auc", n_jobs=1)
        scores[name] = float(np.mean(auc_scores))
        if scores[name] > best_score:
            best_score = scores[name]
            best_name = name
            best_pipeline = pipeline

    best_pipeline.fit(X_train, y_train)
    return best_name, scores, best_pipeline


def calculate_metrics(y_true: pd.Series, y_prob: np.ndarray, threshold: float = 0.5) -> dict:
    y_pred = (y_prob >= threshold).astype(int)
    return {
        "accuracy": float(accuracy_score(y_true, y_pred)),
        "precision": float(precision_score(y_true, y_pred, zero_division=0)),
        "recall": float(recall_score(y_true, y_pred, zero_division=0)),
        "f1": float(f1_score(y_true, y_pred, zero_division=0)),
        "roc_auc": float(roc_auc_score(y_true, y_prob)),
    }


def subgroup_metrics(
    frame: pd.DataFrame,
    y_true: pd.Series,
    y_prob: np.ndarray,
    group_col: str,
    threshold: float = 0.5,
) -> dict:
    output = {}
    if group_col not in frame.columns:
        return output
    y_pred = (y_prob >= threshold).astype(int)
    for val in frame[group_col].dropna().unique():
        mask = frame[group_col] == val
        if mask.sum() < 25:
            continue
        output[str(val)] = {
            "n_samples": int(mask.sum()),
            "precision": float(precision_score(y_true[mask], y_pred[mask], zero_division=0)),
            "recall": float(recall_score(y_true[mask], y_pred[mask], zero_division=0)),
        }
    return output


def extract_feature_importance(pipeline: Pipeline) -> pd.DataFrame:
    prep = pipeline.named_steps["prep"]
    model = pipeline.named_steps["model"]
    feature_names = prep.get_feature_names_out()

    if hasattr(model, "feature_importances_"):
        importance = model.feature_importances_
    elif hasattr(model, "coef_"):
        importance = np.abs(model.coef_.ravel())
    else:
        importance = np.zeros(len(feature_names))

    fi = pd.DataFrame({"feature": feature_names, "importance": importance})
    fi = fi.sort_values("importance", ascending=False).reset_index(drop=True)
    return fi


def main():
    parser = argparse.ArgumentParser(description="Train an interview-ready attrition model.")
    parser.add_argument(
        "--input",
        type=str,
        default="",
        help="Optional path to a CSV with target column Attrition (0/1).",
    )
    parser.add_argument("--artifacts-dir", type=str, default="artifacts")
    args = parser.parse_args()

    project_root = Path(__file__).resolve().parents[1]
    artifacts_dir = project_root / args.artifacts_dir
    artifacts_dir.mkdir(parents=True, exist_ok=True)

    if args.input:
        input_path = Path(args.input).expanduser().resolve()
        data = pd.read_csv(input_path)
    else:
        data = generate_synthetic_data()
        (project_root / "data" / "processed").mkdir(parents=True, exist_ok=True)
        data.to_csv(project_root / "data" / "processed" / "synthetic_attrition.csv", index=False)

    if TARGET_COL not in data.columns:
        raise ValueError(f"Expected target column '{TARGET_COL}' in dataset.")

    X = data.drop(columns=[TARGET_COL])
    y = data[TARGET_COL].astype(int)

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=RANDOM_STATE, stratify=y
    )

    preprocessor = build_preprocessor(X_train)
    best_name, cv_scores, model_pipeline = select_model(X_train, y_train, preprocessor)
    y_prob = model_pipeline.predict_proba(X_test)[:, 1]

    overall_metrics = calculate_metrics(y_test, y_prob, threshold=0.5)
    by_gender = subgroup_metrics(X_test, y_test, y_prob, group_col="Gender", threshold=0.5)
    by_overtime = subgroup_metrics(X_test, y_test, y_prob, group_col="OverTime", threshold=0.5)

    predictions = X_test.copy()
    predictions["actual_attrition"] = y_test.values
    predictions["predicted_attrition_prob"] = y_prob
    predictions["predicted_attrition_label"] = (y_prob >= 0.5).astype(int)

    feature_importance = extract_feature_importance(model_pipeline)

    metrics_payload = {
        "selected_model": best_name,
        "cv_roc_auc": cv_scores,
        "test_metrics": overall_metrics,
        "subgroup_metrics": {
            "Gender": by_gender,
            "OverTime": by_overtime,
        },
    }

    with open(artifacts_dir / "metrics.json", "w", encoding="utf-8") as f:
        json.dump(metrics_payload, f, indent=2)

    predictions.to_csv(artifacts_dir / "predictions.csv", index=False)
    feature_importance.to_csv(artifacts_dir / "feature_importance.csv", index=False)
    joblib.dump(model_pipeline, artifacts_dir / "model.joblib")

    print("Training complete.")
    print(f"Selected model: {best_name}")
    print(f"Test ROC-AUC: {overall_metrics['roc_auc']:.3f}")
    print(f"Artifacts written to: {artifacts_dir}")


if __name__ == "__main__":
    main()
