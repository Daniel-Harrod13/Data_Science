"""Reusable classification pipeline template.

Adapt this for churn, attrition, fraud, risk scoring, diagnosis, pass/fail,
or any binary/multiclass classification problem.
"""

from pathlib import Path

import joblib
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report, f1_score, roc_auc_score
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.pipeline import Pipeline

from model_templates.utilities.ml_helpers import (
    build_preprocessor,
    infer_column_types,
    load_csv,
    save_json,
    split_features_target,
)

DATA_PATH = "data/processed/your_dataset.csv"
TARGET_COLUMN = "target"
ARTIFACT_DIR = Path("artifacts/classification")
RANDOM_STATE = 42


def main() -> None:
    df = load_csv(DATA_PATH)
    X, y = split_features_target(df, TARGET_COLUMN)
    numeric_features, categorical_features = infer_column_types(X)

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=RANDOM_STATE, stratify=y
    )

    preprocessor = build_preprocessor(numeric_features, categorical_features)

    candidates = {
        "logistic_regression": Pipeline(
            steps=[
                ("preprocess", preprocessor),
                ("model", LogisticRegression(max_iter=1000, class_weight="balanced")),
            ]
        ),
        "random_forest": Pipeline(
            steps=[
                ("preprocess", preprocessor),
                ("model", RandomForestClassifier(random_state=RANDOM_STATE, class_weight="balanced")),
            ]
        ),
    }

    param_grid = {
        "logistic_regression": {"model__C": [0.1, 1, 10]},
        "random_forest": {
            "model__n_estimators": [200, 500],
            "model__max_depth": [None, 5, 10],
        },
    }

    best_name = None
    best_search = None
    best_score = -1

    for name, pipeline in candidates.items():
        search = GridSearchCV(
            pipeline,
            param_grid=param_grid[name],
            scoring="f1_weighted",
            cv=5,
            n_jobs=-1,
        )
        search.fit(X_train, y_train)
        if search.best_score_ > best_score:
            best_name = name
            best_search = search
            best_score = search.best_score_

    model = best_search.best_estimator_
    preds = model.predict(X_test)

    metrics = {
        "best_model": best_name,
        "best_cv_f1_weighted": best_score,
        "test_accuracy": accuracy_score(y_test, preds),
        "test_f1_weighted": f1_score(y_test, preds, average="weighted"),
        "classification_report": classification_report(y_test, preds, output_dict=True),
    }

    if hasattr(model, "predict_proba") and y.nunique() == 2:
        proba = model.predict_proba(X_test)[:, 1]
        metrics["test_roc_auc"] = roc_auc_score(y_test, proba)

    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    joblib.dump(model, ARTIFACT_DIR / "model.joblib")
    save_json(metrics, ARTIFACT_DIR / "metrics.json")
    pd.DataFrame({"actual": y_test, "predicted": preds}).to_csv(
        ARTIFACT_DIR / "predictions.csv", index=False
    )

    print(metrics)


if __name__ == "__main__":
    main()
