"""Reusable regression pipeline template.

Adapt this for price prediction, demand forecasting with tabular features,
medical measurements, financial estimates, or any continuous target.
"""

from pathlib import Path

import joblib
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import Ridge
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
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
ARTIFACT_DIR = Path("artifacts/regression")
RANDOM_STATE = 42


def main() -> None:
    df = load_csv(DATA_PATH)
    X, y = split_features_target(df, TARGET_COLUMN)
    numeric_features, categorical_features = infer_column_types(X)

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=RANDOM_STATE
    )

    preprocessor = build_preprocessor(numeric_features, categorical_features)

    candidates = {
        "ridge": Pipeline(steps=[("preprocess", preprocessor), ("model", Ridge())]),
        "random_forest": Pipeline(
            steps=[
                ("preprocess", preprocessor),
                ("model", RandomForestRegressor(random_state=RANDOM_STATE)),
            ]
        ),
    }

    param_grid = {
        "ridge": {"model__alpha": [0.1, 1, 10, 100]},
        "random_forest": {
            "model__n_estimators": [200, 500],
            "model__max_depth": [None, 5, 10],
        },
    }

    best_name = None
    best_search = None
    best_score = float("-inf")

    for name, pipeline in candidates.items():
        search = GridSearchCV(
            pipeline,
            param_grid=param_grid[name],
            scoring="neg_mean_absolute_error",
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
        "best_cv_neg_mae": best_score,
        "test_mae": mean_absolute_error(y_test, preds),
        "test_rmse": mean_squared_error(y_test, preds, squared=False),
        "test_r2": r2_score(y_test, preds),
    }

    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    joblib.dump(model, ARTIFACT_DIR / "model.joblib")
    save_json(metrics, ARTIFACT_DIR / "metrics.json")
    pd.DataFrame({"actual": y_test, "predicted": preds}).to_csv(
        ARTIFACT_DIR / "predictions.csv", index=False
    )

    print(metrics)


if __name__ == "__main__":
    main()
