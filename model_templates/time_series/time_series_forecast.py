"""Reusable time-series forecasting template.

This is a lightweight baseline using lag features and rolling means. It works well
as a starting point before moving to ARIMA, Prophet, XGBoost, or deep learning.
"""

from pathlib import Path

import joblib
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error

from model_templates.utilities.ml_helpers import save_json

DATA_PATH = "data/processed/your_time_series.csv"
DATE_COLUMN = "date"
TARGET_COLUMN = "target"
ARTIFACT_DIR = Path("artifacts/time_series")
RANDOM_STATE = 42
LAGS = [1, 2, 3, 7, 14]
ROLLING_WINDOWS = [3, 7, 14]
TEST_SIZE = 0.2


def make_features(df: pd.DataFrame) -> pd.DataFrame:
    df = df.sort_values(DATE_COLUMN).copy()
    df[DATE_COLUMN] = pd.to_datetime(df[DATE_COLUMN])
    df["day_of_week"] = df[DATE_COLUMN].dt.dayofweek
    df["month"] = df[DATE_COLUMN].dt.month

    for lag in LAGS:
        df[f"lag_{lag}"] = df[TARGET_COLUMN].shift(lag)

    for window in ROLLING_WINDOWS:
        df[f"rolling_mean_{window}"] = df[TARGET_COLUMN].shift(1).rolling(window).mean()

    return df.dropna()


def main() -> None:
    df = pd.read_csv(DATA_PATH)
    featured = make_features(df)

    feature_columns = [
        col for col in featured.columns if col not in [DATE_COLUMN, TARGET_COLUMN]
    ]

    split_idx = int(len(featured) * (1 - TEST_SIZE))
    train = featured.iloc[:split_idx]
    test = featured.iloc[split_idx:]

    X_train, y_train = train[feature_columns], train[TARGET_COLUMN]
    X_test, y_test = test[feature_columns], test[TARGET_COLUMN]

    model = RandomForestRegressor(n_estimators=500, random_state=RANDOM_STATE)
    model.fit(X_train, y_train)
    preds = model.predict(X_test)

    metrics = {
        "test_mae": mean_absolute_error(y_test, preds),
        "test_rmse": mean_squared_error(y_test, preds, squared=False),
        "train_rows": len(train),
        "test_rows": len(test),
    }

    output = test[[DATE_COLUMN, TARGET_COLUMN]].copy()
    output["prediction"] = preds

    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    joblib.dump(model, ARTIFACT_DIR / "forecast_model.joblib")
    save_json(metrics, ARTIFACT_DIR / "metrics.json")
    output.to_csv(ARTIFACT_DIR / "forecast_vs_actual.csv", index=False)

    print(metrics)


if __name__ == "__main__":
    main()
