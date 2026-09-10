"""Reusable clustering template.

Use this for segmentation when you do not have a target variable.
Examples: customer groups, airport/entity risk profiles, survey response patterns.
"""

from pathlib import Path

import joblib
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.pipeline import Pipeline

from model_templates.utilities.ml_helpers import build_preprocessor, infer_column_types, load_csv, save_json

DATA_PATH = "data/processed/your_dataset.csv"
ID_COLUMNS = []  # Example: ["customer_id"]
ARTIFACT_DIR = Path("artifacts/clustering")
RANDOM_STATE = 42
K_VALUES = range(2, 9)


def main() -> None:
    df = load_csv(DATA_PATH)
    X = df.drop(columns=ID_COLUMNS, errors="ignore")
    numeric_features, categorical_features = infer_column_types(X)
    preprocessor = build_preprocessor(numeric_features, categorical_features)

    best_k = None
    best_score = -1
    best_model = None
    scores = {}

    for k in K_VALUES:
        pipeline = Pipeline(
            steps=[
                ("preprocess", preprocessor),
                ("model", KMeans(n_clusters=k, n_init="auto", random_state=RANDOM_STATE)),
            ]
        )
        labels = pipeline.fit_predict(X)
        transformed = pipeline.named_steps["preprocess"].transform(X)
        score = silhouette_score(transformed, labels)
        scores[str(k)] = score

        if score > best_score:
            best_k = k
            best_score = score
            best_model = pipeline

    labels = best_model.predict(X)
    output = df.copy()
    output["cluster"] = labels

    metrics = {"best_k": best_k, "best_silhouette_score": best_score, "all_scores": scores}

    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    joblib.dump(best_model, ARTIFACT_DIR / "kmeans_model.joblib")
    save_json(metrics, ARTIFACT_DIR / "metrics.json")
    output.to_csv(ARTIFACT_DIR / "clustered_data.csv", index=False)

    print(metrics)


if __name__ == "__main__":
    main()
