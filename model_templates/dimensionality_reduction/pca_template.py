"""Reusable PCA template.

Use PCA for dimensionality reduction, visualization, noise reduction, or as a
preprocessing step before modeling.
"""

from pathlib import Path

import joblib
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.pipeline import Pipeline

from model_templates.utilities.ml_helpers import build_preprocessor, infer_column_types, load_csv, save_json

DATA_PATH = "data/processed/your_dataset.csv"
TARGET_COLUMN = None  # Set to a column name if present and should be excluded
N_COMPONENTS = 2
ARTIFACT_DIR = Path("artifacts/pca")


def main() -> None:
    df = load_csv(DATA_PATH)
    X = df.drop(columns=[TARGET_COLUMN], errors="ignore") if TARGET_COLUMN else df.copy()

    numeric_features, categorical_features = infer_column_types(X)
    preprocessor = build_preprocessor(numeric_features, categorical_features)

    pipeline = Pipeline(
        steps=[
            ("preprocess", preprocessor),
            ("pca", PCA(n_components=N_COMPONENTS)),
        ]
    )

    components = pipeline.fit_transform(X)
    component_columns = [f"PC{i + 1}" for i in range(N_COMPONENTS)]
    output = pd.DataFrame(components, columns=component_columns)

    if TARGET_COLUMN and TARGET_COLUMN in df.columns:
        output[TARGET_COLUMN] = df[TARGET_COLUMN].values

    pca = pipeline.named_steps["pca"]
    metrics = {
        "n_components": N_COMPONENTS,
        "explained_variance_ratio": pca.explained_variance_ratio_.tolist(),
        "total_explained_variance": float(pca.explained_variance_ratio_.sum()),
    }

    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    joblib.dump(pipeline, ARTIFACT_DIR / "pca_pipeline.joblib")
    save_json(metrics, ARTIFACT_DIR / "metrics.json")
    output.to_csv(ARTIFACT_DIR / "pca_components.csv", index=False)

    print(metrics)


if __name__ == "__main__":
    main()
