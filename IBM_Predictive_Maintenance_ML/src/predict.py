import argparse
import json
from pathlib import Path

import joblib
import numpy as np
import pandas as pd


def main():
    parser = argparse.ArgumentParser(description="Score maintenance failure risk on new telemetry.")
    parser.add_argument("--model-path", type=str, default="artifacts/model.joblib")
    parser.add_argument("--metrics-path", type=str, default="artifacts/metrics.json")
    parser.add_argument("--input", type=str, required=True, help="Input CSV with feature columns.")
    parser.add_argument("--output", type=str, default="artifacts/scored_new_data.csv")
    args = parser.parse_args()

    project_root = Path(__file__).resolve().parents[1]
    model_path = project_root / args.model_path
    metrics_path = project_root / args.metrics_path
    input_path = Path(args.input).expanduser().resolve()
    output_path = project_root / args.output
    output_path.parent.mkdir(parents=True, exist_ok=True)

    model = joblib.load(model_path)
    with open(metrics_path, "r", encoding="utf-8") as f:
        metrics = json.load(f)
    threshold = float(metrics.get("decision_threshold", 0.5))

    frame = pd.read_csv(input_path)
    features = frame.drop(columns=["FailureNext7Days", "AssetID"], errors="ignore")
    probabilities = model.predict_proba(features)[:, 1]
    labels = (probabilities >= threshold).astype(int)

    result = frame.copy()
    result["failure_probability"] = probabilities
    result["predicted_failure"] = labels
    result["alert"] = np.where(labels == 1, "ALERT", "OK")
    result.to_csv(output_path, index=False)

    print(f"Inference complete. Threshold: {threshold}")
    print(f"Predictions saved to: {output_path}")


if __name__ == "__main__":
    main()
