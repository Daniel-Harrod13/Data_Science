import argparse
from pathlib import Path

import joblib
import pandas as pd


def main():
    parser = argparse.ArgumentParser(description="Run attrition inference on new data.")
    parser.add_argument("--model-path", type=str, default="artifacts/model.joblib")
    parser.add_argument("--input", type=str, required=True, help="Path to input CSV.")
    parser.add_argument("--output", type=str, default="artifacts/predictions_new_data.csv")
    args = parser.parse_args()

    project_root = Path(__file__).resolve().parents[1]
    model_path = project_root / args.model_path
    input_path = Path(args.input).expanduser().resolve()
    output_path = project_root / args.output
    output_path.parent.mkdir(parents=True, exist_ok=True)

    model = joblib.load(model_path)
    frame = pd.read_csv(input_path)
    features = frame.drop(columns=["Attrition"], errors="ignore")
    probabilities = model.predict_proba(features)[:, 1]
    labels = (probabilities >= 0.5).astype(int)

    result = features.copy()
    result["predicted_attrition_prob"] = probabilities
    result["predicted_attrition_label"] = labels
    result.to_csv(output_path, index=False)
    print(f"Inference complete. Predictions saved to: {output_path}")


if __name__ == "__main__":
    main()
