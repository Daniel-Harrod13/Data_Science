"""Random-forest uplift modeling with simulated treatment/control data.

This project uses a T-learner:
1. Train one RandomForestRegressor on treated observations.
2. Train one RandomForestRegressor on control observations.
3. Estimate CATE(x) = E[Y|T=1, X=x] - E[Y|T=0, X=x].

Run from the project root:
    python src/uplift_random_forest.py
"""

from __future__ import annotations

from pathlib import Path
from typing import Tuple

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import train_test_split

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = PROJECT_ROOT / "data"
ARTIFACT_DIR = PROJECT_ROOT / "artifacts"
REPORT_DIR = PROJECT_ROOT / "reports"
RANDOM_STATE = 42

FEATURES = [
    "age",
    "income_k",
    "prior_purchases",
    "site_visits_30d",
    "email_engagement",
    "is_loyalty_member",
]


def simulate_customer_data(n: int = 10_000, seed: int = RANDOM_STATE) -> pd.DataFrame:
    """Simulate a marketing experiment with heterogeneous treatment effects."""
    rng = np.random.default_rng(seed)

    age = np.clip(rng.normal(42, 13, n), 18, 80)
    income_k = np.clip(rng.lognormal(mean=4.0, sigma=0.45, size=n), 20, 180)
    prior_purchases = rng.poisson(lam=2.4, size=n)
    site_visits_30d = rng.poisson(lam=5.5, size=n)
    email_engagement = rng.beta(a=2.0, b=5.0, size=n)
    is_loyalty_member = rng.binomial(1, 0.35, size=n)

    # Randomized treatment assignment. In a real project this could be an A/B test flag.
    treatment = rng.binomial(1, 0.5, size=n)

    # Baseline outcome without treatment: expected purchase value.
    baseline = (
        35
        + 0.42 * income_k
        + 5.0 * prior_purchases
        + 1.8 * site_visits_30d
        + 42 * email_engagement
        + 18 * is_loyalty_member
        - 0.12 * age
        + rng.normal(0, 18, n)
    )

    # True heterogeneous treatment effect.
    # The campaign works best for engaged, frequent visitors and loyalty members,
    # but can be less effective for customers with very high prior purchases.
    true_cate = (
        4
        + 35 * email_engagement
        + 2.3 * site_visits_30d
        + 10 * is_loyalty_member
        - 1.7 * prior_purchases
        + 0.04 * (income_k - 60)
        - 0.08 * np.maximum(age - 55, 0)
    )

    outcome = baseline + treatment * true_cate

    return pd.DataFrame(
        {
            "age": age,
            "income_k": income_k,
            "prior_purchases": prior_purchases,
            "site_visits_30d": site_visits_30d,
            "email_engagement": email_engagement,
            "is_loyalty_member": is_loyalty_member,
            "treatment": treatment,
            "outcome": outcome,
            "true_cate": true_cate,
        }
    )


def fit_t_learner(train: pd.DataFrame) -> Tuple[RandomForestRegressor, RandomForestRegressor]:
    """Fit separate outcome models for treated and control units."""
    rf_params = dict(
        n_estimators=300,
        min_samples_leaf=20,
        max_features="sqrt",
        random_state=RANDOM_STATE,
        n_jobs=-1,
    )
    treated_model = RandomForestRegressor(**rf_params)
    control_model = RandomForestRegressor(**rf_params)

    treated = train[train["treatment"] == 1]
    control = train[train["treatment"] == 0]

    treated_model.fit(treated[FEATURES], treated["outcome"])
    control_model.fit(control[FEATURES], control["outcome"])
    return treated_model, control_model


def predict_cate(
    treated_model: RandomForestRegressor,
    control_model: RandomForestRegressor,
    frame: pd.DataFrame,
) -> np.ndarray:
    """Estimate conditional average treatment effect for each row."""
    y1_hat = treated_model.predict(frame[FEATURES])
    y0_hat = control_model.predict(frame[FEATURES])
    return y1_hat - y0_hat


def uplift_by_decile(frame: pd.DataFrame) -> pd.DataFrame:
    """Summarize true and predicted uplift by predicted-CATE decile."""
    ranked = frame.copy()
    ranked["uplift_decile"] = pd.qcut(
        ranked["pred_cate"].rank(method="first"), 10, labels=False
    ) + 1
    ranked["uplift_decile"] = 11 - ranked["uplift_decile"]  # 1 = highest predicted uplift

    return (
        ranked.groupby("uplift_decile")
        .agg(
            customers=("pred_cate", "size"),
            avg_pred_cate=("pred_cate", "mean"),
            avg_true_cate=("true_cate", "mean"),
            avg_outcome=("outcome", "mean"),
        )
        .reset_index()
    )


def make_plots(test: pd.DataFrame, deciles: pd.DataFrame) -> None:
    """Create portfolio-ready evaluation charts."""
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

    plt.figure(figsize=(8, 6))
    colors = {"Would treat": "#2ca02c", "Excluded": "#9e9e9e"}
    for policy_group, color in colors.items():
        subset = test[test["policy_group"] == policy_group]
        plt.scatter(
            subset["true_cate"],
            subset["pred_cate"],
            alpha=0.45,
            s=18,
            color=color,
            label=f"{policy_group} (n={len(subset):,})",
        )
    lims = [test[["true_cate", "pred_cate"]].min().min(), test[["true_cate", "pred_cate"]].max().max()]
    plt.plot(lims, lims, "--", color="black", linewidth=1, label="Perfect prediction")
    plt.axhline(
        test["treatment_policy_threshold"].iloc[0],
        color="#2ca02c",
        linestyle=":",
        linewidth=1.5,
        label="Treatment threshold",
    )
    plt.xlabel("True treatment effect, tau(x)")
    plt.ylabel("Predicted CATE")
    plt.title("Predicted CATE vs. True Tau by Targeting Decision")
    plt.legend(frameon=True)
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "predicted_vs_true_cate.png", dpi=160)
    plt.close()

    plt.figure(figsize=(9, 5))
    plt.plot(deciles["uplift_decile"], deciles["avg_true_cate"], marker="o", label="True CATE")
    plt.plot(deciles["uplift_decile"], deciles["avg_pred_cate"], marker="o", label="Predicted CATE")
    plt.gca().invert_xaxis()
    plt.xlabel("Predicted uplift decile (1 = highest predicted uplift)")
    plt.ylabel("Average treatment effect")
    plt.title("Uplift Ranking by Decile")
    plt.legend()
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "uplift_by_decile.png", dpi=160)
    plt.close()


def main() -> None:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)

    data = simulate_customer_data()
    data.to_csv(DATA_DIR / "simulated_uplift_data.csv", index=False)

    train, test = train_test_split(data, test_size=0.30, random_state=RANDOM_STATE, stratify=data["treatment"])
    treated_model, control_model = fit_t_learner(train)

    test = test.copy()
    test["pred_cate"] = predict_cate(treated_model, control_model, test)

    # Example targeting policy: treat the top 30% of customers by predicted uplift.
    treatment_policy_threshold = test["pred_cate"].quantile(0.70)
    test["treatment_policy_threshold"] = treatment_policy_threshold
    test["would_treat"] = test["pred_cate"] >= treatment_policy_threshold
    test["policy_group"] = np.where(test["would_treat"], "Would treat", "Excluded")
    test.to_csv(DATA_DIR / "test_predictions.csv", index=False)

    rmse = np.sqrt(mean_squared_error(test["true_cate"], test["pred_cate"]))
    mae = mean_absolute_error(test["true_cate"], test["pred_cate"])
    r2 = r2_score(test["true_cate"], test["pred_cate"])
    corr = np.corrcoef(test["true_cate"], test["pred_cate"])[0, 1]

    deciles = uplift_by_decile(test)
    deciles.to_csv(REPORT_DIR / "uplift_deciles.csv", index=False)
    make_plots(test, deciles)

    top_decile = deciles.loc[deciles["uplift_decile"] == 1, "avg_true_cate"].iloc[0]
    bottom_decile = deciles.loc[deciles["uplift_decile"] == 10, "avg_true_cate"].iloc[0]
    would_treat_count = int(test["would_treat"].sum())
    excluded_count = int((~test["would_treat"]).sum())

    summary = f"""# Model Results\n\nRandom Forest T-learner performance on holdout data:\n\n- CATE RMSE: {rmse:.2f}\n- CATE MAE: {mae:.2f}\n- CATE R-squared: {r2:.3f}\n- CATE correlation: {corr:.3f}\n- True uplift in top predicted decile: {top_decile:.2f}\n- True uplift in bottom predicted decile: {bottom_decile:.2f}\n\nTargeting policy:\n\n- Treat customers with predicted CATE >= {treatment_policy_threshold:.2f}\n- Would treat: {would_treat_count:,} customers\n- Excluded: {excluded_count:,} customers\n\nA positive gap between top and bottom deciles indicates the model can rank customers by expected incremental impact.\n"""
    (REPORT_DIR / "model_results.md").write_text(summary)
    print(summary)


if __name__ == "__main__":
    main()
