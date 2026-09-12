"""Surrogate index example for an aquarium store.

Question:
    Does a short-run treatment increase long-term customer value?

Problem:
    Long-term outcomes, like 180-day revenue, take months to observe.

Solution:
    1. Use historical customers with short-term behaviors and observed long-term value.
    2. Train a surrogate-index model: E[long_term_value | short_term_signals].
    3. Run a new experiment where treatment/control customers only have short-term signals so far.
    4. Estimate the treatment effect on the surrogate index as an early proxy for long-term impact.

Run from this project root:
    python src/surrogate_index_aquarium.py
"""

from __future__ import annotations

from pathlib import Path

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

BASELINE_FEATURES = [
    "prior_90d_spend",
    "tank_size_gallons",
    "customer_tenure_months",
    "freshwater_customer",
    "saltwater_customer",
]

SURROGATES = [
    "first_14d_spend",
    "water_test_count_30d",
    "repeat_visit_30d",
    "livestock_purchase_30d",
    "maintenance_subscription_30d",
]


def simulate_historical_customers(n: int = 12_000, seed: int = RANDOM_STATE) -> pd.DataFrame:
    """Historical customers with short-term signals and observed 180-day value."""
    rng = np.random.default_rng(seed)

    prior_90d_spend = np.clip(rng.gamma(shape=2.2, scale=35, size=n), 0, 350)
    tank_size_gallons = rng.choice([10, 20, 29, 40, 55, 75, 120], size=n, p=[0.16, 0.21, 0.18, 0.16, 0.13, 0.10, 0.06])
    customer_tenure_months = np.clip(rng.exponential(scale=16, size=n), 0, 96)
    saltwater_customer = rng.binomial(1, 0.22, size=n)
    freshwater_customer = 1 - saltwater_customer

    engagement = (
        0.010 * prior_90d_spend
        + 0.012 * tank_size_gallons
        + 0.020 * customer_tenure_months
        + 0.65 * saltwater_customer
        + rng.normal(0, 0.75, n)
    )

    first_14d_spend = np.clip(
        12 + 0.26 * prior_90d_spend + 0.28 * tank_size_gallons + 18 * saltwater_customer + rng.normal(0, 22, n),
        0,
        None,
    )
    water_test_count_30d = rng.poisson(np.clip(0.35 + 0.18 * engagement + 0.009 * tank_size_gallons, 0.05, 5.0))
    repeat_visit_30d = rng.binomial(1, 1 / (1 + np.exp(-(engagement - 1.0))))
    livestock_purchase_30d = rng.binomial(1, 1 / (1 + np.exp(-(engagement - 1.35 + 0.45 * saltwater_customer))))
    maintenance_subscription_30d = rng.binomial(1, 1 / (1 + np.exp(-(engagement - 2.1 + 0.018 * tank_size_gallons))))

    long_term_value_180d = (
        30
        + 0.34 * prior_90d_spend
        + 0.55 * tank_size_gallons
        + 1.15 * first_14d_spend
        + 26 * water_test_count_30d
        + 65 * repeat_visit_30d
        + 78 * livestock_purchase_30d
        + 165 * maintenance_subscription_30d
        + 35 * saltwater_customer
        + rng.normal(0, 45, n)
    )

    return pd.DataFrame(
        {
            "prior_90d_spend": prior_90d_spend,
            "tank_size_gallons": tank_size_gallons,
            "customer_tenure_months": customer_tenure_months,
            "freshwater_customer": freshwater_customer,
            "saltwater_customer": saltwater_customer,
            "first_14d_spend": first_14d_spend,
            "water_test_count_30d": water_test_count_30d,
            "repeat_visit_30d": repeat_visit_30d,
            "livestock_purchase_30d": livestock_purchase_30d,
            "maintenance_subscription_30d": maintenance_subscription_30d,
            "long_term_value_180d": long_term_value_180d,
        }
    )


def simulate_new_experiment(n: int = 4_000, seed: int = RANDOM_STATE + 1) -> pd.DataFrame:
    """New randomized experiment with surrogates observed but true long-term value not yet available."""
    rng = np.random.default_rng(seed)

    prior_90d_spend = np.clip(rng.gamma(shape=2.2, scale=35, size=n), 0, 350)
    tank_size_gallons = rng.choice([10, 20, 29, 40, 55, 75, 120], size=n, p=[0.16, 0.21, 0.18, 0.16, 0.13, 0.10, 0.06])
    customer_tenure_months = np.clip(rng.exponential(scale=16, size=n), 0, 96)
    saltwater_customer = rng.binomial(1, 0.22, size=n)
    freshwater_customer = 1 - saltwater_customer
    treatment = rng.binomial(1, 0.5, size=n)

    # Treatment: personalized aquarium care plan + coupon bundle.
    # It increases early spend, water testing, repeat visits, and subscriptions.
    engagement = (
        0.010 * prior_90d_spend
        + 0.012 * tank_size_gallons
        + 0.020 * customer_tenure_months
        + 0.65 * saltwater_customer
        + 0.48 * treatment
        + rng.normal(0, 0.75, n)
    )

    first_14d_spend = np.clip(
        12
        + 0.26 * prior_90d_spend
        + 0.28 * tank_size_gallons
        + 18 * saltwater_customer
        + 15 * treatment
        + rng.normal(0, 22, n),
        0,
        None,
    )
    water_test_count_30d = rng.poisson(np.clip(0.35 + 0.18 * engagement + 0.009 * tank_size_gallons, 0.05, 5.0))
    repeat_visit_30d = rng.binomial(1, 1 / (1 + np.exp(-(engagement - 1.0))))
    livestock_purchase_30d = rng.binomial(1, 1 / (1 + np.exp(-(engagement - 1.35 + 0.45 * saltwater_customer))))
    maintenance_subscription_30d = rng.binomial(1, 1 / (1 + np.exp(-(engagement - 2.1 + 0.018 * tank_size_gallons))))

    # Simulated only for validation in this portfolio project; in a real experiment this is unobserved initially.
    true_long_term_value_180d = (
        30
        + 0.34 * prior_90d_spend
        + 0.55 * tank_size_gallons
        + 1.15 * first_14d_spend
        + 26 * water_test_count_30d
        + 65 * repeat_visit_30d
        + 78 * livestock_purchase_30d
        + 165 * maintenance_subscription_30d
        + 35 * saltwater_customer
        + rng.normal(0, 45, n)
    )

    return pd.DataFrame(
        {
            "prior_90d_spend": prior_90d_spend,
            "tank_size_gallons": tank_size_gallons,
            "customer_tenure_months": customer_tenure_months,
            "freshwater_customer": freshwater_customer,
            "saltwater_customer": saltwater_customer,
            "treatment": treatment,
            "first_14d_spend": first_14d_spend,
            "water_test_count_30d": water_test_count_30d,
            "repeat_visit_30d": repeat_visit_30d,
            "livestock_purchase_30d": livestock_purchase_30d,
            "maintenance_subscription_30d": maintenance_subscription_30d,
            "true_long_term_value_180d": true_long_term_value_180d,
        }
    )


def fit_surrogate_index(historical: pd.DataFrame) -> tuple[RandomForestRegressor, pd.DataFrame]:
    """Train model that maps short-term surrogates to long-term value."""
    features = BASELINE_FEATURES + SURROGATES
    train, test = train_test_split(historical, test_size=0.30, random_state=RANDOM_STATE)

    model = RandomForestRegressor(
        n_estimators=300,
        min_samples_leaf=25,
        max_features="sqrt",
        random_state=RANDOM_STATE,
        n_jobs=-1,
    )
    model.fit(train[features], train["long_term_value_180d"])

    scored = test.copy()
    scored["surrogate_index"] = model.predict(scored[features])
    return model, scored


def estimate_treatment_effect(experiment: pd.DataFrame) -> pd.Series:
    """Difference in means between randomized treatment and control groups."""
    treated = experiment[experiment["treatment"] == 1]
    control = experiment[experiment["treatment"] == 0]
    return pd.Series(
        {
            "treated_mean": treated["surrogate_index"].mean(),
            "control_mean": control["surrogate_index"].mean(),
            "surrogate_index_ate": treated["surrogate_index"].mean() - control["surrogate_index"].mean(),
            "true_180d_ate_for_validation": treated["true_long_term_value_180d"].mean()
            - control["true_long_term_value_180d"].mean(),
            "n_treated": len(treated),
            "n_control": len(control),
        }
    )


def make_plots(validation: pd.DataFrame, experiment: pd.DataFrame, effects: pd.Series) -> None:
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

    plt.figure(figsize=(7.5, 6))
    plt.scatter(
        validation["long_term_value_180d"],
        validation["surrogate_index"],
        alpha=0.28,
        s=12,
        color="#1f77b4",
    )
    lims = [
        validation[["long_term_value_180d", "surrogate_index"]].min().min(),
        validation[["long_term_value_180d", "surrogate_index"]].max().max(),
    ]
    plt.plot(lims, lims, "--", color="black", linewidth=1)
    plt.xlabel("Observed 180-day customer value")
    plt.ylabel("Predicted surrogate index")
    plt.title("Surrogate Index Validation on Historical Customers")
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "surrogate_index_validation.png", dpi=160)
    plt.close()

    labels = ["Control", "Treatment"]
    means = [effects["control_mean"], effects["treated_mean"]]
    colors = ["#9e9e9e", "#2ca02c"]
    plt.figure(figsize=(7, 5))
    plt.bar(labels, means, color=colors)
    plt.ylabel("Average surrogate index")
    plt.title("Estimated Long-Term Impact Before 180-Day Outcome Is Observed")
    for i, value in enumerate(means):
        plt.text(i, value + 3, f"${value:,.0f}", ha="center")
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "surrogate_index_treatment_effect.png", dpi=160)
    plt.close()

    plt.figure(figsize=(8, 5))
    for group, color, label in [(0, "#9e9e9e", "Control"), (1, "#2ca02c", "Treatment")]:
        subset = experiment[experiment["treatment"] == group]
        plt.hist(subset["surrogate_index"], bins=35, alpha=0.55, color=color, label=label)
    plt.xlabel("Surrogate index: predicted 180-day customer value")
    plt.ylabel("Customers")
    plt.title("Distribution Shift in Predicted Long-Term Value")
    plt.legend()
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "surrogate_index_distribution.png", dpi=160)
    plt.close()


def main() -> None:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)

    historical = simulate_historical_customers()
    experiment = simulate_new_experiment()
    historical.to_csv(DATA_DIR / "historical_customers.csv", index=False)

    model, validation = fit_surrogate_index(historical)
    features = BASELINE_FEATURES + SURROGATES
    experiment = experiment.copy()
    experiment["surrogate_index"] = model.predict(experiment[features])
    experiment.to_csv(DATA_DIR / "experiment_surrogate_index.csv", index=False)

    rmse = np.sqrt(mean_squared_error(validation["long_term_value_180d"], validation["surrogate_index"]))
    mae = mean_absolute_error(validation["long_term_value_180d"], validation["surrogate_index"])
    r2 = r2_score(validation["long_term_value_180d"], validation["surrogate_index"])
    corr = np.corrcoef(validation["long_term_value_180d"], validation["surrogate_index"])[0, 1]

    effects = estimate_treatment_effect(experiment)
    effects.to_frame("value").to_csv(REPORT_DIR / "surrogate_treatment_effect.csv")
    make_plots(validation, experiment, effects)

    summary = f"""# Surrogate Index Results\n\nAquarium store treatment: personalized aquarium care plan + coupon bundle.\n\nSurrogate-index validation on historical customers:\n\n- RMSE: ${rmse:,.2f}\n- MAE: ${mae:,.2f}\n- R-squared: {r2:.3f}\n- Correlation: {corr:.3f}\n\nEstimated treatment effect before the 180-day outcome is available:\n\n- Treatment mean surrogate index: ${effects['treated_mean']:,.2f}\n- Control mean surrogate index: ${effects['control_mean']:,.2f}\n- Estimated ATE on surrogate index: ${effects['surrogate_index_ate']:,.2f}\n- True simulated 180-day ATE for validation: ${effects['true_180d_ate_for_validation']:,.2f}\n- Treated customers: {int(effects['n_treated']):,}\n- Control customers: {int(effects['n_control']):,}\n\nInterpretation: the treatment increases short-term behaviors that historically predict long-term value, so the surrogate index provides an early estimate of long-term impact.\n"""
    (REPORT_DIR / "model_results.md").write_text(summary)
    print(summary)


if __name__ == "__main__":
    main()
