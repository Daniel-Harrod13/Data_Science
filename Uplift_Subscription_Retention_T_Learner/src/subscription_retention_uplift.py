"""Subscription-retention uplift modeling with a Random Forest T-learner.

Scenario
--------
A streaming subscription business runs a randomized retention experiment. Half of
eligible subscribers receive a proactive save offer before renewal. The outcome is
90-day retained revenue. The goal is not to predict who will renew anyway; the
goal is to rank subscribers by incremental revenue caused by the offer.

Run from the project root:
    python src/subscription_retention_uplift.py
"""

from __future__ import annotations

from pathlib import Path
from typing import Tuple

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.ensemble import RandomForestRegressor
from sklearn.inspection import permutation_importance
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import train_test_split

from t_learner import TLearner

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = PROJECT_ROOT / "data"
ARTIFACT_DIR = PROJECT_ROOT / "artifacts"
REPORT_DIR = PROJECT_ROOT / "reports"
RANDOM_STATE = 474

FEATURES = [
    "monthly_fee",
    "tenure_months",
    "usage_hours_30d",
    "support_tickets_90d",
    "days_since_last_login",
    "plan_tier",
    "autopay",
    "price_sensitivity",
    "prior_discounts",
]


def simulate_subscription_experiment(n: int = 12_000, seed: int = RANDOM_STATE) -> pd.DataFrame:
    """Simulate a randomized retention-offer experiment with known CATE."""
    rng = np.random.default_rng(seed)

    plan_tier = rng.choice([0, 1, 2], size=n, p=[0.46, 0.38, 0.16])  # basic, standard, premium
    monthly_fee = np.clip(
        rng.normal(16 + 8 * plan_tier, 3.2 + 0.8 * plan_tier, n),
        8,
        45,
    )
    tenure_months = np.clip(rng.gamma(shape=2.3, scale=10.5, size=n), 1, 84)
    usage_hours_30d = np.clip(
        rng.gamma(shape=2.1 + 0.25 * plan_tier, scale=8.0, size=n),
        0,
        95,
    )
    support_tickets_90d = rng.poisson(lam=0.35 + 0.025 * np.maximum(monthly_fee - 18, 0), size=n)
    days_since_last_login = np.clip(rng.gamma(shape=1.8, scale=7.5, size=n), 0, 60)
    autopay = rng.binomial(1, 1 / (1 + np.exp(-(0.7 + 0.03 * tenure_months - 0.05 * monthly_fee))), size=n)
    price_sensitivity = rng.beta(
        a=1.8 + 0.035 * monthly_fee + 0.35 * (plan_tier == 0),
        b=3.2 + 0.012 * tenure_months,
        size=n,
    )
    prior_discounts = rng.poisson(lam=np.clip(0.25 + 1.1 * price_sensitivity - 0.012 * tenure_months, 0.05, 2.3), size=n)

    treatment = rng.binomial(1, 0.5, size=n)

    # Baseline retained 90-day revenue without a proactive offer.
    baseline_retention_prob = 1 / (
        1
        + np.exp(
            -(
                -0.85
                + 0.045 * tenure_months
                + 0.070 * usage_hours_30d
                - 0.080 * days_since_last_login
                - 0.300 * support_tickets_90d
                + 0.550 * autopay
                - 1.050 * price_sensitivity
                + 0.180 * plan_tier
            )
        )
    )
    gross_90d_revenue = 3 * monthly_fee
    baseline = gross_90d_revenue * baseline_retention_prob + rng.normal(0, 4.5, n)

    # True treatment effect: strongest for price-sensitive subscribers who still
    # use the service, weaker for loyal/autopay users, and sometimes negative for
    # already-stable premium users because the offer gives away margin.
    true_cate = (
        1.8
        + 22.0 * price_sensitivity
        + 0.42 * usage_hours_30d
        - 0.34 * days_since_last_login
        + 2.3 * support_tickets_90d
        - 5.8 * autopay
        - 0.13 * tenure_months
        - 2.1 * plan_tier
        - 3.0 * prior_discounts
        - 0.11 * monthly_fee
        + 6.0 * ((price_sensitivity > 0.58) & (usage_hours_30d > 12))
    )

    outcome = baseline + treatment * true_cate + rng.normal(0, 3.0, n)

    return pd.DataFrame(
        {
            "monthly_fee": monthly_fee,
            "tenure_months": tenure_months,
            "usage_hours_30d": usage_hours_30d,
            "support_tickets_90d": support_tickets_90d,
            "days_since_last_login": days_since_last_login,
            "plan_tier": plan_tier,
            "autopay": autopay,
            "price_sensitivity": price_sensitivity,
            "prior_discounts": prior_discounts,
            "treatment": treatment,
            "outcome_90d_revenue": outcome,
            "true_cate": true_cate,
        }
    )


def fit_uplift_model(train: pd.DataFrame) -> TLearner:
    """Adapt the reusable T-learner template with a stronger RF base model."""
    base_rf = RandomForestRegressor(
        n_estimators=350,
        min_samples_leaf=25,
        max_features="sqrt",
        random_state=RANDOM_STATE,
        n_jobs=-1,
    )
    return TLearner(estimator=base_rf).fit(
        train[FEATURES], train["treatment"], train["outcome_90d_revenue"]
    )


def summarize_by_decile(frame: pd.DataFrame) -> pd.DataFrame:
    """Summarize uplift ranking quality by predicted-CATE decile."""
    ranked = frame.copy()
    ranked["uplift_decile"] = pd.qcut(
        ranked["pred_cate"].rank(method="first"), 10, labels=False
    ) + 1
    ranked["uplift_decile"] = 11 - ranked["uplift_decile"]
    return (
        ranked.groupby("uplift_decile")
        .agg(
            subscribers=("pred_cate", "size"),
            avg_pred_cate=("pred_cate", "mean"),
            avg_true_cate=("true_cate", "mean"),
            avg_90d_revenue=("outcome_90d_revenue", "mean"),
            treatment_rate=("treatment", "mean"),
        )
        .reset_index()
    )


def policy_gain_curve(frame: pd.DataFrame) -> pd.DataFrame:
    """Compute cumulative true uplift captured as more subscribers are targeted."""
    ranked = frame.sort_values("pred_cate", ascending=False).reset_index(drop=True)
    ranked["rank"] = np.arange(1, len(ranked) + 1)
    ranked["targeted_pct"] = ranked["rank"] / len(ranked)
    ranked["cum_true_uplift"] = ranked["true_cate"].cumsum()
    total_positive_oracle = ranked.loc[ranked["true_cate"] > 0, "true_cate"].sum()
    ranked["share_of_oracle_positive_uplift"] = ranked["cum_true_uplift"] / total_positive_oracle
    return ranked[["targeted_pct", "cum_true_uplift", "share_of_oracle_positive_uplift"]]


def make_plots(test: pd.DataFrame, deciles: pd.DataFrame, gains: pd.DataFrame, model: TLearner) -> None:
    """Create polished, portfolio-ready visuals."""
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    sns.set_theme(style="whitegrid", context="talk")
    palette = {"Offer": "#2A9D8F", "Do not offer": "#8D99AE"}

    fig, ax = plt.subplots(figsize=(10, 7))
    sns.scatterplot(
        data=test.sample(min(len(test), 2500), random_state=RANDOM_STATE),
        x="true_cate",
        y="pred_cate",
        hue="policy_group",
        palette=palette,
        alpha=0.58,
        s=36,
        edgecolor="none",
        ax=ax,
    )
    lims = [test[["true_cate", "pred_cate"]].min().min(), test[["true_cate", "pred_cate"]].max().max()]
    ax.plot(lims, lims, color="#1f2937", linestyle="--", linewidth=1.3, label="Perfect prediction")
    ax.axhline(test["offer_threshold"].iloc[0], color="#E76F51", linestyle=":", linewidth=2.2, label="Offer threshold")
    ax.set_title("Predicted vs. True Subscriber-Level Uplift", weight="bold", pad=14)
    ax.set_xlabel("True incremental 90-day revenue from offer ($)")
    ax.set_ylabel("Predicted CATE / uplift ($)")
    ax.legend(frameon=True, title="Policy decision")
    sns.despine()
    fig.tight_layout()
    fig.savefig(ARTIFACT_DIR / "predicted_vs_true_uplift.png", dpi=180)
    plt.close(fig)

    long_deciles = deciles.melt(
        id_vars="uplift_decile",
        value_vars=["avg_true_cate", "avg_pred_cate"],
        var_name="metric",
        value_name="avg_uplift",
    )
    long_deciles["metric"] = long_deciles["metric"].map(
        {"avg_true_cate": "True uplift", "avg_pred_cate": "Predicted uplift"}
    )
    fig, ax = plt.subplots(figsize=(11, 6))
    sns.lineplot(
        data=long_deciles,
        x="uplift_decile",
        y="avg_uplift",
        hue="metric",
        marker="o",
        linewidth=3,
        markersize=9,
        palette=["#264653", "#E76F51"],
        ax=ax,
    )
    ax.invert_xaxis()
    ax.axhline(0, color="#6b7280", linewidth=1)
    ax.set_title("Uplift Ranking by Predicted Decile", weight="bold", pad=14)
    ax.set_xlabel("Predicted uplift decile (1 = highest predicted uplift)")
    ax.set_ylabel("Average incremental 90-day revenue ($)")
    ax.legend(title="")
    sns.despine()
    fig.tight_layout()
    fig.savefig(ARTIFACT_DIR / "uplift_by_decile.png", dpi=180)
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(10, 6))
    plot_gains = gains.iloc[:: max(1, len(gains) // 200)].copy()
    sns.lineplot(
        data=plot_gains,
        x="targeted_pct",
        y="share_of_oracle_positive_uplift",
        color="#2A9D8F",
        linewidth=3.2,
        ax=ax,
    )
    ax.axvline(0.25, color="#E76F51", linestyle=":", linewidth=2.2, label="Chosen top-25% policy")
    ax.set_title("Cumulative Uplift Captured by Targeting Depth", weight="bold", pad=14)
    ax.set_xlabel("Share of subscribers receiving offer")
    ax.set_ylabel("Share of oracle positive uplift captured")
    ax.set_xlim(0, 1)
    ax.set_ylim(0, max(1.02, plot_gains["share_of_oracle_positive_uplift"].max() * 1.03))
    ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda y, _: f"{y:.0%}"))
    ax.xaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: f"{x:.0%}"))
    ax.legend(frameon=True)
    sns.despine()
    fig.tight_layout()
    fig.savefig(ARTIFACT_DIR / "policy_gain_curve.png", dpi=180)
    plt.close(fig)

    # Permutation importance on the estimated CATE, not the observed outcome.
    cate_proxy = model.predict(test[FEATURES])
    importance_model = RandomForestRegressor(
        n_estimators=250,
        min_samples_leaf=30,
        max_features="sqrt",
        random_state=RANDOM_STATE,
        n_jobs=-1,
    ).fit(test[FEATURES], cate_proxy)
    importances = permutation_importance(
        importance_model,
        test[FEATURES],
        cate_proxy,
        n_repeats=8,
        random_state=RANDOM_STATE,
        n_jobs=-1,
    )
    imp = (
        pd.DataFrame({"feature": FEATURES, "importance": importances.importances_mean})
        .sort_values("importance", ascending=True)
    )
    fig, ax = plt.subplots(figsize=(10, 6.5))
    sns.barplot(data=imp, x="importance", y="feature", color="#457B9D", ax=ax)
    ax.set_title("Main Drivers of Predicted Offer Uplift", weight="bold", pad=14)
    ax.set_xlabel("Permutation importance for predicted CATE")
    ax.set_ylabel("")
    sns.despine()
    fig.tight_layout()
    fig.savefig(ARTIFACT_DIR / "cate_feature_importance.png", dpi=180)
    plt.close(fig)


def main() -> None:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)

    data = simulate_subscription_experiment()
    data.to_csv(DATA_DIR / "simulated_subscription_retention_experiment.csv", index=False)

    train, test = train_test_split(
        data,
        test_size=0.30,
        random_state=RANDOM_STATE,
        stratify=data["treatment"],
    )

    model = fit_uplift_model(train)
    test = test.copy()
    test["pred_cate"] = model.predict(test[FEATURES])

    # Business policy: save-offer budget covers the top 25% by predicted uplift.
    offer_threshold = test["pred_cate"].quantile(0.75)
    test["offer_threshold"] = offer_threshold
    test["would_offer"] = test["pred_cate"] >= offer_threshold
    test["policy_group"] = np.where(test["would_offer"], "Offer", "Do not offer")
    test.to_csv(DATA_DIR / "holdout_policy_predictions.csv", index=False)

    rmse = np.sqrt(mean_squared_error(test["true_cate"], test["pred_cate"]))
    mae = mean_absolute_error(test["true_cate"], test["pred_cate"])
    r2 = r2_score(test["true_cate"], test["pred_cate"])
    corr = np.corrcoef(test["true_cate"], test["pred_cate"])[0, 1]

    deciles = summarize_by_decile(test)
    gains = policy_gain_curve(test)
    deciles.to_csv(REPORT_DIR / "uplift_deciles.csv", index=False)
    gains.to_csv(REPORT_DIR / "policy_gain_curve.csv", index=False)
    make_plots(test, deciles, gains, model)

    top_decile = deciles.loc[deciles["uplift_decile"] == 1, "avg_true_cate"].iloc[0]
    bottom_decile = deciles.loc[deciles["uplift_decile"] == 10, "avg_true_cate"].iloc[0]
    policy_true_value = test.loc[test["would_offer"], "true_cate"].sum()
    random_same_size_value = test["would_offer"].mean() * test["true_cate"].sum()
    incremental_gain_vs_random = policy_true_value - random_same_size_value
    targeted_count = int(test["would_offer"].sum())
    excluded_count = int((~test["would_offer"]).sum())
    oracle_share_at_25 = gains.iloc[targeted_count - 1]["share_of_oracle_positive_uplift"]

    summary = f"""# Model Results\n\nRandom Forest T-learner performance on holdout subscribers:\n\n- CATE RMSE: {rmse:.2f}\n- CATE MAE: {mae:.2f}\n- CATE R-squared: {r2:.3f}\n- CATE correlation: {corr:.3f}\n- True uplift in top predicted decile: ${top_decile:.2f}\n- True uplift in bottom predicted decile: ${bottom_decile:.2f}\n\nTargeting policy:\n\n- Offer threshold: predicted CATE >= ${offer_threshold:.2f}\n- Offer group: {targeted_count:,} subscribers\n- Do-not-offer group: {excluded_count:,} subscribers\n- True incremental value captured by top-25% policy: ${policy_true_value:,.0f}\n- Incremental value vs. random same-size targeting: ${incremental_gain_vs_random:,.0f}\n- Share of oracle positive uplift captured at 25% targeting depth: {oracle_share_at_25:.1%}\n\nInterpretation: the model separates persuadable, price-sensitive subscribers from subscribers who either would renew anyway or are too disengaged to save profitably.\n"""
    (REPORT_DIR / "model_results.md").write_text(summary)
    print(summary)


if __name__ == "__main__":
    main()
