"""Switchback experiment simulation for Air Force recruiting ad spend.

Question:
    Can an Air Force recruiting team reduce paid media bids without materially
    reducing total qualified recruiting leads?

Motivation:
    Platform dashboards can over-credit paid ads because some applicants would have
    searched organically anyway. A switchback experiment randomizes bid policy over
    region-days and measures first-party total qualified leads instead of relying on
    platform-attributed conversions.

Run from this project root:
    python src/airforce_recruiting_switchback.py
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = PROJECT_ROOT / "data"
ARTIFACT_DIR = PROJECT_ROOT / "artifacts"
REPORT_DIR = PROJECT_ROOT / "reports"
RANDOM_STATE = 42


def simulate_switchback_data(n_regions: int = 14, n_days: int = 56, seed: int = RANDOM_STATE) -> pd.DataFrame:
    """Simulate a region-day switchback experiment for recruiting ad bids."""
    rng = np.random.default_rng(seed)
    dates = pd.date_range("2026-01-05", periods=n_days, freq="D")
    regions = [f"Recruiting_Region_{i:02d}" for i in range(1, n_regions + 1)]

    region_strength = dict(zip(regions, rng.normal(0, 0.18, n_regions)))
    region_population = dict(zip(regions, rng.uniform(0.75, 1.45, n_regions)))

    rows = []
    for region in regions:
        # Balanced randomized switchback schedule within each region.
        assignment = np.array([0, 1] * (n_days // 2))
        if n_days % 2:
            assignment = np.append(assignment, rng.integers(0, 2))
        rng.shuffle(assignment)

        for day_idx, date in enumerate(dates):
            treatment = int(assignment[day_idx])  # 1 = reduced-bid policy
            dow = date.day_name()
            weekend = int(date.dayofweek >= 5)
            national_trend = 0.020 * day_idx
            exam_season = 0.25 * np.sin(2 * np.pi * day_idx / 28)

            baseline_interest = (
                22
                * region_population[region]
                * np.exp(region_strength[region] + national_trend + exam_season - 0.18 * weekend)
            )

            # Reduced bids lower impressions/clicks substantially.
            ad_spend = rng.normal(1_250 * region_population[region] * (1 - 0.48 * treatment), 95)
            ad_spend = max(ad_spend, 120)
            impressions = rng.normal(80_000 * region_population[region] * (1 - 0.42 * treatment), 5_500)
            clicks = rng.normal(1_900 * region_population[region] * (1 - 0.36 * treatment), 130)

            # Platform attribution falls sharply because fewer paid clicks happen.
            platform_attributed_leads = rng.poisson(max(3, 0.022 * clicks * (1 - 0.18 * treatment)))

            # True total qualified leads barely move because paid ads partly cannibalize organic search.
            # Reduced bids do cause some loss, but much smaller than platform attribution implies.
            true_incremental_loss = 1.15 * treatment * region_population[region]
            total_qualified_leads = rng.poisson(max(2, baseline_interest - true_incremental_loss))

            rows.append(
                {
                    "date": date,
                    "region": region,
                    "day_of_week": dow,
                    "reduced_bid_policy": treatment,
                    "ad_spend": ad_spend,
                    "impressions": impressions,
                    "clicks": clicks,
                    "platform_attributed_leads": platform_attributed_leads,
                    "total_qualified_leads": total_qualified_leads,
                    "organic_plus_direct_leads": max(total_qualified_leads - platform_attributed_leads, 0),
                }
            )

    df = pd.DataFrame(rows)
    df["estimated_cost_per_qualified_lead"] = df["ad_spend"] / df["total_qualified_leads"].clip(lower=1)
    return df


def analyze_experiment(df: pd.DataFrame) -> tuple[object, object, pd.DataFrame]:
    """Estimate switchback effects with region and day-of-week fixed effects."""
    spend_model = smf.ols(
        "ad_spend ~ reduced_bid_policy + C(region) + C(day_of_week)", data=df
    ).fit(cov_type="cluster", cov_kwds={"groups": df["region"]})

    leads_model = smf.ols(
        "total_qualified_leads ~ reduced_bid_policy + C(region) + C(day_of_week)", data=df
    ).fit(cov_type="cluster", cov_kwds={"groups": df["region"]})

    attributed_model = smf.ols(
        "platform_attributed_leads ~ reduced_bid_policy + C(region) + C(day_of_week)", data=df
    ).fit(cov_type="cluster", cov_kwds={"groups": df["region"]})

    daily = (
        df.groupby(["date", "reduced_bid_policy"])
        .agg(
            ad_spend=("ad_spend", "sum"),
            total_qualified_leads=("total_qualified_leads", "sum"),
            platform_attributed_leads=("platform_attributed_leads", "sum"),
        )
        .reset_index()
    )
    daily["cost_per_qualified_lead"] = daily["ad_spend"] / daily["total_qualified_leads"]

    effects = pd.DataFrame(
        {
            "metric": ["ad_spend", "total_qualified_leads", "platform_attributed_leads"],
            "switchback_effect": [
                spend_model.params["reduced_bid_policy"],
                leads_model.params["reduced_bid_policy"],
                attributed_model.params["reduced_bid_policy"],
            ],
            "p_value": [
                spend_model.pvalues["reduced_bid_policy"],
                leads_model.pvalues["reduced_bid_policy"],
                attributed_model.pvalues["reduced_bid_policy"],
            ],
        }
    )
    return leads_model, spend_model, effects


def permutation_test(df: pd.DataFrame, n_permutations: int = 2000, seed: int = RANDOM_STATE) -> tuple[float, np.ndarray, float]:
    """Randomization inference by shuffling treatment labels within each recruiting region."""
    rng = np.random.default_rng(seed)
    observed = df.loc[df["reduced_bid_policy"] == 1, "total_qualified_leads"].mean() - df.loc[
        df["reduced_bid_policy"] == 0, "total_qualified_leads"
    ].mean()

    null_effects = np.zeros(n_permutations)
    for i in range(n_permutations):
        shuffled = df.copy()
        shuffled["perm_treatment"] = shuffled.groupby("region")["reduced_bid_policy"].transform(
            lambda x: rng.permutation(x.to_numpy())
        )
        null_effects[i] = shuffled.loc[shuffled["perm_treatment"] == 1, "total_qualified_leads"].mean() - shuffled.loc[
            shuffled["perm_treatment"] == 0, "total_qualified_leads"
        ].mean()

    p_value = np.mean(np.abs(null_effects) >= abs(observed))
    return observed, null_effects, p_value


def make_plots(df: pd.DataFrame, effects: pd.DataFrame, null_effects: np.ndarray, observed_perm_effect: float) -> None:
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

    by_policy = df.groupby("reduced_bid_policy").agg(
        ad_spend=("ad_spend", "mean"),
        total_qualified_leads=("total_qualified_leads", "mean"),
        platform_attributed_leads=("platform_attributed_leads", "mean"),
    )
    labels = ["Baseline bids", "Reduced bids"]

    plt.figure(figsize=(8, 5))
    plt.bar(labels, by_policy["ad_spend"], color=["#4c78a8", "#2ca02c"])
    plt.ylabel("Average daily ad spend per region")
    plt.title("Reduced-Bid Switchbacks Cut Paid Media Spend")
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "ad_spend_by_policy.png", dpi=160)
    plt.close()

    x = np.arange(2)
    width = 0.35
    plt.figure(figsize=(8, 5))
    plt.bar(x - width / 2, by_policy["platform_attributed_leads"], width, label="Platform-attributed leads", color="#ff7f0e")
    plt.bar(x + width / 2, by_policy["total_qualified_leads"], width, label="First-party total qualified leads", color="#1f77b4")
    plt.xticks(x, labels)
    plt.ylabel("Average leads per region-day")
    plt.title("Platform Attribution Falls More Than Ground-Truth Leads")
    plt.legend()
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "attributed_vs_total_leads.png", dpi=160)
    plt.close()

    plt.figure(figsize=(8, 5))
    plt.hist(null_effects, bins=40, color="#9e9e9e", edgecolor="white")
    plt.axvline(observed_perm_effect, color="#d62728", linewidth=2, label=f"Observed diff = {observed_perm_effect:.2f}")
    plt.xlabel("Permuted treatment-control difference in qualified leads")
    plt.ylabel("Randomized schedules")
    plt.title("Permutation Test for Total Qualified Leads")
    plt.legend()
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "permutation_test_total_leads.png", dpi=160)
    plt.close()

    plt.figure(figsize=(8, 5))
    colors = ["#2ca02c" if metric == "ad_spend" else "#1f77b4" for metric in effects["metric"]]
    plt.bar(effects["metric"], effects["switchback_effect"], color=colors)
    plt.axhline(0, color="black", linewidth=0.8)
    plt.ylabel("Reduced-bid effect per region-day")
    plt.title("Switchback Estimates: Spend Drops, Total Leads Barely Move")
    plt.xticks(rotation=20, ha="right")
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "switchback_effects.png", dpi=160)
    plt.close()


def main() -> None:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)

    df = simulate_switchback_data()
    df.to_csv(DATA_DIR / "airforce_recruiting_switchback_data.csv", index=False)

    _, _, effects = analyze_experiment(df)
    effects.to_csv(REPORT_DIR / "switchback_effect_estimates.csv", index=False)

    observed_perm_effect, null_effects, permutation_p = permutation_test(df)
    pd.DataFrame({"null_effect": null_effects}).to_csv(REPORT_DIR / "permutation_null_distribution.csv", index=False)
    make_plots(df, effects, null_effects, observed_perm_effect)

    baseline = df[df["reduced_bid_policy"] == 0]
    treatment = df[df["reduced_bid_policy"] == 1]
    spend_saved = baseline["ad_spend"].mean() - treatment["ad_spend"].mean()
    lead_change = treatment["total_qualified_leads"].mean() - baseline["total_qualified_leads"].mean()
    attributed_change = treatment["platform_attributed_leads"].mean() - baseline["platform_attributed_leads"].mean()
    cpl_baseline = baseline["ad_spend"].sum() / baseline["total_qualified_leads"].sum()
    cpl_treatment = treatment["ad_spend"].sum() / treatment["total_qualified_leads"].sum()

    summary = f"""# Air Force Recruiting Switchback Results\n\nScenario: randomized region-day switchback test of reduced paid-media bids for recruiting campaigns.\n\nAverage observed differences on reduced-bid region-days:\n\n- Ad spend saved per region-day: ${spend_saved:,.2f}\n- Change in first-party total qualified leads per region-day: {lead_change:.2f}\n- Change in platform-attributed leads per region-day: {attributed_change:.2f}\n- Baseline cost per qualified lead: ${cpl_baseline:,.2f}\n- Reduced-bid cost per qualified lead: ${cpl_treatment:,.2f}\n\nFixed-effects switchback estimates:\n\n{effects.to_string(index=False)}\n\nPermutation test for total qualified leads:\n\n- Observed treatment-control difference: {observed_perm_effect:.2f}\n- Randomization-inference p-value: {permutation_p:.3f}\n\nInterpretation: the platform-attributed lead count falls much more than first-party total qualified leads. In this simulated Air Force recruiting setting, reduced bids lower media cost while preserving most measured recruiting demand, suggesting the baseline bids were partly cannibalizing organic/direct interest.\n"""
    (REPORT_DIR / "model_results.md").write_text(summary)
    print(summary)


if __name__ == "__main__":
    main()
