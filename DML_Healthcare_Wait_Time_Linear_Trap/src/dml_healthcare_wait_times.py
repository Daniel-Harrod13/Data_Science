"""Double Machine Learning vs Fixed Effects for healthcare wait-time operations.

Portfolio question:
    What is the long-term adherence impact of reducing long appointment wait times?

The trap:
    Fixed effects can help remove clinic/specialty confounding, but a linear model still
    spreads the treatment effect evenly across all wait times. In this simulation, the
    true impact is nonlinear: reducing a 7-day wait barely matters, while reducing a
    45-day wait has a large effect.

Run from this project root:
    python src/dml_healthcare_wait_times.py
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score
from sklearn.model_selection import KFold
from sklearn.preprocessing import OneHotEncoder

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = PROJECT_ROOT / "data"
ARTIFACT_DIR = PROJECT_ROOT / "artifacts"
REPORT_DIR = PROJECT_ROOT / "reports"
RANDOM_STATE = 42


def wait_time_effect(wait_days: np.ndarray) -> np.ndarray:
    """True nonlinear causal effect of wait time on 180-day adherence score.

    Wait times <= 14 days have almost no penalty. The penalty becomes moderate from
    14 to 30 days and steep after 30 days, where patients start missing follow-ups
    and delaying medication starts.
    """
    wait_days = np.asarray(wait_days)
    moderate = -0.55 * np.clip(wait_days - 14, 0, 16)
    severe = -1.95 * np.clip(wait_days - 30, 0, None)
    return moderate + severe


def true_marginal_effect(wait_days: np.ndarray) -> np.ndarray:
    """Derivative of the true dose-response curve."""
    wait_days = np.asarray(wait_days)
    return np.select(
        [wait_days <= 14, wait_days <= 30, wait_days > 30],
        [0.0, -0.55, -2.50],
    )


def simulate_healthcare_data(n: int = 25_000, seed: int = RANDOM_STATE) -> pd.DataFrame:
    """Simulate observational healthcare operations data with confounding and a nonlinear effect."""
    rng = np.random.default_rng(seed)

    n_clinics = 45
    n_specialties = 8
    clinic_id = rng.integers(0, n_clinics, n)
    specialty_id = rng.integers(0, n_specialties, n)

    clinic_quality = rng.normal(0, 1, n_clinics)
    clinic_capacity = rng.normal(0, 1, n_clinics)
    specialty_complexity = rng.normal(0, 0.8, n_specialties)

    age = np.clip(rng.normal(52, 16, n), 18, 90)
    comorbidity_index = rng.poisson(1.8, n)
    distance_miles = np.clip(rng.gamma(2.0, 6.5, n), 0, 80)
    prior_no_show_rate = rng.beta(1.5, 7.0, n)
    medicaid = rng.binomial(1, 0.28, n)
    urgent_referral = rng.binomial(1, 0.22, n)

    wait_days = (
        25
        + 6.0 * specialty_complexity[specialty_id]
        - 5.0 * clinic_capacity[clinic_id]
        - 3.0 * clinic_quality[clinic_id]
        + 2.5 * comorbidity_index
        + 4.5 * medicaid
        - 8.0 * urgent_referral
        + 0.10 * distance_miles
        + rng.normal(0, 7, n)
    )
    wait_days = np.clip(wait_days, 1, 75)

    baseline_adherence = (
        78
        + 4.5 * clinic_quality[clinic_id]
        - 3.2 * prior_no_show_rate * 10
        - 0.18 * distance_miles
        - 1.2 * comorbidity_index
        - 3.5 * medicaid
        + 2.0 * urgent_referral
        + rng.normal(0, 8, n)
    )

    adherence_180d = baseline_adherence + wait_time_effect(wait_days)
    adherence_180d = np.clip(adherence_180d, 0, 100)

    wait_days_after_intervention = np.maximum(wait_days * 0.80, 1)
    true_intervention_gain = wait_time_effect(wait_days_after_intervention) - wait_time_effect(wait_days)

    return pd.DataFrame(
        {
            "clinic_id": clinic_id.astype(str),
            "specialty_id": specialty_id.astype(str),
            "age": age,
            "comorbidity_index": comorbidity_index,
            "distance_miles": distance_miles,
            "prior_no_show_rate": prior_no_show_rate,
            "medicaid": medicaid,
            "urgent_referral": urgent_referral,
            "wait_days": wait_days,
            "wait_days_after_intervention": wait_days_after_intervention,
            "adherence_180d": adherence_180d,
            "true_intervention_gain": true_intervention_gain,
            "true_marginal_effect": true_marginal_effect(wait_days),
        }
    )


def fit_ols_models(data: pd.DataFrame) -> tuple[float, float]:
    """Estimate naive and fixed-effects linear slopes."""
    naive = smf.ols("adherence_180d ~ wait_days", data=data).fit()
    fixed_effects = smf.ols(
        "adherence_180d ~ wait_days + age + comorbidity_index + distance_miles + "
        "prior_no_show_rate + medicaid + urgent_referral + C(clinic_id) + C(specialty_id)",
        data=data,
    ).fit(cov_type="cluster", cov_kwds={"groups": data["clinic_id"]})
    return naive.params["wait_days"], fixed_effects.params["wait_days"]


def make_design_matrix(data: pd.DataFrame) -> np.ndarray:
    """Features used by nuisance models, including clinic and specialty indicators."""
    numeric = data[
        [
            "age",
            "comorbidity_index",
            "distance_miles",
            "prior_no_show_rate",
            "medicaid",
            "urgent_referral",
        ]
    ].to_numpy()
    encoder = OneHotEncoder(sparse_output=False, handle_unknown="ignore")
    categorical = encoder.fit_transform(data[["clinic_id", "specialty_id"]])
    return np.hstack([numeric, categorical])


def cross_fit_residuals(data: pd.DataFrame) -> pd.DataFrame:
    """Cross-fit nuisance models for E[Y|X] and E[D|X]."""
    X = make_design_matrix(data)
    y = data["adherence_180d"].to_numpy()
    d = data["wait_days"].to_numpy()

    y_hat = np.zeros(len(data))
    d_hat = np.zeros(len(data))
    splitter = KFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)

    for train_idx, test_idx in splitter.split(X):
        y_model = RandomForestRegressor(
            n_estimators=250,
            min_samples_leaf=30,
            max_features="sqrt",
            random_state=RANDOM_STATE,
            n_jobs=-1,
        )
        d_model = RandomForestRegressor(
            n_estimators=250,
            min_samples_leaf=30,
            max_features="sqrt",
            random_state=RANDOM_STATE + 1,
            n_jobs=-1,
        )
        y_model.fit(X[train_idx], y[train_idx])
        d_model.fit(X[train_idx], d[train_idx])
        y_hat[test_idx] = y_model.predict(X[test_idx])
        d_hat[test_idx] = d_model.predict(X[test_idx])

    residualized = data.copy()
    residualized["y_resid"] = y - y_hat
    residualized["d_resid"] = d - d_hat
    residualized["y_hat"] = y_hat
    residualized["d_hat"] = d_hat
    return residualized


def local_dml_slopes(data: pd.DataFrame) -> pd.DataFrame:
    """Estimate local DML slopes by wait-time band.

    Within each wait-time band, estimate theta = sum(D_resid * Y_resid) / sum(D_resid^2).
    This keeps the orthogonalization idea of DML while allowing the wait-time effect
    to vary across the operational range.
    """
    bins = [0, 14, 30, 45, 75]
    labels = ["<=14 days", "15-30 days", "31-45 days", "46+ days"]
    framed = data.copy()
    framed["wait_band"] = pd.cut(framed["wait_days"], bins=bins, labels=labels, include_lowest=True)

    rows = []
    for band, subset in framed.groupby("wait_band", observed=True):
        numerator = np.sum(subset["d_resid"] * subset["y_resid"])
        denominator = np.sum(subset["d_resid"] ** 2)
        theta = numerator / denominator
        rows.append(
            {
                "wait_band": str(band),
                "n": len(subset),
                "avg_wait_days": subset["wait_days"].mean(),
                "dml_local_slope": theta,
                "true_avg_marginal_effect": subset["true_marginal_effect"].mean(),
            }
        )
    return pd.DataFrame(rows)


def assign_dml_gain(data: pd.DataFrame, slopes: pd.DataFrame) -> pd.DataFrame:
    bins = [0, 14, 30, 45, 75]
    labels = ["<=14 days", "15-30 days", "31-45 days", "46+ days"]
    slope_map = slopes.set_index("wait_band")["dml_local_slope"].to_dict()

    scored = data.copy()
    scored["wait_band"] = pd.cut(scored["wait_days"], bins=bins, labels=labels, include_lowest=True).astype(str)
    scored["dml_local_slope"] = scored["wait_band"].map(slope_map)
    scored["wait_day_change"] = scored["wait_days_after_intervention"] - scored["wait_days"]
    scored["dml_predicted_gain"] = scored["dml_local_slope"] * scored["wait_day_change"]
    return scored


def make_plots(data: pd.DataFrame, slopes: pd.DataFrame, opportunity: pd.DataFrame) -> None:
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

    grid = np.linspace(1, 75, 300)
    plt.figure(figsize=(8, 5))
    plt.plot(grid, wait_time_effect(grid), color="black", linewidth=2.0, label="True dose-response")
    plt.axvline(14, color="#777777", linestyle="--", linewidth=1)
    plt.axvline(30, color="#777777", linestyle="--", linewidth=1)
    plt.xlabel("Appointment wait time, days")
    plt.ylabel("Causal effect on 180-day adherence score")
    plt.title("Nonlinear Wait-Time Penalty: Plateau, Slope, Cliff")
    plt.legend()
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "true_wait_time_effect_curve.png", dpi=160)
    plt.close()

    x = np.arange(len(slopes))
    width = 0.36
    plt.figure(figsize=(8, 5))
    plt.bar(x - width / 2, slopes["true_avg_marginal_effect"], width, label="True avg marginal effect", color="black")
    plt.bar(x + width / 2, slopes["dml_local_slope"], width, label="Local DML slope", color="#1f77b4")
    plt.axhline(0, color="black", linewidth=0.8)
    plt.xticks(x, slopes["wait_band"], rotation=20)
    plt.ylabel("Adherence score change per +1 wait day")
    plt.title("DML Finds That Wait-Time Harm Is Concentrated at Long Waits")
    plt.legend()
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "dml_local_slopes_by_wait_band.png", dpi=160)
    plt.close()

    plt.figure(figsize=(8, 5))
    plt.bar(opportunity["method"], opportunity["avg_gain_per_patient"], color=["#d62728", "#ff7f0e", "#1f77b4", "#2ca02c"])
    plt.ylabel("Avg predicted adherence gain per patient")
    plt.title("Opportunity Sizing: 20% Reduction for Waits Above 30 Days")
    plt.xticks(rotation=20, ha="right")
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "opportunity_sizing_comparison.png", dpi=160)
    plt.close()

    sample = data.sample(5000, random_state=RANDOM_STATE)
    colors = np.where(sample["wait_days"] > 30, "#d62728", np.where(sample["wait_days"] > 14, "#ff7f0e", "#9e9e9e"))
    plt.figure(figsize=(8, 5))
    plt.scatter(sample["wait_days"], sample["dml_predicted_gain"], c=colors, alpha=0.35, s=12)
    plt.xlabel("Current wait time, days")
    plt.ylabel("DML-predicted gain from 20% reduction")
    plt.title("Predicted Benefit Is Small at Short Waits and Large Past the Cliff")
    plt.tight_layout()
    plt.savefig(ARTIFACT_DIR / "dml_predicted_gain_by_wait_time.png", dpi=160)
    plt.close()


def main() -> None:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)

    data = simulate_healthcare_data()
    data.to_csv(DATA_DIR / "simulated_healthcare_wait_times.csv", index=False)

    naive_slope, fe_slope = fit_ols_models(data)
    residualized = cross_fit_residuals(data)
    slopes = local_dml_slopes(residualized)
    scored = assign_dml_gain(residualized, slopes)
    scored.to_csv(DATA_DIR / "dml_scored_wait_time_intervention.csv", index=False)
    slopes.to_csv(REPORT_DIR / "dml_local_slopes.csv", index=False)

    # Operational program: prioritize access improvements for patients facing the 30+ day cliff.
    target = scored[scored["wait_days"] > 30].copy()
    wait_day_change = target["wait_days_after_intervention"] - target["wait_days"]
    opportunity = pd.DataFrame(
        [
            {
                "method": "Naive OLS",
                "avg_gain_per_patient": (naive_slope * wait_day_change).mean(),
                "total_gain": (naive_slope * wait_day_change).sum(),
            },
            {
                "method": "Fixed Effects OLS",
                "avg_gain_per_patient": (fe_slope * wait_day_change).mean(),
                "total_gain": (fe_slope * wait_day_change).sum(),
            },
            {
                "method": "Local DML",
                "avg_gain_per_patient": target["dml_predicted_gain"].mean(),
                "total_gain": target["dml_predicted_gain"].sum(),
            },
            {
                "method": "Oracle True DGP",
                "avg_gain_per_patient": target["true_intervention_gain"].mean(),
                "total_gain": target["true_intervention_gain"].sum(),
            },
        ]
    )
    opportunity.to_csv(REPORT_DIR / "opportunity_sizing.csv", index=False)
    make_plots(scored, slopes, opportunity)

    nuisance_r2_y = r2_score(scored["adherence_180d"], scored["y_hat"])
    nuisance_r2_d = r2_score(scored["wait_days"], scored["d_hat"])
    dml_error = abs(
        opportunity.loc[opportunity["method"] == "Local DML", "avg_gain_per_patient"].iloc[0]
        - opportunity.loc[opportunity["method"] == "Oracle True DGP", "avg_gain_per_patient"].iloc[0]
    )

    opportunity_table = "| Method | Avg gain per patient | Total gain |\n|---|---:|---:|\n"
    for row in opportunity.itertuples(index=False):
        opportunity_table += f"| {row.method} | {row.avg_gain_per_patient:.3f} | {row.total_gain:,.1f} |\n"

    summary = f"""# DML Healthcare Wait-Time Results\n\nScenario: estimate the long-term adherence impact of reducing appointment wait times above 30 days by 20%.\n\nLinear slope estimates for the effect of one additional wait day on 180-day adherence:\n\n- Naive OLS slope: {naive_slope:.3f}\n- Fixed Effects OLS slope: {fe_slope:.3f}\n\nCross-fitted nuisance model quality:\n\n- Outcome nuisance R-squared: {nuisance_r2_y:.3f}\n- Treatment nuisance R-squared: {nuisance_r2_d:.3f}\n\nOpportunity sizing: average adherence-score gain per targeted patient from a 20% wait-time reduction among patients currently waiting more than 30 days:\n\n{opportunity_table}\nLocal DML absolute error vs oracle average gain: {dml_error:.3f} adherence points per patient.\n\nInterpretation: fixed effects reduce confounding, but the linear specification spreads benefit across all wait times. Local DML better captures that operational value is concentrated among patients facing long waits, especially beyond the 30-day cliff.\n"""
    (REPORT_DIR / "model_results.md").write_text(summary)
    print(summary)


if __name__ == "__main__":
    main()
