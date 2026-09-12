"""Monte Carlo operating characteristics for low-traffic healthcare A/B tests.

This project uses synthetic appointment-reminder outcomes only. It compares a
frequentist one-sided 5% z-test ship rule with a conjugate normal Bayesian rule
whose optimistic prior is estimated from genuine historical winners.

Run from the project root:
    ../.venv/bin/python src/simulate_ab_testing.py
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from scipy.stats import norm

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = PROJECT_ROOT / "data"
ARTIFACT_DIR = PROJECT_ROOT / "artifacts"
REPORT_DIR = PROJECT_ROOT / "reports"

SEED = 20250308
N_SIMULATIONS = 50_000
N_PER_ARM = 300
BASELINE_RATE = 0.20
HISTORICAL_EFFECT = 0.02
K_VALUES = (0, 1, 3, 5, 10)
HAIRCUTS = (0.00, 0.25, 0.50, 0.75, 1.00)
POSTERIOR_SHIP_THRESHOLD = 0.95
SCENARIOS = {
    "winner": 0.02,
    "dud": 0.00,
    "harmful": -0.01,
}


def planning_variance(effect: float) -> float:
    """Return the design-based variance of a difference in two proportions."""
    treatment_rate = BASELINE_RATE + effect
    return (
        treatment_rate * (1.0 - treatment_rate) / N_PER_ARM
        + BASELINE_RATE * (1.0 - BASELINE_RATE) / N_PER_ARM
    )


def simulate_estimates(
    rng: np.random.Generator, effect: float, shape: tuple[int, ...]
) -> tuple[np.ndarray, np.ndarray]:
    """Simulate arm-level binomial outcomes and Wald effect standard errors."""
    control_successes = rng.binomial(N_PER_ARM, BASELINE_RATE, size=shape)
    treatment_successes = rng.binomial(
        N_PER_ARM, BASELINE_RATE + effect, size=shape
    )
    control_rate = control_successes / N_PER_ARM
    treatment_rate = treatment_successes / N_PER_ARM
    estimate = treatment_rate - control_rate
    variance = (
        treatment_rate * (1.0 - treatment_rate) / N_PER_ARM
        + control_rate * (1.0 - control_rate) / N_PER_ARM
    )
    return estimate, np.sqrt(np.maximum(variance, np.finfo(float).eps))


def posterior_probability_positive(
    estimate: np.ndarray,
    standard_error: np.ndarray,
    prior_mean: np.ndarray | float,
    k: int,
) -> np.ndarray:
    """Compute P(effect > 0 | data) under a conjugate normal approximation.

    The new estimate has likelihood Normal(effect, observed_se^2). For k > 0,
    the empirical prior is Normal((1 - haircut) * historical_mean,
    historical_planning_variance / k). For k = 0, prior precision is zero,
    which is the flat-prior limit.
    """
    likelihood_precision = 1.0 / np.square(standard_error)
    prior_precision = k / planning_variance(HISTORICAL_EFFECT)
    posterior_variance = 1.0 / (likelihood_precision + prior_precision)
    posterior_mean = posterior_variance * (
        estimate * likelihood_precision + np.asarray(prior_mean) * prior_precision
    )
    return norm.cdf(posterior_mean / np.sqrt(posterior_variance))


def run_simulation() -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Run the fixed-seed Monte Carlo experiment and return report tables."""
    rng = np.random.default_rng(SEED)

    historical_estimates, _ = simulate_estimates(
        rng, HISTORICAL_EFFECT, (N_SIMULATIONS, max(K_VALUES))
    )
    cumulative_historical_means = np.cumsum(historical_estimates, axis=1) / np.arange(
        1, max(K_VALUES) + 1
    )

    rows: list[dict[str, object]] = []
    scenario_definitions: list[dict[str, object]] = []
    for scenario, true_effect in SCENARIOS.items():
        estimates, standard_errors = simulate_estimates(
            rng, true_effect, (N_SIMULATIONS,)
        )
        z_statistics = estimates / standard_errors
        frequentist_ship = z_statistics > norm.ppf(0.95)
        frequentist_rate = float(frequentist_ship.mean())
        rows.append(
            {
                "decision_method": "Frequentist one-sided 5% z-test",
                "k_historical": 0,
                "prior_mean_haircut_pct": np.nan,
                "scenario": scenario,
                "true_effect": true_effect,
                "ship_rate": frequentist_rate,
                "monte_carlo_se": np.sqrt(
                    frequentist_rate * (1.0 - frequentist_rate) / N_SIMULATIONS
                ),
                "n_simulations": N_SIMULATIONS,
                "decision_threshold": "one-sided p < 0.05 for a positive effect",
            }
        )

        for k in K_VALUES:
            raw_prior_mean: np.ndarray | float
            raw_prior_mean = (
                0.0 if k == 0 else cumulative_historical_means[:, k - 1]
            )
            for haircut in HAIRCUTS:
                prior_mean = (1.0 - haircut) * raw_prior_mean
                posterior_probability = posterior_probability_positive(
                    estimates, standard_errors, prior_mean, k
                )
                ship = posterior_probability >= POSTERIOR_SHIP_THRESHOLD
                ship_rate = float(ship.mean())
                rows.append(
                    {
                        "decision_method": "Bayesian empirical normal prior",
                        "k_historical": k,
                        "prior_mean_haircut_pct": int(haircut * 100),
                        "scenario": scenario,
                        "true_effect": true_effect,
                        "ship_rate": ship_rate,
                        "monte_carlo_se": np.sqrt(
                            ship_rate * (1.0 - ship_rate) / N_SIMULATIONS
                        ),
                        "n_simulations": N_SIMULATIONS,
                        "decision_threshold": (
                            f"posterior P(effect > 0) >= {POSTERIOR_SHIP_THRESHOLD:.2f}"
                        ),
                    }
                )

        scenario_definitions.append(
            {
                "scenario": scenario,
                "control_rate": BASELINE_RATE,
                "treatment_rate": BASELINE_RATE + true_effect,
                "true_effect": true_effect,
                "patients_per_arm": N_PER_ARM,
            }
        )

    operating = pd.DataFrame(rows)
    scenario_table = pd.DataFrame(scenario_definitions)

    prior_rows = []
    for k in K_VALUES:
        if k == 0:
            prior_rows.append(
                {
                    "k_historical": k,
                    "mean_raw_historical_estimate": np.nan,
                    "prior_standard_deviation": np.inf,
                    "interpretation": "flat-prior limit (zero prior precision)",
                }
            )
        else:
            prior_rows.append(
                {
                    "k_historical": k,
                    "mean_raw_historical_estimate": cumulative_historical_means[
                        :, k - 1
                    ].mean(),
                    "prior_standard_deviation": np.sqrt(
                        planning_variance(HISTORICAL_EFFECT) / k
                    ),
                    "interpretation": "normal prior before mean haircut",
                }
            )
    prior_summary = pd.DataFrame(prior_rows)
    return operating, scenario_table, prior_summary


def make_bayesian_matrix(operating: pd.DataFrame) -> pd.DataFrame:
    """Create one row per Bayesian configuration with all three risk/power rates."""
    bayesian = operating[
        operating["decision_method"] == "Bayesian empirical normal prior"
    ]
    matrix = bayesian.pivot(
        index=["k_historical", "prior_mean_haircut_pct"],
        columns="scenario",
        values="ship_rate",
    ).reset_index()
    return matrix.rename(
        columns={
            "winner": "power_winner_ship_rate",
            "dud": "dud_false_positive_ship_rate",
            "harmful": "harmful_ship_rate",
        }
    )


def plot_heatmaps(matrix: pd.DataFrame) -> None:
    """Plot Bayesian power, dud false-positive, and harmful ship rates."""
    sns.set_theme(style="whitegrid", context="notebook")
    metrics = [
        ("power_winner_ship_rate", "Power: true +2 pp winner"),
        ("dud_false_positive_ship_rate", "False positive: true 0 pp dud"),
        ("harmful_ship_rate", "Harmful ship: true -1 pp effect"),
    ]
    fig, axes = plt.subplots(1, 3, figsize=(17, 5.2), constrained_layout=True)
    for axis, (column, title) in zip(axes, metrics):
        grid = matrix.pivot(
            index="k_historical", columns="prior_mean_haircut_pct", values=column
        ).reindex(index=K_VALUES, columns=[int(value * 100) for value in HAIRCUTS])
        sns.heatmap(
            grid * 100,
            annot=True,
            fmt=".1f",
            cmap="mako" if column == "power_winner_ship_rate" else "rocket_r",
            vmin=0,
            vmax=100,
            cbar_kws={"label": "Ship rate (%)"},
            ax=axis,
        )
        axis.set_title(title)
        axis.set_xlabel("Prior-mean haircut (%)")
        axis.set_ylabel("Historical winners used (k)")
    fig.suptitle(
        "Bayesian long-run operating characteristics\n"
        f"{N_PER_ARM} patients/arm; ship when posterior P(effect > 0) ≥ "
        f"{POSTERIOR_SHIP_THRESHOLD:.2f}",
        fontsize=14,
    )
    fig.savefig(ARTIFACT_DIR / "bayesian_operating_characteristics_heatmaps.png", dpi=180)
    plt.close(fig)


def plot_tradeoffs(operating: pd.DataFrame, matrix: pd.DataFrame) -> None:
    """Plot power against dud and harmful shipping risk."""
    sns.set_theme(style="whitegrid", context="notebook")
    frequentist = operating[
        operating["decision_method"] == "Frequentist one-sided 5% z-test"
    ].set_index("scenario")["ship_rate"]

    fig, axes = plt.subplots(1, 2, figsize=(13, 5.5), constrained_layout=True)
    risk_columns = [
        ("dud_false_positive_ship_rate", "Dud false-positive ship rate"),
        ("harmful_ship_rate", "Harmful ship rate"),
    ]
    palette = sns.color_palette("viridis", n_colors=len(HAIRCUTS))
    for axis, (risk_column, x_label) in zip(axes, risk_columns):
        for color, haircut in zip(palette, [int(h * 100) for h in HAIRCUTS]):
            subset = matrix[matrix["prior_mean_haircut_pct"] == haircut].sort_values(
                "k_historical"
            )
            axis.plot(
                subset[risk_column] * 100,
                subset["power_winner_ship_rate"] * 100,
                marker="o",
                linewidth=1.8,
                color=color,
                label=f"{haircut}% haircut",
            )
            for row in subset.itertuples(index=False):
                if row.k_historical in (1, 10):
                    axis.annotate(
                        f"k={row.k_historical}",
                        (getattr(row, risk_column) * 100, row.power_winner_ship_rate * 100),
                        xytext=(4, 4),
                        textcoords="offset points",
                        fontsize=7,
                        color=color,
                    )
        frequentist_risk = frequentist["dud" if "dud" in risk_column else "harmful"]
        axis.scatter(
            frequentist_risk * 100,
            frequentist["winner"] * 100,
            marker="*",
            s=180,
            color="black",
            label="Frequentist benchmark",
            zorder=5,
        )
        axis.set_xlabel(f"{x_label} (%)")
        axis.set_ylabel("Winner power / ship rate (%)")
        axis.set_title(f"Power vs {x_label.lower()}")
    handles, labels = axes[1].get_legend_handles_labels()
    fig.legend(handles, labels, loc="outside lower center", ncol=3, frameon=False)
    fig.suptitle("Risk-power tradeoffs across optimistic-prior choices", fontsize=14)
    fig.savefig(ARTIFACT_DIR / "risk_power_tradeoff.png", dpi=180, bbox_inches="tight")
    plt.close(fig)


def write_model_results(operating: pd.DataFrame, matrix: pd.DataFrame) -> None:
    """Write a concise generated Markdown summary with precise interpretation."""
    frequentist = operating[
        operating["decision_method"] == "Frequentist one-sided 5% z-test"
    ].set_index("scenario")["ship_rate"]
    selected = matrix[
        matrix["k_historical"].isin([0, 1, 3, 5, 10])
        & matrix["prior_mean_haircut_pct"].isin([0, 50, 100])
    ].copy()
    selected = selected.rename(
        columns={
            "k_historical": "k",
            "prior_mean_haircut_pct": "Haircut",
            "power_winner_ship_rate": "Winner power",
            "dud_false_positive_ship_rate": "Dud ship rate",
            "harmful_ship_rate": "Harmful ship rate",
        }
    )
    for column in ["Winner power", "Dud ship rate", "Harmful ship rate"]:
        selected[column] = selected[column].map(lambda value: f"{100 * value:.1f}%")
    selected["Haircut"] = selected["Haircut"].map(lambda value: f"{int(value)}%")
    table_columns = [
        "k",
        "Haircut",
        "Winner power",
        "Dud ship rate",
        "Harmful ship rate",
    ]
    table_lines = [
        "| " + " | ".join(table_columns) + " |",
        "|" + "|".join(["---"] * len(table_columns)) + "|",
    ]
    table_lines.extend(
        "| " + " | ".join(str(row[column]) for column in table_columns) + " |"
        for _, row in selected.iterrows()
    )
    selected_table = "\n".join(table_lines)

    report = f"""# Model Results

Generated by `src/simulate_ab_testing.py` with seed `{SEED}` and `{N_SIMULATIONS:,}`
Monte Carlo repetitions per scenario.

## Frequentist benchmark

The frequentist rule ships when a one-sided z-test rejects zero in favor of a
positive effect at `p < 0.05`.

- True +2 percentage-point winner: **{100 * frequentist['winner']:.1f}%** ship rate (power)
- True 0-point dud: **{100 * frequentist['dud']:.1f}%** ship rate (false-positive rate)
- True -1-point harmful test: **{100 * frequentist['harmful']:.1f}%** ship rate

## Selected Bayesian operating characteristics

The Bayesian rule ships when `P(effect > 0 | current and historical data) >=
{POSTERIOR_SHIP_THRESHOLD:.2f}`. These percentages are repeated-sampling,
long-run operating characteristics of that decision rule. They are not posterior
probabilities that any particular shipped experiment is a false positive.

{selected_table}

The complete grid is in `bayesian_risk_power_matrix.csv`. Monte Carlo standard
errors for every rate are in `operating_characteristics.csv`; the largest is no
more than {100 * operating['monte_carlo_se'].max():.2f} percentage points.

## Interpretation

Optimistic historical information can increase the chance of shipping a real
winner, but the same prior can also increase shipping under null or harmful new
experiments. Mean haircuts reduce that optimism. A 100% haircut centers the prior
at zero while retaining its precision, so it is not the same as discarding the
historical experiments. The `k=0` rows are the flat-prior limit and are identical
across haircuts by construction.

This is a synthetic methodological simulation, not patient-level evidence and
not a claim of HIPAA compliance.
"""
    (REPORT_DIR / "model_results.md").write_text(report, encoding="utf-8")


def main() -> None:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_DIR.mkdir(parents=True, exist_ok=True)

    operating, scenarios, prior_summary = run_simulation()
    matrix = make_bayesian_matrix(operating)

    scenarios.to_csv(DATA_DIR / "simulation_scenarios.csv", index=False)
    operating.to_csv(REPORT_DIR / "operating_characteristics.csv", index=False)
    matrix.to_csv(REPORT_DIR / "bayesian_risk_power_matrix.csv", index=False)
    prior_summary.to_csv(REPORT_DIR / "historical_prior_summary.csv", index=False)
    plot_heatmaps(matrix)
    plot_tradeoffs(operating, matrix)
    write_model_results(operating, matrix)

    print(f"Completed {N_SIMULATIONS:,} simulations per scenario with seed {SEED}.")
    print(f"Reports: {REPORT_DIR}")
    print(f"Plots:   {ARTIFACT_DIR}")


if __name__ == "__main__":
    main()
