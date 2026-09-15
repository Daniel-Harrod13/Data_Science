"""Original aquarium promotion study using canonical Gaussian information increments."""
from pathlib import Path
import argparse
import json

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import norm

ROOT = Path(__file__).resolve().parents[1]
ALPHA = 0.05
DAYS = 30
CUSTOMERS_PER_ARM_PER_DAY = 40
SPENDING_SD = 25.0
CONTROL_MEAN = 45.0
LOOK_DAYS = np.array([6, 12, 18, 24, 30])


def simulate_z(rng, trials, information, final_effect_z=0.0):
    """Correlated Z statistics; independent increments, known variance, one endpoint."""
    information = np.asarray(information, dtype=float)
    increments = np.diff(np.r_[0.0, information])
    if trials < 1 or np.any(increments <= 0) or not np.isclose(information[-1], 1):
        raise ValueError("Positive trials and increasing information ending at 1 required")
    brownian = np.cumsum(rng.normal(size=(trials, len(information))) * np.sqrt(increments), axis=1)
    return brownian / np.sqrt(information) + final_effect_z * np.sqrt(information)


def calibrate(z_null, information, alpha=ALPHA):
    """Monte Carlo critical constants for two-sided classical boundary shapes."""
    t = np.asarray(information)
    obf_constant = np.quantile(np.max(np.abs(z_null) * np.sqrt(t), axis=1), 1 - alpha)
    pocock_constant = np.quantile(np.max(np.abs(z_null), axis=1), 1 - alpha)
    return {
        "O'Brien–Fleming": obf_constant / np.sqrt(t),
        "Pocock": np.repeat(pocock_constant, len(t)),
    }


def first_crossing(z, boundaries, days):
    crossed = np.abs(z) >= np.asarray(boundaries)
    rejected = crossed.any(axis=1)
    index = np.where(rejected, crossed.argmax(axis=1), z.shape[1] - 1)
    direction = np.where(rejected, np.sign(z[np.arange(len(z)), index]), 0)
    return rejected, np.asarray(days)[index], direction


def wilson(successes, total):
    p = successes / total
    q = norm.ppf(0.975)
    denominator = 1 + q*q/total
    center = (p + q*q/(2*total)) / denominator
    half = q*np.sqrt(p*(1-p)/total + q*q/(4*total*total)) / denominator
    return center - half, center + half


def run(trials=100_000, calibration_trials=500_000, seed=20260915):
    if trials < 100 or calibration_trials < 100:
        raise ValueError("Use at least 100 calibration and evaluation trials")
    for folder in ("reports", "artifacts", "data"):
        (ROOT / folder).mkdir(exist_ok=True)
    streams = np.random.SeedSequence(seed).spawn(6)
    information = LOOK_DAYS / DAYS
    boundaries = calibrate(simulate_z(np.random.default_rng(streams[0]), calibration_trials, information), information)
    boundary_table = pd.DataFrame({"day": LOOK_DAYS, "information_fraction": information, **boundaries})
    boundary_table.to_csv(ROOT / "reports/boundaries.csv", index=False)
    fixed = norm.ppf(1 - ALPHA / 2)
    final_se = SPENDING_SD * np.sqrt(2 / (DAYS * CUSTOMERS_PER_ARM_PER_DAY))
    scenarios = {"No effect": 0.0, "Modest improvement": 1.0, "Large improvement": 4.0, "Harm": -3.0}
    rows = []
    for stream, (scenario, effect) in zip(streams[1:5], scenarios.items()):
        # Daily aggregate mean differences follow the normal approximation, not raw receipts.
        z = simulate_z(np.random.default_rng(stream), trials, np.arange(1, DAYS + 1) / DAYS, effect / final_se)
        planned = z[:, LOOK_DAYS - 1]
        methods = {
            "Fixed horizon": (z[:, -1:], [fixed], [DAYS]),
            "Naive daily peeking": (z, np.repeat(fixed, DAYS), np.arange(1, DAYS + 1)),
            "Naive five looks": (planned, np.repeat(fixed, len(LOOK_DAYS)), LOOK_DAYS),
            **{name: (planned, boundary, LOOK_DAYS) for name, boundary in boundaries.items()},
        }
        for method, (path, boundary, days) in methods.items():
            rejected, stop, direction = first_crossing(path, boundary, days)
            low, high = wilson(rejected.sum(), trials)
            rows.append({
                "scenario": scenario, "effect_dollars": effect, "method": method,
                "trials": trials, "rejection_rate": rejected.mean(),
                "rejection_ci_low": low, "rejection_ci_high": high,
                "correct_direction_rate": np.mean(direction == np.sign(effect)) if effect else np.nan,
                "wrong_direction_rate": np.mean(direction == -np.sign(effect)) if effect else np.nan,
                "positive_stop_rate": np.mean(direction > 0), "negative_stop_rate": np.mean(direction < 0),
                "early_stop_rate": np.mean(stop < DAYS), "mean_stop_day": stop.mean(),
                "mean_total_customers": (2 * CUSTOMERS_PER_ARM_PER_DAY * stop).mean(),
                "mean_treatment_customers": (CUSTOMERS_PER_ARM_PER_DAY * stop).mean(),
            })
    results = pd.DataFrame(rows)
    results.to_csv(ROOT / "reports/operating_characteristics.csv", index=False)
    metadata = {"seed": seed, "evaluation_trials_per_scenario": trials, "calibration_trials": calibration_trials,
                "alpha_two_sided": ALPHA, "control_mean_dollars": CONTROL_MEAN,
                "known_spending_sd_dollars": SPENDING_SD, "customers_per_arm_per_day": CUSTOMERS_PER_ARM_PER_DAY,
                "planned_look_days": LOOK_DAYS.tolist(), "final_standard_error_dollars": final_se,
                "numpy_version": np.__version__, "pandas_version": pd.__version__}
    (ROOT / "reports/run_metadata.json").write_text(json.dumps(metadata, indent=2) + "\n")
    # Illustrative aggregate data, independent of boundary calibration and evaluation.
    rng = np.random.default_rng(streams[5])
    control = rng.normal(CONTROL_MEAN, SPENDING_SD / np.sqrt(CUSTOMERS_PER_ARM_PER_DAY), DAYS)
    treatment = rng.normal(CONTROL_MEAN + 1, SPENDING_SD / np.sqrt(CUSTOMERS_PER_ARM_PER_DAY), DAYS)
    pd.DataFrame({"day": np.arange(1, DAYS+1), "customers_per_arm": CUSTOMERS_PER_ARM_PER_DAY,
                  "control_mean_spend": control, "treatment_mean_spend": treatment}).to_csv(ROOT / "data/example_daily_aggregates.csv", index=False)
    make_plots(boundaries, results, fixed)
    write_report(results, boundary_table, metadata)
    print(results.to_string(index=False))
    return results


def make_plots(boundaries, results, fixed):
    plt.style.use("seaborn-v0_8-whitegrid")
    fig, ax = plt.subplots(figsize=(8, 5))
    for name, values in boundaries.items():
        ax.plot(LOOK_DAYS, values, "o-", label=name)
    ax.axhline(fixed, color="gray", linestyle="--", label="Unadjusted 1.96")
    ax.set(xlabel="Planned analysis day", ylabel="Two-sided absolute Z threshold", title="Aquarium promotion: five planned looks")
    ax.legend()
    fig.tight_layout()
    fig.savefig(ROOT / "artifacts/sequential_boundaries.png", dpi=180)
    plt.close(fig)
    null = results[results.scenario == "No effect"]
    fig, ax = plt.subplots(figsize=(9, 5))
    p = null.rejection_rate.to_numpy()
    ax.barh(null.method, 100*p, xerr=100*np.array([p-null.rejection_ci_low, null.rejection_ci_high-p]), color="#277d91")
    ax.axvline(5, color="#b64040", linestyle="--", label="5% target")
    ax.set(xlabel="False-positive rate (%) with 95% Monte Carlo intervals", title="Peeking risk under no promotion effect")
    ax.legend()
    fig.tight_layout()
    fig.savefig(ROOT / "artifacts/false_positive_rates.png", dpi=180)
    plt.close(fig)
    nonnull = results[results.scenario != "No effect"]
    fig, axes = plt.subplots(1, 2, figsize=(13, 5))
    for method, group in nonnull.groupby("method", sort=False):
        axes[0].plot(group.scenario, group.correct_direction_rate*100, "o-", label=method)
        axes[1].plot(group.scenario, group.mean_stop_day, "o-", label=method)
    axes[0].set(ylabel="Correct-direction detection (%)", title="Detection by effect scenario", ylim=(0, 105))
    axes[1].set(ylabel="Mean stopping day (all trials)", title="Speed, including non-rejections", ylim=(0, 32))
    for ax in axes:
        ax.tick_params(axis="x", rotation=15)
    axes[1].legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(ROOT / "artifacts/power_and_stopping.png", dpi=180)
    plt.close(fig)


def write_report(results, boundaries, metadata):
    lines = ["# Aquarium Promotion: Simulation Results", "", "All results are synthetic, not store observations.", "",
             f"Seed: {metadata['seed']}; independent calibration paths: {metadata['calibration_trials']:,}; evaluation paths per scenario: {metadata['evaluation_trials_per_scenario']:,}.", "",
             "## Calibrated two-sided boundaries", "", "| Day | O’Brien–Fleming | Pocock |", "|---|---:|---:|"]
    for _, row in boundaries.iterrows():
        obf = row["O'Brien–Fleming"]
        lines.append(f"| {row['day']:.0f} | {obf:.4f} | {row['Pocock']:.4f} |")
    lines += ["", "## Operating characteristics", "", "Rejection under the null is a false positive. Detection below means a rejection in the correct direction; wrong-direction rates are in the CSV. Customers and stopping days average over every trial, including non-rejections.", "", "| Scenario | Method | Rejection % (95% MC interval) | Correct detection % | Mean day | Treatment customers |", "|---|---|---:|---:|---:|---:|"]
    for _, row in results.iterrows():
        detection = "—" if pd.isna(row.correct_direction_rate) else f"{100*row.correct_direction_rate:.2f}"
        lines.append(f"| {row.scenario} | {row.method} | {100*row.rejection_rate:.2f} ({100*row.rejection_ci_low:.2f}–{100*row.rejection_ci_high:.2f}) | {detection} | {row.mean_stop_day:.2f} | {row.mean_treatment_customers:.0f} |")
    lines += ["", "## Interpretation", "", "- Compare the sequential rules to the fixed-horizon benchmark at approximately equal false-positive risk. Naive peeking's apparent detection advantage comes with inflated risk.", "- O’Brien–Fleming protects the final analysis with a high early threshold; Pocock offers easier early detection but a higher final threshold.", "- A negative crossing supports harm, not merely lack of benefit. No crossing is inconclusive; this design has no futility boundary.", "- Confidence intervals quantify evaluation Monte Carlo error conditional on the calibrated boundaries; they exclude calibration uncertainty.", "- These are efficacy/harm boundaries for spending, not a comprehensive safety system or proof of profitability.", ""]
    (ROOT / "reports/results.md").write_text("\n".join(lines))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--trials", type=int, default=100_000)
    parser.add_argument("--calibration-trials", type=int, default=500_000)
    parser.add_argument("--seed", type=int, default=20260915)
    args = parser.parse_args()
    run(args.trials, args.calibration_trials, args.seed)
