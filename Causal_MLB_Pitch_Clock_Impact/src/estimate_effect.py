"""Estimate the impact of MLB's pitch clock on game duration."""

from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import statsmodels.formula.api as smf

PROJECT_DIR = Path(__file__).resolve().parents[1]
DATA_PATH = PROJECT_DIR / "data" / "processed" / "mlb_pitch_clock_games.csv"
TABLE_DIR = PROJECT_DIR / "reports" / "tables"
FIGURE_DIR = PROJECT_DIR / "reports" / "figures"


def naive_pre_post(df: pd.DataFrame, outcome: str) -> dict:
    pre = df[df["post_pitch_clock"] == 0][outcome]
    post = df[df["post_pitch_clock"] == 1][outcome]
    return {
        "outcome": outcome,
        "method": "naive_pre_post_difference",
        "pre_mean": pre.mean(),
        "post_mean": post.mean(),
        "estimate": post.mean() - pre.mean(),
    }


def adjusted_regression(df: pd.DataFrame, outcome: str) -> dict:
    formula = (
        f"{outcome} ~ post_pitch_clock + total_runs + game_outs + attendance "
        "+ C(day_night) + C(month) + C(park_id)"
    )
    model = smf.ols(formula, data=df).fit(cov_type="HC3")
    return {
        "outcome": outcome,
        "method": "adjusted_ols_with_month_and_park_fixed_effects",
        "estimate": model.params["post_pitch_clock"],
        "std_error": model.bse["post_pitch_clock"],
        "p_value": model.pvalues["post_pitch_clock"],
        "n_games": int(model.nobs),
        "r_squared": model.rsquared,
    }


def make_figures(df: pd.DataFrame) -> None:
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    sns.set_theme(style="whitegrid")

    monthly = (
        df.assign(month_start=df["date"].values.astype("datetime64[M]"))
        .groupby("month_start", as_index=False)
        .agg(
            avg_duration=("duration_minutes", "mean"),
            avg_stolen_bases=("total_stolen_bases", "mean"),
            games=("duration_minutes", "size"),
        )
    )

    plt.figure(figsize=(11, 5))
    sns.lineplot(data=monthly, x="month_start", y="avg_duration", marker="o")
    plt.axvline(pd.Timestamp("2023-01-01"), color="red", linestyle="--", label="Pitch clock era begins")
    plt.title("Average MLB Game Duration Before and After the Pitch Clock")
    plt.xlabel("Month")
    plt.ylabel("Average duration, minutes")
    plt.legend()
    plt.tight_layout()
    plt.savefig(FIGURE_DIR / "monthly_game_duration.png", dpi=300)
    plt.close()

    plt.figure(figsize=(8, 5))
    sns.boxplot(data=df, x="post_pitch_clock", y="duration_minutes")
    plt.xticks([0, 1], ["Pre pitch clock", "Post pitch clock"])
    plt.title("Distribution of Game Duration")
    plt.xlabel("")
    plt.ylabel("Duration, minutes")
    plt.tight_layout()
    plt.savefig(FIGURE_DIR / "duration_distribution_pre_post.png", dpi=300)
    plt.close()

    plt.figure(figsize=(11, 5))
    sns.lineplot(data=monthly, x="month_start", y="avg_stolen_bases", marker="o")
    plt.axvline(pd.Timestamp("2023-01-01"), color="red", linestyle="--", label="Pitch clock era begins")
    plt.title("Average Stolen Bases Per Game Before and After 2023 Rule Changes")
    plt.xlabel("Month")
    plt.ylabel("Average stolen bases per game")
    plt.legend()
    plt.tight_layout()
    plt.savefig(FIGURE_DIR / "monthly_stolen_bases.png", dpi=300)
    plt.close()


def main() -> None:
    TABLE_DIR.mkdir(parents=True, exist_ok=True)
    df = pd.read_csv(DATA_PATH, parse_dates=["date"])

    estimates = [
        naive_pre_post(df, "duration_minutes"),
        adjusted_regression(df, "duration_minutes"),
        naive_pre_post(df, "total_stolen_bases"),
        adjusted_regression(df, "total_stolen_bases"),
    ]

    results = pd.DataFrame(estimates)
    yearly = df.groupby("season", as_index=False).agg(
        games=("duration_minutes", "size"),
        avg_duration=("duration_minutes", "mean"),
        median_duration=("duration_minutes", "median"),
        avg_stolen_bases=("total_stolen_bases", "mean"),
        avg_total_runs=("total_runs", "mean"),
    )

    results.to_csv(TABLE_DIR / "causal_estimates.csv", index=False)
    yearly.to_csv(TABLE_DIR / "yearly_summary.csv", index=False)
    make_figures(df)

    print("Causal estimates")
    print(results)
    print("\nYearly summary")
    print(yearly)


if __name__ == "__main__":
    main()
