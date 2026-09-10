"""Build an analysis-ready MLB game-level dataset from Retrosheet game logs."""

from pathlib import Path

import pandas as pd

PROJECT_DIR = Path(__file__).resolve().parents[1]
RAW_DIR = PROJECT_DIR / "data" / "raw"
OUTPUT_PATH = PROJECT_DIR / "data" / "processed" / "mlb_pitch_clock_games.csv"
YEARS = [2021, 2022, 2023, 2024]


def load_year(year: int) -> pd.DataFrame:
    path = RAW_DIR / f"gl{year}.txt"
    # Retrosheet game logs are comma-separated without a header.
    raw = pd.read_csv(path, header=None)

    df = pd.DataFrame(
        {
            "date": pd.to_datetime(raw[0].astype(str), format="%Y%m%d"),
            "season": year,
            "visiting_team": raw[3],
            "home_team": raw[6],
            "visiting_score": pd.to_numeric(raw[9], errors="coerce"),
            "home_score": pd.to_numeric(raw[10], errors="coerce"),
            "game_outs": pd.to_numeric(raw[11], errors="coerce"),
            "day_night": raw[12],
            "park_id": raw[16],
            "attendance": pd.to_numeric(raw[17], errors="coerce"),
            "duration_minutes": pd.to_numeric(raw[18], errors="coerce"),
            # Retrosheet batting stat columns: away SB = 34th field, home SB = 62nd field, zero-indexed 33 and 61.
            "visiting_stolen_bases": pd.to_numeric(raw[33], errors="coerce"),
            "home_stolen_bases": pd.to_numeric(raw[61], errors="coerce"),
        }
    )

    return df


def build_dataset() -> pd.DataFrame:
    df = pd.concat([load_year(year) for year in YEARS], ignore_index=True)
    df["total_runs"] = df["visiting_score"] + df["home_score"]
    df["innings"] = df["game_outs"] / 6
    df["total_stolen_bases"] = df["visiting_stolen_bases"] + df["home_stolen_bases"]
    df["post_pitch_clock"] = (df["season"] >= 2023).astype(int)
    df["month"] = df["date"].dt.month

    # Keep complete records for the main analysis.
    df = df.dropna(subset=["duration_minutes", "game_outs", "total_runs"])

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(OUTPUT_PATH, index=False)
    return df


if __name__ == "__main__":
    dataset = build_dataset()
    print(f"Wrote {OUTPUT_PATH}")
    print(dataset.head())
    print(dataset.groupby("season")["duration_minutes"].agg(["count", "mean", "median"]))
