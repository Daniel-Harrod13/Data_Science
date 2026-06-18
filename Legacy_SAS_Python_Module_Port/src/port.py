"""SAS-style edit checks and deterministic summary outputs."""

from __future__ import annotations

from typing import Tuple

import pandas as pd

REQUIRED_COLUMNS = ["record_id", "age", "income", "state"]
CHECK_COLUMNS = [
    "chk_missing_age",
    "chk_invalid_age",
    "chk_missing_income",
    "chk_invalid_income",
    "chk_missing_state",
]


def _validate_input_columns(df: pd.DataFrame) -> None:
    missing_columns = [column for column in REQUIRED_COLUMNS if column not in df.columns]
    if missing_columns:
        raise ValueError(f"Input dataframe is missing required columns: {missing_columns}")


def apply_edit_checks(df: pd.DataFrame, id_column: str = "record_id") -> pd.DataFrame:
    """Apply row-level edit checks similar to a SAS data cleaning routine."""
    _validate_input_columns(df)
    if id_column not in df.columns:
        raise ValueError(f"Configured id column '{id_column}' is not present in the dataframe.")

    checked = df.copy()
    normalized_state = checked["state"].fillna("").astype(str).str.strip()

    checked["chk_missing_age"] = checked["age"].isna()
    checked["chk_invalid_age"] = checked["age"].notna() & (
        (checked["age"] < 18) | (checked["age"] > 99)
    )
    checked["chk_missing_income"] = checked["income"].isna()
    checked["chk_invalid_income"] = checked["income"].notna() & (checked["income"] <= 0)
    checked["chk_missing_state"] = normalized_state.eq("")
    checked["chk_any_issue"] = checked[CHECK_COLUMNS].any(axis=1)

    # SAS pipelines often assume deterministic ordering for downstream comparisons.
    checked = checked.sort_values(by=id_column, kind="mergesort").reset_index(drop=True)
    return checked


def summarize_checks(checked_df: pd.DataFrame) -> pd.DataFrame:
    """Summarize the number of rows flagged by each edit check."""
    missing_checks = [column for column in CHECK_COLUMNS if column not in checked_df.columns]
    if missing_checks:
        raise ValueError(
            f"Checked dataframe is missing required check columns: {missing_checks}"
        )

    summary_rows = [
        {"check_name": check_name, "flagged_rows": int(checked_df[check_name].sum())}
        for check_name in CHECK_COLUMNS
    ]
    summary = pd.DataFrame(summary_rows)
    summary = summary.sort_values(by="check_name", kind="mergesort").reset_index(drop=True)
    return summary


def run_edit_check_pipeline(
    df: pd.DataFrame, id_column: str = "record_id"
) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Run all edit checks and return full, summary, and issue-only outputs."""
    checked = apply_edit_checks(df, id_column=id_column)
    summary = summarize_checks(checked)
    flagged_rows = checked.loc[checked["chk_any_issue"]].reset_index(drop=True)
    return checked, summary, flagged_rows
