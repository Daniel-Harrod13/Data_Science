from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

PROJECT_SRC = Path(__file__).resolve().parents[1] / "src"
if str(PROJECT_SRC) not in sys.path:
    sys.path.insert(0, str(PROJECT_SRC))

from port import run_edit_check_pipeline  # noqa: E402


def test_pipeline_matches_expected_parity_outputs() -> None:
    source_df = pd.DataFrame(
        {
            "record_id": [103, 101, 104, 102, 105],
            "age": [25, None, 17, 99, 101],
            "income": [50_000, 42_000, -10, None, 60_000],
            "state": ["CA", "", "WA", "  ", None],
        }
    )

    checked_df, summary_df, flagged_df = run_edit_check_pipeline(source_df)

    assert checked_df["record_id"].tolist() == [101, 102, 103, 104, 105]
    assert checked_df["chk_any_issue"].tolist() == [True, True, False, True, True]
    assert flagged_df["record_id"].tolist() == [101, 102, 104, 105]

    expected_summary = pd.DataFrame(
        [
            {"check_name": "chk_invalid_age", "flagged_rows": 2},
            {"check_name": "chk_invalid_income", "flagged_rows": 1},
            {"check_name": "chk_missing_age", "flagged_rows": 1},
            {"check_name": "chk_missing_income", "flagged_rows": 1},
            {"check_name": "chk_missing_state", "flagged_rows": 3},
        ]
    )
    pd.testing.assert_frame_equal(summary_df, expected_summary)


def test_boundary_age_and_income_rules() -> None:
    source_df = pd.DataFrame(
        {
            "record_id": [1, 2, 3, 4],
            "age": [18, 99, 17, 100],
            "income": [10, 1, 0, -5],
            "state": ["NY", "IL", "CA", "TX"],
        }
    )

    checked_df, _, _ = run_edit_check_pipeline(source_df)

    assert checked_df["chk_invalid_age"].tolist() == [False, False, True, True]
    assert checked_df["chk_invalid_income"].tolist() == [False, False, True, True]


def test_whitespace_and_null_states_are_flagged() -> None:
    source_df = pd.DataFrame(
        {
            "record_id": [2, 1, 3],
            "age": [30, 40, 50],
            "income": [50_000, 55_000, 60_000],
            "state": [None, "   ", "OR"],
        }
    )

    checked_df, summary_df, flagged_df = run_edit_check_pipeline(source_df)

    assert checked_df["record_id"].tolist() == [1, 2, 3]
    assert checked_df["chk_missing_state"].tolist() == [True, True, False]
    assert flagged_df["record_id"].tolist() == [1, 2]
    assert (
        summary_df.loc[summary_df["check_name"] == "chk_missing_state", "flagged_rows"].item()
        == 2
    )
