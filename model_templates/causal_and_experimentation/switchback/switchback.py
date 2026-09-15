"""Balanced within-unit switchback assignment and sharp-null randomization test."""
import numpy as np
import pandas as pd


def _validate_panel(panel):
    if panel.empty or panel[["unit", "period"]].isna().any().any():
        raise ValueError("Nonempty panel with nonmissing unit and period required")
    if panel.duplicated(["unit", "period"]).any():
        raise ValueError("One row per unit-period required")
    if (panel.groupby("unit").size() < 2).any():
        raise ValueError("At least two periods per unit required")


def balanced_assignment(panel, seed=42):
    """Uniformly choose floor(periods/2) treated periods within each unit."""
    _validate_panel(panel)
    rng = np.random.default_rng(seed)
    treatment = np.zeros(len(panel), dtype=int)
    for positions in panel.groupby("unit", sort=False).indices.values():
        selected = rng.choice(positions, size=len(positions)//2, replace=False)
        treatment[selected] = 1
    return treatment


def randomization_test(panel, permutations=1999, seed=42):
    """Requires EXACTLY the balanced_assignment design, no carryover/interference.

    Input columns: unit, period, treatment, outcome. Two-sided Fisher sharp-null
    test using a unit/period fixed-effects score, standardized by a constant.
    """
    _validate_panel(panel)
    if permutations < 1:
        raise ValueError("Positive permutation count required")
    t = panel["treatment"].to_numpy(dtype=float)
    y = panel["outcome"].to_numpy(dtype=float)
    if not np.isin(t, [0, 1]).all() or not np.isfinite(y).all():
        raise ValueError("Binary treatment and finite outcomes required")
    for positions in panel.groupby("unit", sort=False).indices.values():
        if t[positions].sum() != len(positions)//2:
            raise ValueError("Observed assignment does not match balanced within-unit design")
    fixed = pd.get_dummies(panel[["unit", "period"]].astype(str), drop_first=True, dtype=float)
    W = np.column_stack([np.ones(len(panel)), fixed.to_numpy()])
    y_resid = y - W @ np.linalg.lstsq(W, y, rcond=None)[0]
    t_resid = t - W @ np.linalg.lstsq(W, t, rcond=None)[0]
    if not np.isfinite(y_resid).all() or not np.isfinite(t_resid).all():
        raise ValueError("Fixed-effects calculation returned nonfinite residuals")
    denominator = t_resid @ t_resid
    if denominator <= 1e-10:
        raise ValueError("Treatment is confounded with fixed effects; effect not estimable")
    observed = t @ y_resid
    rng = np.random.default_rng(seed)
    null_scores = np.empty(permutations)
    for i in range(permutations):
        permuted = balanced_assignment(panel, seed=int(rng.integers(0, 2**32)))
        null_scores[i] = permuted @ y_resid
    p = (1 + np.sum(np.abs(null_scores) >= abs(observed) - 1e-10)) / (permutations + 1)
    return {"fixed_effects_estimate": float(observed/denominator),
            "sharp_null_p_value": float(p), "observed_score": float(observed),
            "null_scores": null_scores}
