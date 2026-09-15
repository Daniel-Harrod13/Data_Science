"""Cross-fitted partially linear DML for one continuous treatment."""
import numpy as np
from sklearn.base import clone
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import KFold
from sklearn.utils.validation import check_X_y


def estimate_dml(X, treatment, outcome, outcome_model=None, treatment_model=None, folds=5, seed=42):
    """IID estimator under Y = theta*D + g(X) + error; not a nonlinear dose curve."""
    X, y = check_X_y(X, outcome, dtype=float)
    d = np.asarray(treatment, dtype=float)
    if d.shape != y.shape or not np.isfinite(d).all():
        raise ValueError("Treatment must be finite and row-aligned")
    if not 2 <= folds <= len(y) // 2:
        raise ValueError("Require 2 <= folds <= n/2")
    default = RandomForestRegressor(n_estimators=200, min_samples_leaf=10, random_state=seed, n_jobs=-1)
    y_base = outcome_model if outcome_model is not None else default
    d_base = treatment_model if treatment_model is not None else default
    y_hat, d_hat = np.empty(len(y)), np.empty(len(y))
    for train, test in KFold(folds, shuffle=True, random_state=seed).split(X):
        y_hat[test] = clone(y_base).fit(X[train], y[train]).predict(X[test])
        d_hat[test] = clone(d_base).fit(X[train], d[train]).predict(X[test])
    if not np.isfinite(y_hat).all() or not np.isfinite(d_hat).all():
        raise ValueError("Nuisance models returned nonfinite predictions")
    u, v = y - y_hat, d - d_hat
    residual_variance = np.mean(v*v)
    if residual_variance <= 1e-12:
        raise ValueError("Insufficient residual treatment variation")
    theta = np.mean(v*u) / residual_variance
    influence = v * (u - theta*v) / residual_variance
    return {"effect": float(theta),
            "iid_standard_error": float(np.std(influence, ddof=1)/np.sqrt(len(y))),
            "residual_treatment_variance": float(residual_variance),
            "outcome_residuals": u, "treatment_residuals": v}
