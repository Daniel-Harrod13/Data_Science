"""Two-model uplift estimator for a continuous outcome; no project-specific I/O."""
import numpy as np
from sklearn.base import clone
from sklearn.ensemble import RandomForestRegressor
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted


class TLearner:
    def __init__(self, estimator=None):
        self.estimator = estimator

    def fit(self, X, treatment, outcome):
        X, y = check_X_y(X, outcome, dtype=float)
        t = np.asarray(treatment)
        if t.shape != y.shape or not np.isin(t, [0, 1]).all():
            raise ValueError("Treatment must be a row-aligned binary vector")
        if min(np.sum(t == 0), np.sum(t == 1)) < 2:
            raise ValueError("At least two observations per arm required")
        base = self.estimator if self.estimator is not None else RandomForestRegressor(
            n_estimators=200, min_samples_leaf=20, random_state=42, n_jobs=-1)
        self.control_ = clone(base).fit(X[t == 0], y[t == 0])
        self.treated_ = clone(base).fit(X[t == 1], y[t == 1])
        return self

    def predict(self, X):
        """Predict conditional treatment effect, treated minus control."""
        check_is_fitted(self, ["control_", "treated_"])
        X = check_array(X, dtype=float)
        return self.treated_.predict(X) - self.control_.predict(X)
