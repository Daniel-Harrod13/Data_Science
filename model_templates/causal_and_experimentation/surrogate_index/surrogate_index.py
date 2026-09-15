"""Train a historical outcome bridge, then compare scores in a new experiment."""
import numpy as np
from sklearn.base import clone
from sklearn.ensemble import RandomForestRegressor
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted


class SurrogateIndex:
    def __init__(self, estimator=None):
        self.estimator = estimator

    def fit(self, historical_features, long_term_outcome):
        X, y = check_X_y(historical_features, long_term_outcome, dtype=float)
        base = self.estimator if self.estimator is not None else RandomForestRegressor(
            n_estimators=200, min_samples_leaf=20, random_state=42, n_jobs=-1)
        self.model_ = clone(base).fit(X, y)
        return self

    def predict(self, features):
        check_is_fitted(self, ["model_"])
        return self.model_.predict(check_array(features, dtype=float))

    def contrast(self, experiment_features, treatment):
        """Unadjusted randomized-arm contrast; not automatically a long-term ATE."""
        scores = self.predict(experiment_features)
        t = np.asarray(treatment)
        if t.shape != scores.shape or not np.isin(t, [0, 1]).all():
            raise ValueError("Treatment must be a row-aligned binary vector")
        if min(np.sum(t == 0), np.sum(t == 1)) < 2:
            raise ValueError("At least two observations per arm required")
        a, b = scores[t == 1], scores[t == 0]
        return {"score_difference": float(a.mean() - b.mean()),
                "conditional_standard_error": float(np.sqrt(a.var(ddof=1)/len(a) + b.var(ddof=1)/len(b)))}
