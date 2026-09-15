"""Normal effect likelihood with a normal prior; fixed-analysis building block."""
import numpy as np
from scipy.stats import norm


def posterior_effect(estimate, standard_error, prior_mean=0.0, prior_sd=1.0, minimum_effect=0.0):
    """All arguments use the same outcome units; prior_sd=inf is a flat prior."""
    estimate = np.asarray(estimate, dtype=float)
    se = np.asarray(standard_error, dtype=float)
    if not np.isfinite(estimate).all() or not np.isfinite(se).all() or np.any(se <= 0):
        raise ValueError("Finite estimates and positive finite standard errors required")
    if not np.isfinite(prior_mean) or np.isnan(prior_sd) or prior_sd <= 0 or not np.isfinite(minimum_effect):
        raise ValueError("Invalid prior or minimum effect")
    prior_precision = 0.0 if np.isinf(prior_sd) else 1.0 / prior_sd**2
    variance = 1.0 / (1.0/se**2 + prior_precision)
    mean = variance * (estimate/se**2 + prior_mean*prior_precision)
    sd = np.sqrt(variance)
    return {"mean": mean, "sd": sd,
            "probability_above_minimum": norm.cdf((mean-minimum_effect)/sd),
            "credible_low": mean - norm.ppf(.975)*sd,
            "credible_high": mean + norm.ppf(.975)*sd}
