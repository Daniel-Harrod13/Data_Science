"""Monte Carlo O'Brien-Fleming/Pocock boundaries for planned two-sided Z tests."""
import numpy as np


def _information(values):
    t = np.asarray(values, dtype=float)
    if t.ndim != 1 or len(t) == 0 or not np.isfinite(t).all():
        raise ValueError("Information must be a nonempty finite vector")
    if np.any(np.diff(np.r_[0., t]) <= 0) or not np.isclose(t[-1], 1):
        raise ValueError("Information must increase strictly from above zero to 1")
    return t


def simulate_z(information, trials=100_000, final_effect_z=0.0, seed=42):
    t = _information(information)
    if trials < 1 or not np.isfinite(final_effect_z):
        raise ValueError("Positive trials and finite effect required")
    increments = np.random.default_rng(seed).normal(size=(trials, len(t))) * np.sqrt(np.diff(np.r_[0., t]))
    return np.cumsum(increments, axis=1)/np.sqrt(t) + final_effect_z*np.sqrt(t)


def calibrate_boundaries(information, method="obf", alpha=.05, trials=500_000, seed=42):
    """Calibrate BEFORE the experiment; validation must use a different seed."""
    t = _information(information)
    if method not in {"obf", "pocock"} or not 0 < alpha < 1:
        raise ValueError("Use method='obf'/'pocock' and 0 < alpha < 1")
    z = simulate_z(t, trials=trials, seed=seed)
    shape = 1/np.sqrt(t) if method == "obf" else np.ones(len(t))
    constant = np.quantile(np.max(np.abs(z)/shape, axis=1), 1-alpha)
    return shape * constant


def first_crossing(z, boundaries):
    """Complete planned paths only. Return zero-based index, rejection, direction."""
    z = np.asarray(z, dtype=float)
    b = np.asarray(boundaries, dtype=float)
    if z.ndim != 2 or z.shape[0] == 0 or b.ndim != 1 or len(b) == 0 or z.shape[1] != len(b):
        raise ValueError("Expected trials-by-looks Z matrix and matching boundary vector")
    if not np.isfinite(z).all() or not np.isfinite(b).all() or np.any(b <= 0):
        raise ValueError("Finite Z and positive finite boundaries required")
    crossings = np.abs(z) >= b
    rejected = crossings.any(axis=1)
    index = np.where(rejected, crossings.argmax(axis=1), len(b)-1)
    direction = np.where(rejected, np.sign(z[np.arange(len(z)), index]), 0)
    return {"rejected": rejected, "stop_index": index, "direction": direction}
