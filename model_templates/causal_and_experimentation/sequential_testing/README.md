# Group-Sequential Testing

**Question:** Can I check an experiment before completion without inflating false-positive risk?

**Use when:** Analysis times are planned in information units and your standardized effect statistics follow the canonical Gaussian independent-increment model.

**Inputs:** Strictly increasing information fractions ending at 1, a two-sided alpha, and a choice of `obf` (O’Brien–Fleming) or `pocock`. Information is inverse-variance information relative to the maximum, not necessarily elapsed time or raw sample fraction.

```python
from group_sequential import calibrate_boundaries, simulate_z, first_crossing

information = [0.2, 0.4, 0.6, 0.8, 1.0]
boundaries = calibrate_boundaries(information, method='obf', seed=42)
# Independent simulation validates calibration; never reuse the calibration seed.
null_z = simulate_z(information, trials=100_000, seed=73)
validation = first_crossing(null_z, boundaries)
print(validation['rejected'].mean())
```

**Core:** Simulate correlated Z paths with `Corr(Z(s), Z(t)) = sqrt(s/t)`. Calibrate the maximum standardized absolute Z statistic. O’Brien–Fleming boundaries have shape `c/sqrt(t)`; Pocock uses constant `c`. Monte Carlo calibration approximates the desired alpha; it is not an exact analytic guarantee or a general alpha-spending implementation.

**Outputs:** A boundary vector, simulated Z paths, and first-crossing decisions (`rejected`, zero-based `stop_index`, signed `direction`). A non-rejecting path ends at the last look with direction zero. `first_crossing` expects complete paths for retrospective/simulation use; for a live experiment, compare the current absolute Z with its pre-specified boundary and stop at the first crossing. Never require unobserved future outcomes to make a current decision.

**Adapt:** Define endpoint, estimand, variance model, maximum information, and looks before collecting outcomes. `simulate_z` accepts an alternative effect in final-standard-error units via `final_effect_z`. Validate null risk, correct-direction power, and expected information used on independent simulations. Calibration uses memory proportional to trials × looks.

**Choice:** O’Brien–Fleming has a high early hurdle and a final threshold closer to the fixed-horizon threshold. Pocock permits easier early detection but sacrifices final-look sensitivity at the same maximum sample size.

**Do not use as-is:** Unplanned extra looks, arbitrary schedule changes, correlated/clustered observations without a valid information process, multiple endpoints without adjustment, or naïve post-stopping fixed-horizon confidence intervals. Negative crossing means evidence of a negative effect; no crossing is not proof of no effect. No futility boundaries or sequentially adjusted effect intervals are supplied.

**Dependencies:** NumPy. [Back to mind map](../README.md).
