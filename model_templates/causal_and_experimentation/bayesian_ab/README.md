# Bayesian A/B: Normal–Normal Updating

**Question:** How does prior evidence change my belief about a treatment effect?

**Use when:** You have an approximately normal effect estimate with a defensible standard error and want transparent prior sensitivity analysis.

**Inputs:** `estimate` (treatment minus control), positive `standard_error`, `prior_mean`, positive `prior_sd`, and a `minimum_effect` of interest. All use the same outcome units. For rates use proportions consistently, not a mixture of proportions and percentage points. Estimates and standard errors may be scalars or broadcast-compatible arrays.

```python
from normal_normal import posterior_effect

result = posterior_effect(
    estimate=effect_estimate,
    standard_error=effect_se,
    prior_mean=historical_mean,
    prior_sd=historical_uncertainty,
    minimum_effect=minimum_useful_effect,
)
print(result['probability_above_minimum'])
# Flat-prior baseline:
baseline = posterior_effect(effect_estimate, effect_se, prior_sd=float('inf'))
```

**Outputs:** Posterior `mean`, `sd`, probability of exceeding the minimum effect, and a central 95% credible interval (`credible_low`, `credible_high`). These are posterior statements, not frequentist error guarantees.

**Adapt:** Compare skeptical, historical, and weak priors. Do not use the current experiment twice by estimating its prior from its own outcomes. Historical winners alone are a selected sample, not an unbiased prior for all new experiments. Account for uncertainty and relevance when translating history into a prior.

**Assumptions:** Approximately normal effect likelihood with supplied standard error treated as known; independent, defensible prior evidence; appropriate effect scale.

**Do not use as-is:** Sparse binary outcomes needing an exact binomial likelihood, complex dependence, or automatic sequential shipping decisions. A posterior probability cutoff does not automatically control a repeated-monitoring false-positive rate. Simulate the complete decision policy under null and alternative scenarios before choosing thresholds.

**Dependencies:** NumPy, SciPy. [Back to mind map](../README.md).
