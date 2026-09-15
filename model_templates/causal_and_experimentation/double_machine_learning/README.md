# Partially Linear Double Machine Learning

**Question:** What is the adjusted effect of a continuous treatment after accounting for measured confounders?

**Use when:** Flexible models are needed for confounding adjustment, but a constant treatment slope is a reasonable scientific specification.

**Inputs:** Numeric pretreatment confounders `X` (n × p), finite continuous treatment `treatment` (n,), and continuous `outcome` (n,). Rows are independent observations.

```python
from partial_linear_dml import estimate_dml

result = estimate_dml(X, treatment, outcome, folds=5, seed=42)
print(result['effect'], result['iid_standard_error'])
```

**Core:** Cross-fit models of E[Y|X] and E[D|X]; regress outcome residuals on treatment residuals without an intercept. The maintained model is `Y = theta * D + g(X) + error`. The effect is outcome units per treatment unit.

**Outputs:** `effect`, asymptotic `iid_standard_error`, `residual_treatment_variance`, and both residual arrays. A small residual treatment variance indicates weak identifying variation; the numerical guard is not a substantive overlap diagnostic.

**Adapt:** Supply cloneable sklearn regressors via `outcome_model=` and `treatment_model=`. Fit learned preprocessing within folds using pipelines; tune nuisance learners without leaking held-out fold outcomes. Assess stability across splits and nuisance models.

**Assumptions:** No unmeasured confounding, consistency, no interference, sufficient residual treatment variation, partially linear effect structure, and adequate nuisance estimation rates for DML inference.

**Do not use as-is:** Clustered or longitudinal observations (need appropriate folds and inference), post-treatment controls, nonlinear dose-response curves, or heterogeneous effects interpreted as a universal slope. Cross-fitting is not a cure for omitted confounders.

This is intentionally a standard DML core, not the project-specific treatment-band slope heuristic. Estimating within bands defined by realized treatment is not automatically a valid local causal derivative. Choose a justified heterogeneous/dose-response extension instead.

**Dependencies:** NumPy, scikit-learn. [Back to mind map](../README.md).
