# Uplift / T-Learner

**Question:** Who benefits most from treatment?

**Use when:** You want conditional treatment-effect predictions rather than ordinary outcome predictions. This starter targets continuous outcomes using separate regressors for treatment and control.

**Inputs:** Numeric pretreatment feature matrix `X` (n × p), binary assignment `treatment` (n,), continuous `outcome` (n,). Both arms must be represented. Predict on held-out/new features with the same columns and order.

```python
from t_learner import TLearner

model = TLearner().fit(X_train, treatment_train, outcome_train)
predicted_effect = model.predict(X_test)
```

**Output:** Treated-minus-control predictions for each row. Positive values indicate predicted outcome increases, which are not necessarily beneficial if lower outcomes are preferred.

**Adapt:** Pass a cloneable sklearn regressor as `estimator=`. Tune on training data; assess uplift ranking and policy value on untouched randomized holdout data. Individual true effects are not observed in real data, so ordinary CATE RMSE cannot be calculated without an oracle.

**Assumptions:** Consistency, no interference, overlap, and randomized assignment or conditional exchangeability given measured pretreatment confounders. Do not include post-treatment features.

**Do not use as-is:** Sparse treatment arms, unsupported feature regions, binary classification with class-label subtraction, or causal claims from unmeasured confounding. No effect confidence intervals or propensity-based correction are supplied.

**Dependencies:** NumPy, scikit-learn. [Back to mind map](../README.md).
