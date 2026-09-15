# Surrogate Index

**Question:** Can short-term signals estimate a delayed treatment effect?

**Use when:** Historical data contain early signals and the eventual outcome, while a new randomized experiment has only the early signals available.

**Inputs:** Historical numeric features (short-term surrogates and relevant baseline covariates), historical long-term outcome, and matching feature columns from a separate randomized experiment with binary treatment assignment.

```python
from surrogate_index import SurrogateIndex

bridge = SurrogateIndex().fit(historical_features, historical_outcome)
scores = bridge.predict(experiment_features)
result = bridge.contrast(experiment_features, treatment)
```

**Output:** Predicted long-term scores; randomized-arm `score_difference`; `conditional_standard_error` for independent experiment observations conditional on the fitted bridge. This standard error excludes uncertainty in training the bridge. It is not a complete uncertainty estimate for a long-term ATE.

**Adapt:** Pass a cloneable sklearn regressor as `estimator=`. Validate predictions on untouched historical observations and examine feature support in the new experiment. Use a two-sample bootstrap with bridge refitting if appropriate for full sampling uncertainty.

**Assumptions:** The long-term outcome relationship given surrogates/baseline features transports to the new experiment; surrogates capture the treatment's relevant effects on the long-term outcome; overlap and consistent measurement hold. Prediction accuracy alone does not validate these causal assumptions.

**Do not use as-is:** Treatments with direct long-term effects not captured by the bridge, shifting populations, observational treatment contrasts, unequal assignment probabilities without adjustment, or clustered observations. Until assumptions are justified, label the result an effect on predicted scores—not established long-term impact.

**Dependencies:** NumPy, scikit-learn. [Back to mind map](../README.md).
