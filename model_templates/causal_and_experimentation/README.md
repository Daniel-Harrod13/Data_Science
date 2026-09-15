# Causal & Experimentation Templates: Question-to-Method Map

Copy-ready, domain-neutral building blocks extracted and generalized from our project methods. No business datasets, project simulation scenarios, charts, or hard-coded project paths. The original portfolio projects remain untouched.

## Start with the question

```text
What decision am I trying to make?
|
+-- How should I design or monitor an experiment?
|   +-- Treatment must vary across units and time --> Switchback
|   +-- I need planned interim checks / early stopping --> Group sequential
|
+-- What treatment effect do I need to estimate?
|   +-- Who benefits most? --> Uplift / T-learner
|   +-- The long-term outcome is not ready --> Surrogate index
|   +-- Treatment is observational and confounded --> Partially linear DML
|
+-- How should I incorporate prior evidence?
    +-- Combine a noisy effect estimate with a prior --> Bayesian A/B
```

These methods play different roles; they are not interchangeable and cannot be combined blindly. In particular, switchback dependence does not automatically satisfy the independent-increment assumptions of this sequential template.

| Question | Open this template | Main requirement / warning |
|---|---|---|
| Who benefits from treatment, not merely who has a high outcome? | [Uplift](uplift/README.md) | Pretreatment covariates; randomized treatment or defensible exchangeability and overlap |
| Can early signals bridge to a delayed outcome? | [Surrogate index](surrogate_index/README.md) | A transportable outcome bridge and credible surrogacy—not prediction accuracy alone |
| What is the adjusted effect of a continuous treatment? | [DML](double_machine_learning/README.md) | Measured confounders, residual treatment variation, partially linear effect specification |
| How does prior evidence change my belief about an effect? | [Bayesian A/B](bayesian_ab/README.md) | Defensible prior and approximately normal effect likelihood |
| Can I randomize treatment across unit-periods? | [Switchback](switchback/README.md) | Known assignment design, no carryover/interference, appropriate periods |
| Can I check results before the final sample? | [Sequential testing](sequential_testing/README.md) | Preplanned information schedule and valid correlated Z statistics |

## What this library does not decide for you

Define the estimand, randomization/observation unit, population, endpoint, missingness policy, minimum useful effect, and assumptions first. Predictive performance does not establish causality. No template automatically handles clustered data, multiple outcomes, profit optimization, or post-selection inference.

The DML template intentionally uses the standard **partially linear** core. It does not carry over treatment-band slope heuristics as if they were a generally valid nonlinear dose-response estimator. For heterogeneous or nonlinear effects, choose and validate a suitable extension explicitly.

## Copy and adapt

1. Use the mind map, then read the method's README and limitations.
2. Copy that method's `.py` file and README into the new project. Each module is standalone: no imports from this repository or sibling templates.
3. Install the dependencies listed in that README, or use this folder's `requirements.txt` for all six.
4. Supply your own data using the documented input schema. Array inputs must be finite, numeric, row-aligned, and consistently encoded.
5. Use domain-appropriate training/validation splits. Fit preprocessing inside training folds when learning transformations; do not leak outcomes or future information.
6. Validate assumptions and operating characteristics before making decisions. Add project-specific loading, reporting, and tests outside the model module.

README snippets use placeholder variables rather than invented business examples. Import statements assume the copied module is beside your analysis script. Importing or executing the modules does not read data or write files.

## Install and test

From `Data_Science/`:

```bash
.venv/bin/python -m pip install -r model_templates/causal_and_experimentation/requirements.txt
.venv/bin/python -m unittest discover -s model_templates/causal_and_experimentation/tests -v
```

The small synthetic tests are implementation checks, not portfolio studies or proof that the methods suit a particular dataset. Python 3.9+; dependencies are compatible ranges rather than a locked environment.

### Local environment note

The existing macOS/Python 3.9 environment with NumPy 2.0.2 emits `matmul` divide-by-zero/overflow/invalid warnings even for a standalone small, finite matrix product whose output matches elementwise multiplication and summation exactly. The ordinary test suite passes, but treating RuntimeWarnings as errors fails in this environment. These warnings are not suppressed by the templates. DML and switchback explicitly reject nonfinite intermediate predictions/residuals; investigate warnings in your own numerical stack rather than assuming every warning has this same cause.
