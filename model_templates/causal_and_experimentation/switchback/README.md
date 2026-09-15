# Switchback Experiment

**Question:** Can I randomize treatment across units and periods when individual-level randomization is impractical?

**Use when:** Units can change treatment status over time, outcomes can be attributed to those periods, and carryover/interference can be prevented or explicitly modeled.

**Inputs:** A pandas DataFrame with one row per `unit`, `period` pair. Add `treatment` (0/1) and finite `outcome` after running the experiment. Use consistently typed unit and period identifiers. Every unit needs at least two periods.

```python
from switchback import balanced_assignment, randomization_test

# BEFORE running the experiment; save the planned schedule.
panel['treatment'] = balanced_assignment(panel, seed=42)
# AFTER collecting outcomes, without overwriting the realized assignment:
result = randomization_test(panel, permutations=1999, seed=73)
print(result['fixed_effects_estimate'], result['sharp_null_p_value'])
```

**Assignment design:** Within each unit, uniformly choose exactly `floor(number_of_periods / 2)` treated periods. Assign independently across units. This does not force alternation or ensure both arms occur within every calendar period. Inspect identifiability before running; outcome-independent restrictions on assignment require a correspondingly restricted randomization distribution.

**Analysis:** Residualize outcome and treatment against unit and period fixed effects. Report the OLS treatment coefficient. The two-sided Monte Carlo randomization test uses a fixed-effects outcome score `treatment @ residualized_outcome`, regenerating assignments under the specified design. It does not use a permuted t-statistic or return a confidence interval. The plus-one p-value tests the Fisher sharp null: no effect for any unit-period.

**Outputs:** `fixed_effects_estimate`, `sharp_null_p_value`, `observed_score`, and `null_scores`. The point estimate equally weights rows; choose outcome aggregation and weighting deliberately.

**Adapt:** Choose period length and washout before assignment. If you use paired blocks, forced alternation, stratification, or constrained schedules, change both assignment generation and the randomization test. Counts matching this design do not prove historical data actually followed it.

**Do not use as-is:** Observational schedules, post-assignment exclusions, missing outcome-dependent periods, carryover, interference between units, or treatment fully confounded with unit/time effects. No clustered asymptotic standard errors are supplied. A sharp-null test is not an equivalence test or a test of every possible average-effect null.

**Dependencies:** NumPy, pandas. [Back to mind map](../README.md).
