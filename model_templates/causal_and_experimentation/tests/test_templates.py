"""Small domain-free checks; no data files or generated artifacts."""
import importlib.util
from pathlib import Path
import unittest

import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.exceptions import NotFittedError

ROOT = Path(__file__).resolve().parents[1]


def load(relative):
    spec = importlib.util.spec_from_file_location(Path(relative).stem, ROOT / relative)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


uplift = load('uplift/t_learner.py')
surrogate = load('surrogate_index/surrogate_index.py')
dml = load('double_machine_learning/partial_linear_dml.py')
bayes = load('bayesian_ab/normal_normal.py')
switch = load('switchback/switchback.py')
sequential = load('sequential_testing/group_sequential.py')


class TemplateTests(unittest.TestCase):
    def test_default_estimators_smoke(self):
        rng = np.random.default_rng(10)
        X = rng.normal(size=(160, 2))
        t = np.tile([0, 1], 80)
        y = X[:, 0] + t + rng.normal(size=len(X))
        effects = uplift.TLearner().fit(X, t, y).predict(X[:10])
        scores = surrogate.SurrogateIndex().fit(X, y).predict(X[:10])
        self.assertTrue(np.isfinite(effects).all())
        self.assertTrue(np.isfinite(scores).all())
        d = X[:, 0] + rng.normal(size=len(X))
        result = dml.estimate_dml(X, d, y, folds=2)
        self.assertTrue(np.isfinite(result['effect']))
        self.assertTrue(np.isfinite(result['iid_standard_error']))

    def test_uplift_known_effect(self):
        rng = np.random.default_rng(11)
        X = rng.normal(size=(400, 2))
        t = np.tile([0, 1], 200)
        y = X[:, 0] + t * (2 + X[:, 1])
        model = uplift.TLearner(LinearRegression()).fit(X, t, y)
        new = rng.normal(size=(30, 2))
        np.testing.assert_allclose(model.predict(new), 2 + new[:, 1], atol=1e-10)
        with self.assertRaises(ValueError):
            uplift.TLearner().fit(X, np.zeros(400), y)
        with self.assertRaises(NotFittedError):
            uplift.TLearner().predict(new)

    def test_surrogate_bridge_and_contrast(self):
        X = np.arange(100).reshape(-1, 1)
        model = surrogate.SurrogateIndex(LinearRegression()).fit(X, 2*X[:, 0] + 3)
        new = np.array([[1], [2], [3], [4]])
        result = model.contrast(new, [0, 0, 1, 1])
        self.assertAlmostEqual(result['score_difference'], 4)
        self.assertAlmostEqual(result['conditional_standard_error'], np.sqrt(2))
        with self.assertRaises(ValueError):
            model.contrast(new, [0, 1, 2, 1])

    def test_dml_effect_and_degenerate_treatment(self):
        rng = np.random.default_rng(12)
        X = rng.normal(size=(3000, 2))
        d = X[:, 0] + rng.normal(size=len(X))
        y = 2*d + X[:, 1] + rng.normal(size=len(X))
        result = dml.estimate_dml(X, d, y, LinearRegression(), LinearRegression())
        self.assertAlmostEqual(result['effect'], 2, delta=.08)
        self.assertGreater(result['iid_standard_error'], 0)
        self.assertEqual(len(result['outcome_residuals']), len(X))
        with self.assertRaises(ValueError):
            dml.estimate_dml(X, np.ones(len(X)), y, LinearRegression(), LinearRegression())

    def test_bayesian_conjugacy_flat_prior_and_vectorization(self):
        result = bayes.posterior_effect(2, 1, prior_mean=0, prior_sd=1)
        self.assertAlmostEqual(float(result['mean']), 1)
        self.assertAlmostEqual(float(result['sd']), np.sqrt(.5))
        flat = bayes.posterior_effect([1, 2], [1, 2], prior_sd=np.inf)
        np.testing.assert_allclose(flat['mean'], [1, 2])
        np.testing.assert_allclose(flat['sd'], [1, 2])
        symmetric = bayes.posterior_effect(0, 1)
        self.assertAlmostEqual(float(symmetric['probability_above_minimum']), .5)
        for se in [0, -1, np.nan]:
            with self.assertRaises(ValueError):
                bayes.posterior_effect(1, se)

    def test_switchback_assignment_and_effect(self):
        panel = pd.DataFrame({'unit': np.repeat(np.arange(8), 10),
                              'period': np.tile(np.arange(10), 8)})
        panel['treatment'] = switch.balanced_assignment(panel, seed=15)
        self.assertTrue((panel.groupby('unit').treatment.sum() == 5).all())
        np.testing.assert_array_equal(panel.treatment, switch.balanced_assignment(panel, seed=15))
        panel['outcome'] = 2*panel.treatment + panel.unit + .2*panel.period
        result = switch.randomization_test(panel, permutations=199, seed=16)
        self.assertAlmostEqual(result['fixed_effects_estimate'], 2)
        self.assertLess(result['sharp_null_p_value'], .05)
        self.assertGreaterEqual(result['sharp_null_p_value'], 1/200)
        panel['outcome'] = 0.
        self.assertEqual(switch.randomization_test(panel, permutations=19)['sharp_null_p_value'], 1)
        with self.assertRaises(ValueError):
            switch.balanced_assignment(pd.concat([panel, panel.iloc[[0]]]))

    def test_switchback_confounded_schedule(self):
        panel = pd.DataFrame({'unit': [0, 0, 1, 1], 'period': [0, 1, 0, 1],
                              'treatment': [0, 1, 0, 1], 'outcome': [0, 1, 0, 1]})
        with self.assertRaises(ValueError):
            switch.randomization_test(panel)

    def test_sequential_independent_null_validation(self):
        t = np.arange(1, 6)/5
        z = sequential.simulate_z(t, trials=60_000, seed=32)
        for method in ['obf', 'pocock']:
            b = sequential.calibrate_boundaries(t, method=method, trials=200_000, seed=31)
            rate = sequential.first_crossing(z, b)['rejected'].mean()
            self.assertAlmostEqual(rate, .05, delta=.005)
            self.assertTrue(np.all(np.diff(b) <= 0))
        np.testing.assert_array_equal(z, sequential.simulate_z(t, trials=60_000, seed=32))

    def test_sequential_first_crossing_and_validation(self):
        result = sequential.first_crossing([[3, 0, 0], [0, -3, 0], [0, 0, 0]], [2, 2, 2])
        np.testing.assert_array_equal(result['stop_index'], [0, 1, 2])
        np.testing.assert_array_equal(result['direction'], [1, -1, 0])
        np.testing.assert_array_equal(result['rejected'], [True, True, False])
        for information in [[], [0, 1], [.5, .4, 1], [.2, .9]]:
            with self.assertRaises(ValueError):
                sequential.calibrate_boundaries(information)
        with self.assertRaises(ValueError):
            sequential.first_crossing([[1, np.nan]], [2, 2])


if __name__ == '__main__':
    unittest.main()
