import sys
import unittest
from pathlib import Path

import numpy as np
from scipy.stats import norm

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))
from sequential_testing import simulate_z, calibrate, first_crossing


class SequentialTests(unittest.TestCase):
    def test_first_crossing_and_no_crossing(self):
        z = np.array([[3., 0., 4.], [0., -3., 4.], [0., 0., 0.], [0., 0., 3.]])
        rejected, day, direction = first_crossing(z, [2, 2, 2], [10, 20, 30])
        np.testing.assert_array_equal(rejected, [True, True, False, True])
        np.testing.assert_array_equal(day, [10, 20, 30, 30])
        np.testing.assert_array_equal(direction, [1, -1, 0, 1])

    def test_invalid_information(self):
        for t in ([0, 1], [.5, .4, 1], [.2, .9]):
            with self.assertRaises(ValueError):
                simulate_z(np.random.default_rng(1), 10, t)

    def test_independent_null_validation(self):
        t = np.arange(1, 6) / 5
        calibration = simulate_z(np.random.default_rng(11), 200_000, t)
        evaluation = simulate_z(np.random.default_rng(22), 100_000, t)
        boundaries = calibrate(calibration, t)
        self.assertTrue(np.all(np.diff(boundaries["O'Brien–Fleming"]) < 0))
        self.assertTrue(np.all(np.diff(boundaries['Pocock']) == 0))
        for boundary in boundaries.values():
            rate = first_crossing(evaluation, boundary, np.arange(1, 6))[0].mean()
            self.assertLess(abs(rate - .05), .004)
        naive = first_crossing(evaluation, np.repeat(norm.ppf(.975), 5), np.arange(1, 6))[0].mean()
        self.assertGreater(naive, .10)
        self.assertLess(abs(np.mean(abs(evaluation[:, -1]) >= norm.ppf(.975)) - .05), .004)
        self.assertAlmostEqual(np.corrcoef(evaluation[:, 0], evaluation[:, -1])[0, 1], np.sqrt(.2), delta=.01)

    def test_single_look_and_reproducibility(self):
        z = simulate_z(np.random.default_rng(33), 100_000, [1])
        bounds = calibrate(z, [1])
        np.testing.assert_array_equal(bounds["O'Brien–Fleming"], bounds['Pocock'])
        self.assertAlmostEqual(bounds['Pocock'][0], norm.ppf(.975), delta=.02)
        np.testing.assert_array_equal(z, simulate_z(np.random.default_rng(33), 100_000, [1]))


if __name__ == '__main__':
    unittest.main()
