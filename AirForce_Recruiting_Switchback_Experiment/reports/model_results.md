# Air Force Recruiting Switchback Results

Scenario: randomized region-day switchback test of reduced paid-media bids for recruiting campaigns.

Average observed differences on reduced-bid region-days:

- Ad spend saved per region-day: $669.42
- Change in first-party total qualified leads per region-day: -2.04
- Change in platform-attributed leads per region-day: -21.74
- Baseline cost per qualified lead: $33.35
- Reduced-bid cost per qualified lead: $18.22

Fixed-effects switchback estimates:

                   metric  switchback_effect      p_value
                 ad_spend        -669.834617 2.149518e-68
    total_qualified_leads          -2.119879 7.164851e-03
platform_attributed_leads         -21.728989 6.467050e-79

Permutation test for total qualified leads:

- Observed treatment-control difference: -2.04
- Randomization-inference p-value: 0.043

Interpretation: the platform-attributed lead count falls much more than first-party total qualified leads. In this simulated Air Force recruiting setting, reduced bids lower media cost while preserving most measured recruiting demand, suggesting the baseline bids were partly cannibalizing organic/direct interest.
