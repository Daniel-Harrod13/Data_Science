# Identification Strategy

This project uses a before/after interrupted time-series style design around the 2023 MLB pitch clock rule change.

## Why This Is Not a Perfect Experiment

The pitch clock was adopted league-wide, so there is no clean untreated MLB control group after 2023. The design estimates a sharp league-wide change after the rule change while controlling for observable game characteristics and seasonality.

## Estimation Approaches

1. **Naive pre/post difference**
   - Average duration after 2023 minus average duration before 2023.

2. **Adjusted regression**
   - Regress game duration on `post_pitch_clock` and controls.
   - Controls include total runs, game length in outs, attendance, day/night status, month, and ballpark fixed effects.

3. **Monthly event-style summary**
   - Plot monthly average duration over time with a visible 2023 treatment boundary.

## Key Limitation

Because every MLB team received the treatment at the same time, the estimate should be interpreted as an association consistent with a causal effect, not as definitive proof. Other league-wide changes in 2023 could also contribute.
