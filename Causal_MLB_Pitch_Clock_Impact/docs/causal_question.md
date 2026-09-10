# Causal Question

## Main Question

What was the impact of MLB's 2023 pitch clock rule change on game duration?

## Treatment

The treatment is the introduction of the MLB pitch clock at the start of the 2023 regular season.

- `post_pitch_clock = 0`: games before 2023
- `post_pitch_clock = 1`: games in 2023 and later

## Outcome

Primary outcome:

- `duration_minutes`: official game length in minutes

Secondary outcome:

- `total_stolen_bases`: total stolen bases by both teams in the game

## Unit of Analysis

One MLB regular-season game.

## Estimand

Average change in game duration after the pitch clock rule, adjusting for observable game characteristics such as total runs, innings/outs, attendance, day/night games, ballpark, and month.
