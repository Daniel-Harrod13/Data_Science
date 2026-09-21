# Model Results

Random Forest T-learner performance on holdout subscribers:

- CATE RMSE: 5.38
- CATE MAE: 4.12
- CATE R-squared: 0.680
- CATE correlation: 0.831
- True uplift in top predicted decile: $19.43
- True uplift in bottom predicted decile: $-8.36

Targeting policy:

- Offer threshold: predicted CATE >= $9.31
- Offer group: 900 subscribers
- Do-not-offer group: 2,700 subscribers
- True incremental value captured by top-25% policy: $13,526
- Incremental value vs. random same-size targeting: $9,417
- Share of oracle positive uplift captured at 25% targeting depth: 59.5%

Interpretation: the model separates persuadable, price-sensitive subscribers from subscribers who either would renew anyway or are too disengaged to save profitably.
