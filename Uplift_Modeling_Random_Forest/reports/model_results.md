# Model Results

Random Forest T-learner performance on holdout data:

- CATE RMSE: 4.13
- CATE MAE: 3.29
- CATE R-squared: 0.821
- CATE correlation: 0.910
- True uplift in top predicted decile: 42.35
- True uplift in bottom predicted decile: 11.27

Targeting policy:

- Treat customers with predicted CATE >= 31.57
- Would treat: 900 customers
- Excluded: 2,100 customers

A positive gap between top and bottom deciles indicates the model can rank customers by expected incremental impact.
