# Predicting-Mathematics-Self-Efficacy-using-the-PISA-2022-Survey
# Data Science Project – PISA 2018 (MATHEFF)

This project explores and predicts **mathematics self-efficacy (MATHEFF)** using data from the PISA 2022 student and school questionnaires. After merging, cleaning, selecting conceptual variables, and handling missing values, multiple machine learning models are trained to evaluate which factors best explain MATHEFF across countries.

## Main Steps
- Read and merge PISA student and school SPSS files using `CNTSCHID`.
- Build a variable dictionary and keep only conceptual (index-level) variables.
- Remove high-missing variables, compute correlations, and produce heatmaps.
- Select predictors (|r| ≥ 0.05 + key controls) and impute missing values (median/mode).
- Standardize continuous variables and split data into train/validation/test.
- Train and compare:  
  **OLS (top 20 vars), Ridge, Lasso, Elastic Net, Random Forest, XGBoost**.
- Export tables for RMSE/R² and variable importance (Lasso, RF, XGB).
- Produce graphs: correlation heatmaps, predicted vs actual, residuals, importance plots, and a world map of median MATHEFF.

## Key Results
- **XGBoost** achieves the best predictive accuracy.  
- Motivation, anxiety, home background, and school climate indices are consistently influential.  
- Many moderate predictors collectively contribute to improvements in R².

## Files
- Main script: `Lucas_Salamanca_FinalProject.R`
- Report: `Lucas_Salamanca_FinalProject.pdf`
- Exported outputs: dictionaries, cleaned datasets, correlations, imputations, model metrics, importance tables, and plots.

## Requirements
R (≥ 4.0) and packages: `haven`, `dplyr`, `ggplot2`, `glmnet`, `randomForest`, `xgboost`, `sf`, `rnaturalearth`, etc.

## How to Run
1. Download PISA 2022 SPSS files and update file paths in the script.  
2. Run the R script sequentially (preprocessing → modeling → plots).  
3. Outputs will be saved automatically in the working directory.

Author: **Lucas Salamanca**
