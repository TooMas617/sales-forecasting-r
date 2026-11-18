# Monthly Sales Forecasting

Time series analysis and forecasting of the monthly sales of a local interior design company (Peri Arreda), from January 2017 to December 2023 (84 observations).
The project compares classical regression, diffusion and ARIMA-family models, and models the COVID-19 shock with an exogenous variable.

Project for the course *Business, Economic and Financial Data*, M.Sc. in Data Science, University of Padova (A.Y. 2024/25).

## Methods

- **Exploratory analysis**: time plot, ACF and PACF.
- **Linear models**: trend-only and trend + monthly seasonality (`tslm`), with Durbin-Watson test on residuals.
- **Generalized additive models (GAM)**: spline and loess smoothing of the trend, plus seasonality.
- **Diffusion models**: Bass model, Generalized Bass Model with a rectangular shock, Guseo-Guidolin model (`DIMORA`).
- **ARIMA / SARIMA**: model selection by AIC and residual diagnostics (Ljung-Box test, ACF).
- **ARIMAX / SARIMAX**: the COVID-19 drop (March-September 2020) is modelled as an exogenous variable with exponential decay.
- **Forecast evaluation**: 70/30 train/test split, RMSE and MASE, then a 12-month forecast on the full series.

## Main results

The best model is **SARIMAX(1,1,1)(0,1,1)[12]** with the COVID-19 shock as exogenous regressor (AIC ≈ 1660, residuals consistent with white noise).

| Metric | Train | Test |
|---|---|---|
| RMSE | 22,326.55 | 28,764.25 |
| MASE | 0.635 | 0.919 |

MASE < 1 on the test set means the model beats the naive (random walk) forecast also on unseen data, although the gap between train and test errors shows limited generalization on such a short series.

## Repository structure

```
├── forecasting.R              # full analysis, organised in RStudio sections
├── sales-forecasting-r.Rproj  # RStudio project: sets the working directory
├── data/
│   └── sales.csv              # monthly sales (semicolon-separated, decimal comma)
└── report/
    └── BEFD_report.pdf        # project report (14 pages)
```

## How to run

1. Install the required packages:
   ```r
   install.packages(c("ggplot2", "zoo", "lmtest", "forecast", "DIMORA", "fpp2", "gam", "mgcv"))
   ```
2. Open `sales-forecasting-r.Rproj` in RStudio, so that the working directory is the project folder.
3. Open `forecasting.R` and run it (Ctrl + Shift + Enter). Use the outline (Ctrl + Shift + O) to jump between sections.

## Limitations

- The series is short (84 months), so the test set covers only about two years.
- AIC values are compared across models with different orders of differencing, which makes them only indicative.

## Authors

Tomas Guarini, Arianna Morandi, Beatrice Spagnolo
