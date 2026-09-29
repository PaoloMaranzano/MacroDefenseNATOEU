# Replication package: *Military deindustrialisation and economic performance in Europe*

Stamegna M., Maranzano P., Mombelli S., Pianta M. (2026)

This folder contains the data and code needed to reproduce every table and figure in the
empirical part of the paper: Tables 2 and 3, the robustness tables and the residual diagnostics
of Appendix B, and Figure 5.

## Contents

```
StamegnaEtAl2026_SCED/
├── README.md                 this file
├── data/
│   └── dataset_paperSCED.dta  panel used in the paper (Stata format)
├── SCED_replication.R        main script: estimates, Word tables and figures
├── SCED_replication.do       Stata version of the estimates and diagnostics (same numbers)
├── check_stata_vs_R.R        checks that the Stata and R results are identical
└── output/
    ├── R/                    results of SCED_replication.R (tables, figures, csv files)
    └── stata/                results of SCED_replication.do (csv files and log)
```

## Data

`data/dataset_paperSCED.dta` is an annual panel of five European NATO members (France, Germany,
Italy, Spain and the United Kingdom) over 1981–2024, with 220 country-year observations. It is a
small subset of the broader database described in the [main README](../README.md) of this
repository.

| Variable | Description | Unit | Source |
|---|---|---|---|
| `Log_MProductiv` | Manufacturing labour productivity: value added per person employed | log, constant 2015 USD | OECD STAN, AMECO |
| `DefGDP` | Military expenditure | % of GDP | NATO |
| `DefInd` | Military specialisation: value added of the manufacturing sectors of the Aerospace and Defence ecosystem | % of manufacturing value added | OECD STAN, AMECO |
| `Exportman_GDP` | Manufacturing exports | % of GDP | OECD ICIO, WIOD; NATO (GDP) |
| `Importman_GDP` | Manufacturing imports | % of GDP | OECD ICIO, WIOD; NATO (GDP) |
| `MInvGDP` | Manufacturing gross fixed capital formation | % of GDP | OECD STAN, AMECO; NATO (GDP) |
| `Log_CivGBARD` | Government budget allocations for civilian R&D | log, constant 2015 USD | OECD MSTI |
| `Log_DefGBARD` | Government budget allocations for military R&D | log, constant 2015 USD | OECD MSTI |
| `SocialExpenditure_ShareGDP` | Social expenditure (not used in the final specifications) | % of GDP | OECD SOCX |
| `D0910`, `D20` | Crisis dummies: 2009–2010 and 2020–2021 | 0/1 | — |

German data start in 1991, after reunification, and Spanish military expenditure starts in 1985;
these years are left missing. Military expenditure and GDP are observed up to 2024. For the other
series, the most recent years that the sources do not yet cover are extrapolated by applying to the
last observation the compound average annual growth rate of the previous ten years:

* 2024: `Log_MProductiv`, `DefInd`, `MInvGDP`, `Log_CivGBARD` and `Log_DefGBARD` (GBARD: 2023–2024 for
  the United Kingdom);
* 2023–2024: `Exportman_GDP` (2021–2024 for the United Kingdom);
* 2021–2024: `Importman_GDP`.

For Italy, GBARD comes from a revised series, whose missing values for 2002–2004 are filled by linear
interpolation. Table A1 of the paper reports the definition, source and coverage of every variable.

## Models

Tables 2 and 3 use a different military variable: military expenditure as a share of GDP
(Table 2) and military specialisation (Table 3). Each model is estimated separately for
France–United Kingdom (FRUK) and Germany–Italy–Spain (GIS), with three specifications: (1)
civilian R&D, (2) military R&D, (3) both. All specifications include manufacturing exports,
imports and investment (% of GDP) and the two crisis dummies.

* **Main model**: country fixed effects with AR(1) errors.
* **Robustness check**: pooled model (common intercept) with AR(1) errors.

Both models are estimated with Prais–Winsten feasible GLS, using one autoregressive coefficient
per country group and panel-corrected standard errors (PCSE; Beck and Katz, 1995). This is the
Stata command `xtpcse ..., correlation(ar1)`. The R function `pw_pcse()` codes the estimator step
by step and reproduces the Stata results exactly.

## Requirements

* **R 4.x** with the packages `haven`, `ggplot2`, `ggpubr`, `officer` and `flextable`:
  `install.packages(c("haven", "ggplot2", "ggpubr", "officer", "flextable"))`
* **Stata 16 or later**. No user-written packages are needed.

## How to run

1. **R**: set the working directory to this folder, then run `source("SCED_replication.R")`. It takes about 20 seconds.
2. **Stata**: set `global ROOT` at the top of `SCED_replication.do` to this folder, then run the whole file. It takes a few seconds.
3. **Check**: in R, run `source("check_stata_vs_R.R")`. It prints `ALL CHECKS PASSED` when the formatted
   tables are identical character by character and all other results agree up to 1e-6.

## Output

The R script produces everything in the paper (in `output/R/`):

| File | Content |
|---|---|
| `SCED_tables.docx` | Tables 2 and 3 (main model), then the robustness tables, one per page |
| `figures/Figure_coefficients_military.png` | Figure 5: coefficients of the military variables, with 95% confidence intervals |
| `figures/Figure_coefficients_all.png` | Coefficients of all regressors |
| `figures/Figure_diagnostic_tests.png` | Ljung–Box, Jarque–Bera and constant-variance tests on the AR(1) innovations |
| `figures/Figure_diagnostics_*.png` | Residual diagnostics for each table and model: innovations over time, autocorrelation functions and normal Q–Q plots |

Both scripts write the same numerical results (in `output/R/` and `output/stata/`):

| File | Content |
|---|---|
| `Table2_*.csv`, `Table3_*.csv` | Formatted tables |
| `estimates_long.csv` | All coefficients (country intercepts included) with standard errors, z, p-values, N, ρ and R² |
| `residuals.csv` | Fitted values, residuals and AR(1) innovations |
| `diagnostics_residuals.csv` | Residual diagnostics by model, country and residual type |
| `diagnostics_crosscountry.csv` | Correlation of residuals between countries of the same group |

## Citation

Stamegna, M., Maranzano, P., Mombelli, S., & Pianta, M. (2026). *Military deindustrialisation and
economic performance in Europe*. Working paper.
