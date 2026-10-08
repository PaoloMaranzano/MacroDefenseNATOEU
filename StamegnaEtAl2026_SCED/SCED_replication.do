/*==============================================================================
  Military deindustrialisation and economic performance in Europe
  Stamegna M., Maranzano P., Mombelli S., Pianta M.

  REPLICATION SCRIPT (Stata): Tables 1, 2 and 3 and residual diagnostics
  ------------------------------------------------------------------------------
  File       : SCED_replication.do
  Companion  : SCED_replication.R  (same steps, same numbers)
  Data       : data/dataset_paperSCED.dta  (FR, DE, IT, ES, UK; 1981-2024)
  Software   : Stata 16 or later. No user-written packages are needed.

  WHAT IS ESTIMATED
  Dependent variable: log manufacturing labour productivity (Log_MProductiv).

  One military variable per table:
     Table 2 -> military expenditure, % of GDP                     (DefGDP)
     Table 3 -> military value added, % of manufacturing VA        (DefInd)

  Two groups of countries, always estimated separately:
     FRUK = France and United Kingdom
     GIS  = Germany, Italy and Spain

  Three specifications (table columns), which differ only in the R&D controls:
     (1) civilian government R&D      (2) military government R&D
     (3) civilian + military R&D
  Every specification also contains manufacturing exports, imports and
  investment (% of GDP) and two crisis dummies (2009-2010 and 2020-2021).

  Two estimators:
     FE_AR1      MAIN MODEL : country fixed effects + AR(1) errors
     POOLED_AR1  ROBUSTNESS : one common intercept  + AR(1) errors
  Both are Prais-Winsten estimates with panel-corrected standard errors
  (PCSE; Beck and Katz, 1995), from
     xtpcse ..., correlation(ar1)
  i.e. - one AR(1) coefficient (rho) common to the countries of a group;
       - PCSE allow each country to have its own error variance and the
         errors of different countries to be correlated in the same year;
       - no deterministic time trend.

  In the FE tables the constant is not reported: with country dummies it is
  only the intercept of the reference country (France in FRUK, Germany in GIS).
  All intercepts are saved in estimates_long.csv.

  ROLE OF THIS SCRIPT
  The tables (Word) and figures of the paper are produced by SCED_replication.R.
  This Stata script produces the same numbers in csv files; the script
  check_stata_vs_R.R verifies that the two sets of results are identical.

  OUTPUT (folder output/stata)
     Table1_growth_rates.csv     Table 1: average annual growth rates (section 7)
     Table2_FE_AR1.csv, Table2_POOLED_AR1.csv   formatted Table 2
     Table3_FE_AR1.csv, Table3_POOLED_AR1.csv   formatted Table 3
     estimates_long.csv          every coefficient with SE, z, p-value, N, rho, R2
     residuals.csv               fitted values, residuals and AR(1) innovations
     diagnostics_residuals.csv   residual diagnostics by model and country
     diagnostics_crosscountry.csv  correlation of residuals between countries
     SCED_replication_log.txt    full Stata log

  HOW TO RUN: set the path in section 0, then run the whole file.
==============================================================================*/


*------------------------------------------------------------------------------
* 0. SET-UP
*------------------------------------------------------------------------------
version 16
clear all
set more off
set varabbrev off            // variable names must be typed in full

* >>> Set the path of the replication folder (the one that contains this file)
global ROOT "."
cd "$ROOT"

global OUT "output/stata"
capture mkdir "output"
capture mkdir "$OUT"

capture log close
log using "$OUT/SCED_replication_log.txt", text replace


*------------------------------------------------------------------------------
* 1. DATA
*------------------------------------------------------------------------------
use "data/dataset_paperSCED.dta", clear

* Panel structure: one row per country and year.
* encode sorts countries alphabetically: DE=1, ES=2, FR=3, IT=4, UK=5.
encode country, gen(pid)
xtset pid year

* Country groups
gen str4 block = cond(inlist(country, "FR", "UK"), "FRUK", "GIS")

* Country fixed effects, written as explicit dummy variables.
* The first country of each group (alphabetical) is the base category:
* France in FRUK and Germany in GIS. Their intercept is the "Constant".
gen byte FE_UK = country == "UK"
gen byte FE_ES = country == "ES"
gen byte FE_IT = country == "IT"

tempfile fulldata
save `fulldata'


*------------------------------------------------------------------------------
* 2. MODEL SET-UP
*------------------------------------------------------------------------------
local y       Log_MProductiv
local core    Exportman_GDP Importman_GDP MInvGDP     // trade and investment
local crisis  D0910 D20                               // crisis dummies

* Extra controls of the three specifications (table columns 1-3)
local extra1  Log_CivGBARD
local extra2  Log_DefGBARD
local extra3  Log_CivGBARD Log_DefGBARD

* Country dummies used by the fixed-effects model in each group
local fe_FRUK FE_UK
local fe_GIS  FE_ES FE_IT

* Row labels of the tables
local lab_DefGDP                      "Military expenditure / GDP"
local lab_DefInd                      "Military VA / manufacturing VA"
local lab_Exportman_GDP               "Manufacturing exports / GDP"
local lab_Importman_GDP               "Manufacturing imports / GDP"
local lab_MInvGDP                     "Manufacturing investment / GDP"
local lab_Log_CivGBARD                "Civilian government R&D (log)"
local lab_Log_DefGBARD                "Military government R&D (log)"
local lab_D0910                       "Crisis indicator: 2009-2010"
local lab_D20                         "Crisis indicator: 2020-2021"
local lab__cons                       "Constant"


*------------------------------------------------------------------------------
* 3. ESTIMATION: 2 military variables x 3 specifications x 2 groups x 2 models
*------------------------------------------------------------------------------
* What xtpcse, correlation(ar1) does, step by step
* (the R script codes the same steps explicitly):
*   Step 1. OLS of y on the regressors; keep the residuals e(it).
*   Step 2. For each country i: rho_i = sum_t e(it)e(i,t-1) / sum_t e(i,t-1)^2.
*           Common rho of the group = average of the rho_i, weighted by the
*           number of pairs of consecutive years of each country (T_i - 1).
*   Step 3. Prais-Winsten transformation of y and of every regressor
*           (constant and country dummies included), country by country:
*             first year  : z*(i1) = sqrt(1-rho^2) * z(i1)
*             other years : z*(it) = z(it) - rho * z(i,t-1)
*   Step 4. OLS on the transformed data = Prais-Winsten coefficients.
*   Step 5. Panel-corrected standard errors (PCSE):
*           Sigma = covariance matrix of the transformed residuals between
*                   countries, computed on the years common to all the
*                   countries of the group (option casewise, the default);
*           V = inverse(X*'X*) * [sum over years of X*_t' Sigma X*_t] *
*               inverse(X*'X*),
*           where X*_t are the rows of year t. Tests use the normal distribution.

tempfile coefs resids
tempname P
postfile `P' str32 model str6 military byte table str4 block byte spec ///
    str10 method str32 term double(b se z p) int(N years) double(R2 rho) ///
    using `coefs', replace

* Empty file to which the residuals of every model are appended
clear
gen str32 model = ""
save `resids', emptyok

foreach mil in DefGDP DefInd {
    local table = cond("`mil'" == "DefGDP", 2, 3)

    forvalues s = 1/3 {
        foreach blk in FRUK GIS {
            foreach method in FE_AR1 POOLED_AR1 {

                * (a) Sample: countries of the group, complete observations only
                use `fulldata', clear
                keep if block == "`blk'"
                local x `mil' `core' `extra`s'' `crisis'
                egen nmiss = rowmiss(`y' `x')
                keep if nmiss == 0
                * Years must be consecutive within each country (AR(1) lags)
                bysort pid (year): assert year == year[_n-1] + 1 if _n > 1
                quietly summarize year
                local years = r(max) - r(min) + 1

                * (b) Regressors: the FE model adds the country dummies
                local rhs `x'
                if "`method'" == "FE_AR1" local rhs `x' `fe_`blk''

                * (c) Estimation
                local model "`mil'_s`s'_`blk'_`method'"
                display as text _n(2) "=========  `model'  ========="
                xtpcse `y' `rhs', correlation(ar1)
                local rho = real("`e(rho)'")

                * (d) Fitted values, residuals and AR(1) innovations,
                *     all on the original (untransformed) scale:
                *       residual   u(it)   = y(it) - fitted(it)
                *       innovation eps(it) = u(it) - rho * u(i,t-1)
                *     The innovation is missing in the first year of a country.
                predict double fitted, xb
                gen double resid_raw   = `y' - fitted
                gen double resid_innov = resid_raw - `rho' * L.resid_raw

                * (e) Descriptive R-squared on the original scale of y
                gen double sq_resid = resid_raw^2
                quietly summarize sq_resid
                scalar SSE = r(sum)
                quietly summarize `y'
                scalar SST = r(Var) * (r(N) - 1)
                scalar R2  = 1 - SSE / SST

                * (f) Store every coefficient with SE, z and two-sided p-value
                matrix B = e(b)
                matrix V = e(V)
                local terms : colnames B
                local k = 0
                foreach t of local terms {
                    local ++k
                    scalar coef = B[1, `k']
                    scalar stderr = sqrt(V[`k', `k'])
                    post `P' ("`model'") ("`mil'") (`table') ("`blk'") (`s') ///
                        ("`method'") ("`t'") (coef) (stderr) (coef/stderr)  ///
                        (2*normal(-abs(coef/stderr))) (e(N)) (`years')      ///
                        (R2) (`rho')
                }

                * (g) Append the residuals of this model to the residual file
                gen str32 model    = "`model'"
                gen str6  military = "`mil'"
                gen byte  spec     = `s'
                gen str10 method   = "`method'"
                keep model military block spec method country year ///
                     fitted resid_raw resid_innov
                append using `resids'
                save `resids', replace
            }
        }
    }
}
postclose `P'

* Save all estimates
use `coefs', clear
sort table method block spec, stable
export delimited using "$OUT/estimates_long.csv", replace


*------------------------------------------------------------------------------
* 4. FORMATTED TABLES 2 AND 3
*------------------------------------------------------------------------------
* Layout: one column per group and specification (FRUK_1 ... GIS_5);
* coefficient with stars on one row, standard error in parentheses below.
* *** p<0.01, ** p<0.05, * p<0.10.
* The constant is shown only in the pooled tables (see the header).

local rowterms `core' Log_CivGBARD Log_DefGBARD `crisis'

foreach table in 2 3 {
    local mil = cond(`table' == 2, "DefGDP", "DefInd")

    foreach method in FE_AR1 POOLED_AR1 {

        local shown `mil' `rowterms'
        if "`method'" == "POOLED_AR1" local shown `shown' _cons

        * (a) Coefficient rows
        use `coefs', clear
        keep if table == `table' & method == "`method'"
        gen row = .
        gen str40 variable = ""
        local r = 0
        foreach v of local shown {
            local ++r
            replace row = `r' if term == "`v'"
            replace variable = "`lab_`v''" if term == "`v'"
        }
        drop if missing(row)          // intercepts of the FE model not displayed
        gen str3 stars = cond(p < .01, "***", cond(p < .05, "**", ///
                         cond(p < .10, "*", "")))
        gen str20 cell1 = strtrim(string(b, "%9.4f")) + stars
        gen str20 cell2 = "(" + strtrim(string(se, "%9.4f")) + ")"
        keep row variable block spec cell1 cell2
        reshape long cell, i(row block spec) j(line)
        gen order = 2*row - 2 + line   // coefficient, then its SE
        replace variable = "" if line == 2
        keep order variable block spec cell
        tempfile part1
        save `part1'

        * (b) Statistics rows (one value per model)
        use `coefs', clear
        keep if table == `table' & method == "`method'" & term == "_cons"
        gen str20 cell1 = strtrim(string(N, "%9.0f"))
        gen str20 cell2 = strtrim(string(years, "%9.0f"))
        gen str20 cell3 = strtrim(string(R2, "%9.3f"))
        gen str20 cell4 = strtrim(string(rho, "%9.3f"))
        gen str20 cell5 = cond(method == "FE_AR1", "Yes", "No")
        keep block spec cell1-cell5
        reshape long cell, i(block spec) j(stat)
        gen order = 100 + stat
        gen str40 variable = ""
        replace variable = "Observations"               if stat == 1
        replace variable = "Calendar years"             if stat == 2
        replace variable = "R-squared (original scale)" if stat == 3
        replace variable = "rho (common AR(1))"         if stat == 4
        replace variable = "Country fixed effects"      if stat == 5
        keep order variable block spec cell

        * (c) One column per group x specification, then export
        append using `part1'
        gen str8 column = block + "_" + string(spec)
        drop block spec
        reshape wide cell, i(order) j(column) string
        rename cell* *
        sort order
        drop order
        order variable
        export delimited using "$OUT/Table`table'_`method'.csv", replace
    }
}


*------------------------------------------------------------------------------
* 5. RESIDUAL DIAGNOSTICS (by model, residual type and country)
*------------------------------------------------------------------------------
* Two residual types for every model:
*   raw   = u(it), the regression residual; it is serially correlated by
*           construction (that is why the model has AR(1) errors);
*   innov = eps(it), the AR(1) innovation; if the AR(1) is adequate it
*           should be close to white noise. This is the main diagnostic.
* Statistics, computed country by country:
*   N, mean, sd, skewness, kurtosis;
*   Jarque-Bera normality test  JB = N/6 * [S^2 + (K-3)^2/4] ~ chi2(2);
*   Ljung-Box test of no serial correlation up to h = min(8, N/4) lags;
*   variance screen: N*R2 of the regression of squared residuals on the
*           fitted value and its square ~ chi2(2) (constant variance).
* All p-values are approximate (small samples, estimated parameters).

use `resids', clear
order model military block spec method country year fitted resid_raw resid_innov
sort model country year
export delimited using "$OUT/residuals.csv", replace

* Long format: one row per model, residual type, country and year
reshape long resid_, i(model country year) j(type) string
rename resid_ value
drop if missing(value)            // first-year innovations do not exist
egen long id = group(model type country)
tempfile longres
save `longres'

tempfile diag
tempname D
postfile `D' str32 model str6 military str4 block byte spec str10 method  ///
    str5 type str2 country int N double(mean sd skewness kurtosis JB JB_p) ///
    byte lags double(Q Q_p LM LM_p) using `diag', replace

levelsof id, local(ids)
foreach i of local ids {
    use `longres', clear
    keep if id == `i'
    sort year
    tsset year

    * Moments. They are stored as scalars, so that a negative skewness is
    * squared correctly: typing a macro such as -0.5^2 would give -0.25.
    quietly summarize value, detail
    scalar nobs = r(N)
    scalar mu   = r(mean)
    scalar sdev = r(sd)
    scalar skew = r(skewness)
    scalar kurt = r(kurtosis)
    scalar jb   = nobs/6 * (skew^2 + (kurt - 3)^2/4)

    * Ljung-Box portmanteau test
    scalar h = min(8, floor(nobs/4))
    quietly wntestq value, lags(`=h')
    scalar q  = r(stat)
    scalar qp = r(p)

    * Variance screen
    gen double value_sq  = value^2
    gen double fitted_sq = fitted^2
    quietly regress value_sq fitted fitted_sq
    scalar lm  = e(N) * e(r2)
    scalar lmp = chi2tail(e(df_m), lm)

    post `D' (model[1]) (military[1]) (block[1]) (spec[1]) (method[1])  ///
        (type[1]) (country[1]) (nobs) (mu) (sdev) (skew) (kurt) (jb)    ///
        (chi2tail(2, jb)) (h) (q) (qp) (lm) (lmp)
}
postclose `D'
use `diag', clear
sort military spec block method type country
export delimited using "$OUT/diagnostics_residuals.csv", replace


*------------------------------------------------------------------------------
* 6. CORRELATION OF RESIDUALS BETWEEN COUNTRIES (common years)
*------------------------------------------------------------------------------
* Common shocks make the errors of different countries correlated in the same
* year. The PCSE take this correlation into account; these figures show how
* strong it is.

tempfile cross
tempname C
postfile `C' str32 model str5 type str2 country1 str2 country2 ///
    int N double correlation using `cross', replace

use `longres', clear
egen long pair = group(model type)
tempfile pairs
save `pairs'
levelsof pair, local(pids)
foreach i of local pids {
    use `pairs', clear
    keep if pair == `i'
    local m   = model[1]
    local typ = type[1]
    local blk = block[1]
    keep year country value
    reshape wide value, i(year) j(country) string
    if "`blk'" == "FRUK" local couples "FR-UK"
    if "`blk'" == "GIS"  local couples "DE-ES DE-IT ES-IT"
    foreach cp of local couples {
        local c1 = substr("`cp'", 1, 2)
        local c2 = substr("`cp'", 4, 2)
        quietly correlate value`c1' value`c2'
        post `C' ("`m'") ("`typ'") ("`c1'") ("`c2'") (r(N)) (r(rho))
    }
}
postclose `C'
use `cross', clear
sort model type country1 country2
export delimited using "$OUT/diagnostics_crosscountry.csv", replace


*------------------------------------------------------------------------------
* 7. TABLE 1: AVERAGE ANNUAL GROWTH RATES (Section 4.1 of the paper)
*------------------------------------------------------------------------------
* Same data as the regressions:
*   - productivity = exp(Log_MProductiv), the dependent variable in levels;
*   - exports in levels (millions of constant 2015 USD) = Exportman_GDP x GDP / 100,
*     i.e. the regression variable times GDP;
*   - value added (GVA_man) and employment (Emp_man).
* The growth rate of year t is x(t) / x(t-1) - 1. Table 1 reports, in percent,
* the average of the annual growth rates over 1981-2024, or over 1991-2024 for
* Germany, whose employment data start in 1991. The rows GIS and FRUK are
* unweighted averages of the countries of each group.

use `fulldata', clear
gen double GVA          = GVA_man
gen double Employment   = Emp_man
gen double Productivity = exp(Log_MProductiv)
gen double Export       = Exportman_GDP * GDP / 100
keep if !missing(GVA, Employment, Productivity, Export)    // Germany starts in 1991
sort pid year
by pid: assert year == year[_n-1] + 1 if _n > 1             // consecutive years only
foreach v in GVA Employment Productivity Export {
    by pid: gen double g_`v' = 100 * (`v' / `v'[_n-1] - 1) if _n > 1
}
collapse (mean) GVA=g_GVA Employment=g_Employment Productivity=g_Productivity ///
    Export=g_Export (min) first=year (max) last=year, by(country block)

gen str14 Country = ""
replace Country = "Germany"        if country == "DE"
replace Country = "Spain"          if country == "ES"
replace Country = "Italy"          if country == "IT"
replace Country = "France"         if country == "FR"
replace Country = "United Kingdom" if country == "UK"
gen str9 Years = string(first) + "-" + string(last)

* Group averages (unweighted)
preserve
collapse (mean) GVA Employment Productivity Export, by(block)
gen str14 Country = block
gen str9 Years = ""
tempfile groups
save `groups'
restore
append using `groups'

* Order of the rows: Germany, Spain, Italy, GIS, France, United Kingdom, FRUK
gen byte row = 1*(Country == "Germany") + 2*(Country == "Spain") + 3*(Country == "Italy") ///
    + 4*(Country == "GIS") + 5*(Country == "France") + 6*(Country == "United Kingdom") ///
    + 7*(Country == "FRUK")
sort row
keep Country Years GVA Employment Productivity Export
order Country Years GVA Employment Productivity Export
foreach v in GVA Employment Productivity Export {
    replace `v' = round(`v', 0.01)
    format `v' %9.2f
}
list, noobs clean
export delimited using "$OUT/Table1_growth_rates.csv", datafmt replace


display as result _n "Done. Output saved in $OUT"
log close
