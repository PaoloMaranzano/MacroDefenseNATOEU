###############################################################################
#  Military deindustrialisation and economic performance in Europe
#  Stamegna M., Maranzano P., Mombelli S., Pianta M.
#
#  REPLICATION SCRIPT (R): Tables 2 and 3 and residual diagnostics
#  ---------------------------------------------------------------------------
#  File       : SCED_replication.R
#  Companion  : SCED_replication.do  (same steps, same numbers)
#  Data       : data/dataset_paperSCED.dta  (FR, DE, IT, ES, UK; 1981-2024)
#  Software   : R 4.x. Only the package 'haven' (to read the Stata file).
#
#  WHAT IS ESTIMATED
#  Dependent variable: log manufacturing labour productivity (Log_MProductiv).
#
#  One military variable per table:
#     Table 2 -> military expenditure, % of GDP                   (DefGDP)
#     Table 3 -> military value added, % of manufacturing VA      (DefInd)
#
#  Two groups of countries, always estimated separately:
#     FRUK = France and United Kingdom
#     GIS  = Germany, Italy and Spain
#
#  Three specifications (table columns), which differ only in the R&D controls:
#     (1) civilian government R&D      (2) military government R&D
#     (3) civilian + military R&D
#  Every specification also contains manufacturing exports, imports and
#  investment (% of GDP) and two crisis dummies (2009-2010 and 2020-2021).
#
#  Two estimators:
#     FE_AR1      MAIN MODEL : country fixed effects + AR(1) errors
#     POOLED_AR1  ROBUSTNESS : one common intercept  + AR(1) errors
#  Both are Prais-Winsten estimates with panel-corrected standard errors
#  (PCSE; Beck and Katz, 1995). The function pw_pcse() below codes, step by
#  step, what Stata's xtpcse ..., correlation(ar1) does,
#  and reproduces its results exactly:
#     - one AR(1) coefficient (rho) common to the countries of a group;
#     - PCSE allow each country to have its own error variance and the
#       errors of different countries to be correlated in the same year;
#     - no deterministic time trend.
#
#  In the FE tables the constant is not reported: with country dummies it is
#  only the intercept of the reference country (France in FRUK, Germany in GIS).
#  All intercepts are saved in estimates_long.csv.
#
#  OUTPUT (folder output/R)
#     SCED_tables.docx            Tables 2 and 3 (FE and pooled) in one Word file
#     figures/                    diagnostic figures (section 8) and
#                                 coefficient figures (section 9)
#     Table2_FE_AR1.csv, Table2_POOLED_AR1.csv   formatted Table 2
#     Table3_FE_AR1.csv, Table3_POOLED_AR1.csv   formatted Table 3
#     estimates_long.csv          every coefficient with SE, z, p-value, N, rho, R2
#     residuals.csv               fitted values, residuals and AR(1) innovations
#     diagnostics_residuals.csv   residual diagnostics by model and country
#     diagnostics_crosscountry.csv  correlation of residuals between countries
#
#  HOW TO RUN: set the working directory in section 0, then source the file.
###############################################################################


#------------------------------------------------------------------------------
# 0. SET-UP
#------------------------------------------------------------------------------
# >>> Set the path of the replication folder (the one that contains this file)
# setwd("path/to/Replication_package")

library(haven)      # read_dta(): reads the Stata data file
library(ggplot2)    # figures
library(ggpubr)     # ggarrange(): combines several ggplot panels in one figure
library(officer)    # Word document
library(flextable)  # tables in the Word document (loaded last: its font()
                    # must not be masked by the one in ggpubr)

OUT <- file.path("output", "R")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)


#------------------------------------------------------------------------------
# 1. DATA
#------------------------------------------------------------------------------
d <- as.data.frame(read_dta("data/dataset_paperSCED.dta"))

# Panel structure: one row per country and year, sorted by country and year
d <- d[order(d$country, d$year), ]

# Country groups
d$block <- ifelse(d$country %in% c("FR", "UK"), "FRUK", "GIS")

# Country fixed effects, written as explicit dummy variables.
# The first country of each group (alphabetical) is the base category:
# France in FRUK and Germany in GIS. Their intercept is the "Constant".
d$FE_UK <- as.numeric(d$country == "UK")
d$FE_ES <- as.numeric(d$country == "ES")
d$FE_IT <- as.numeric(d$country == "IT")


#------------------------------------------------------------------------------
# 2. MODEL SET-UP
#------------------------------------------------------------------------------
y_var  <- "Log_MProductiv"
core   <- c("Exportman_GDP", "Importman_GDP", "MInvGDP")   # trade and investment
crisis <- c("D0910", "D20")                                # crisis dummies

# Extra controls of the three specifications (table columns 1-3)
extra <- list(
  c("Log_CivGBARD"),
  c("Log_DefGBARD"),
  c("Log_CivGBARD", "Log_DefGBARD")
)

# Country dummies used by the fixed-effects model in each group
fe_dummies <- list(FRUK = "FE_UK", GIS = c("FE_ES", "FE_IT"))

# Row labels of the tables
labels <- c(
  DefGDP                     = "Military expenditure / GDP",
  DefInd                     = "Military VA / manufacturing VA",
  Exportman_GDP              = "Manufacturing exports / GDP",
  Importman_GDP              = "Manufacturing imports / GDP",
  MInvGDP                    = "Manufacturing investment / GDP",
  Log_CivGBARD               = "Civilian government R&D (log)",
  Log_DefGBARD               = "Military government R&D (log)",
  D0910                      = "Crisis indicator: 2009-2010",
  D20                        = "Crisis indicator: 2020-2021",
  `_cons`                    = "Constant"
)


#------------------------------------------------------------------------------
# 3. ESTIMATION
#------------------------------------------------------------------------------
# 3.1 The estimator: Prais-Winsten with common AR(1) errors and PCSE
# Inputs : y (vector), X (matrix of regressors, constant included),
#          country and year of each row (rows sorted by country and year).
# Output : coefficients, standard errors, rho.
pw_pcse <- function(y, X, country, year) {
  rows_by_country <- split(seq_along(y), country)

  # Step 1. OLS of y on X; keep the residuals e
  e <- as.vector(y - X %*% qr.solve(X, y))

  # Step 2. rho of each country: slope of e(t) on e(t-1), without constant.
  #         Common rho = average of the rho_i, weighted by the number of
  #         pairs of consecutive years of each country (T_i - 1).
  rho_i <- sapply(rows_by_country, function(r) {
    e_t <- e[r][-1]                  # e(t),   from the 2nd year
    e_l <- e[r][-length(r)]          # e(t-1), up to the last-but-one year
    sum(e_t * e_l) / sum(e_l^2)
  })
  pairs <- sapply(rows_by_country, length) - 1
  rho   <- sum(pairs * rho_i) / sum(pairs)

  # Step 3. Prais-Winsten transformation, country by country, of y and of
  #         every column of X (constant and country dummies included):
  #           first year  : z*(i1) = sqrt(1 - rho^2) * z(i1)
  #           other years : z*(it) = z(it) - rho * z(i,t-1)
  y_star <- y
  X_star <- X
  for (r in rows_by_country) {
    first <- r[1]
    now   <- r[-1]
    lag   <- r[-length(r)]
    y_star[first]  <- sqrt(1 - rho^2) * y[first]
    y_star[now]    <- y[now] - rho * y[lag]
    X_star[first, ] <- sqrt(1 - rho^2) * X[first, ]
    X_star[now, ]   <- X[now, , drop = FALSE] - rho * X[lag, , drop = FALSE]
  }

  # Step 4. OLS on the transformed data = Prais-Winsten coefficients
  b <- qr.solve(X_star, y_star)
  u_star <- as.vector(y_star - X_star %*% b)      # transformed residuals

  # Step 5. Panel-corrected standard errors (PCSE)
  # (a) Sigma: covariance of the transformed residuals between countries,
  #     computed on the years in which all countries of the group are
  #     observed (as Stata's default, casewise).
  #     Sigma[i, j] = sum over common years of u*(it) u*(jt) / number of years
  countries <- names(rows_by_country)
  all_years <- sort(unique(year))
  U <- matrix(NA, length(all_years), length(countries),
              dimnames = list(all_years, countries))       # years x countries
  U[cbind(match(year, all_years), match(country, countries))] <- u_star
  common <- complete.cases(U)
  Sigma  <- crossprod(U[common, , drop = FALSE]) / sum(common)
  # (b) "Meat" of the sandwich: sum over years of X*_t' Sigma X*_t, where
  #     X*_t are the rows of year t (only the countries observed in t)
  meat <- matrix(0, ncol(X), ncol(X))
  for (t in all_years) {
    r_t  <- which(year == t)
    c_t  <- match(country[r_t], countries)
    X_t  <- X_star[r_t, , drop = FALSE]
    meat <- meat + t(X_t) %*% Sigma[c_t, c_t, drop = FALSE] %*% X_t
  }
  # (c) V = inverse(X*'X*) * meat * inverse(X*'X*)
  bread <- solve(crossprod(X_star))
  V     <- bread %*% meat %*% bread

  list(b = b, se = sqrt(diag(V)), rho = rho)
}


# 3.2 Estimation loop: 2 military variables x 3 specifications x 2 groups x 2 models
coefs  <- list()   # one data frame of coefficients per model
resids <- list()   # one data frame of residuals per model

for (mil in c("DefGDP", "DefInd")) {
  table <- if (mil == "DefGDP") 2 else 3

  for (s in 1:3) {
    for (blk in c("FRUK", "GIS")) {
      for (method in c("FE_AR1", "POOLED_AR1")) {

        # (a) Sample: countries of the group, complete observations only
        x   <- c(mil, core, extra[[s]], crisis)
        dat <- d[d$block == blk, ]
        dat <- dat[complete.cases(dat[, c(y_var, x)]), ]
        # Years must be consecutive within each country (AR(1) lags)
        stopifnot(all(tapply(dat$year, dat$country,
                             function(t) all(diff(t) == 1))))
        years <- max(dat$year) - min(dat$year) + 1

        # (b) Regressors: the FE model adds the country dummies
        rhs <- x
        if (method == "FE_AR1") rhs <- c(x, fe_dummies[[blk]])
        X <- cbind(as.matrix(dat[, rhs]), `_cons` = 1)
        y <- dat[[y_var]]

        # (c) Estimation
        model <- paste(mil, paste0("s", s), blk, method, sep = "_")
        fit   <- pw_pcse(y, X, dat$country, dat$year)

        # (d) Fitted values, residuals and AR(1) innovations,
        #     all on the original (untransformed) scale:
        #       residual   u(it)   = y(it) - fitted(it)
        #       innovation eps(it) = u(it) - rho * u(i,t-1)
        #     The innovation is missing in the first year of a country.
        fitted <- as.vector(X %*% fit$b)
        u      <- y - fitted
        innov  <- rep(NA_real_, length(u))
        for (r in split(seq_along(u), dat$country)) {
          innov[r[-1]] <- u[r[-1]] - fit$rho * u[r[-length(r)]]
        }

        # (e) Descriptive R-squared on the original scale of y
        R2 <- 1 - sum(u^2) / sum((y - mean(y))^2)

        # (f) Store every coefficient with SE, z and two-sided p-value
        z <- fit$b / fit$se
        coefs[[model]] <- data.frame(
          model = model, military = mil, table = table, block = blk,
          spec = s, method = method, term = colnames(X),
          b = fit$b, se = fit$se, z = z, p = 2 * pnorm(-abs(z)),
          N = length(y), years = years, R2 = R2, rho = fit$rho,
          row.names = NULL)

        # (g) Keep the residuals of this model
        resids[[model]] <- data.frame(
          model = model, military = mil, block = blk, spec = s,
          method = method, country = dat$country, year = dat$year,
          fitted = fitted, resid_raw = u, resid_innov = innov)
      }
    }
  }
}

# Save all estimates
coefs <- do.call(rbind, coefs)
coefs <- coefs[order(coefs$table, coefs$method, coefs$block, coefs$spec), ]
write.csv(coefs, file.path(OUT, "estimates_long.csv"), row.names = FALSE)


#------------------------------------------------------------------------------
# 4. FORMATTED TABLES 2 AND 3
#------------------------------------------------------------------------------
# Layout: one column per group and specification (FRUK_1 ... GIS_5);
# coefficient with stars on one row, standard error in parentheses below.
# *** p<0.01, ** p<0.05, * p<0.10.
# The constant is shown only in the pooled tables (see the header).

stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**",
                     ifelse(p < .10, "*", "")))
columns  <- paste(rep(c("FRUK", "GIS"), each = 3), rep(1:3, 2), sep = "_")
rowterms <- c(core, "Log_CivGBARD", "Log_DefGBARD", crisis)

for (table in 2:3) {
  mil <- if (table == 2) "DefGDP" else "DefInd"

  for (method in c("FE_AR1", "POOLED_AR1")) {
    est <- coefs[coefs$table == table & coefs$method == method, ]
    est$column <- paste(est$block, est$spec, sep = "_")
    tab <- NULL
    shown <- c(mil, rowterms)
    if (method == "POOLED_AR1") shown <- c(shown, "_cons")

    # (a) Coefficient rows: coefficient, then its SE (empty if not included)
    for (v in shown) {
      coef_row <- se_row <- setNames(rep("", length(columns)), columns)
      for (cl in columns) {
        r <- est[est$term == v & est$column == cl, ]
        if (nrow(r) == 1) {
          coef_row[cl] <- paste0(sprintf("%.4f", r$b), stars(r$p))
          se_row[cl]   <- sprintf("(%.4f)", r$se)
        }
      }
      tab <- rbind(tab, c(variable = labels[[v]], coef_row),
                        c(variable = "", se_row))
    }

    # (b) Statistics rows (one value per model)
    one <- est[est$term == "_cons", ]
    one <- one[match(columns, one$column), ]
    tab <- rbind(tab,
      c(variable = "Observations",               sprintf("%.0f", one$N)),
      c(variable = "Calendar years",             sprintf("%.0f", one$years)),
      c(variable = "R-squared (original scale)", sprintf("%.3f", one$R2)),
      c(variable = "rho (common AR(1))",         sprintf("%.3f", one$rho)),
      c(variable = "Country fixed effects",
        rep(if (method == "FE_AR1") "Yes" else "No", length(columns))))

    # (c) Export
    colnames(tab) <- c("variable", columns)
    write.csv(tab, file.path(OUT, paste0("Table", table, "_", method, ".csv")),
              row.names = FALSE, quote = FALSE)
  }
}


#------------------------------------------------------------------------------
# 5. RESIDUAL DIAGNOSTICS (by model, residual type and country)
#------------------------------------------------------------------------------
# Two residual types for every model:
#   raw   = u(it), the regression residual; it is serially correlated by
#           construction (that is why the model has AR(1) errors);
#   innov = eps(it), the AR(1) innovation; if the AR(1) is adequate it
#           should be close to white noise. This is the main diagnostic.
# Statistics, computed country by country:
#   N, mean, sd, skewness, kurtosis;
#   Jarque-Bera normality test  JB = N/6 * [S^2 + (K-3)^2/4] ~ chi2(2);
#   Ljung-Box test of no serial correlation up to h = min(8, N/4) lags;
#   variance screen: N*R2 of the regression of squared residuals on the
#           fitted value and its square ~ chi2(2) (constant variance).
# All p-values are approximate (small samples, estimated parameters).

resids <- do.call(rbind, resids)
resids <- resids[order(resids$model, resids$country, resids$year), ]
write.csv(resids, file.path(OUT, "residuals.csv"), row.names = FALSE)

diagnose <- function(u, fitted) {
  n <- length(u)
  # Moments with denominator n (as Stata's summarize, detail)
  m2 <- mean((u - mean(u))^2)
  m3 <- mean((u - mean(u))^3)
  m4 <- mean((u - mean(u))^4)
  skew <- m3 / m2^1.5
  kurt <- m4 / m2^2
  jb   <- n / 6 * (skew^2 + (kurt - 3)^2 / 4)
  # Ljung-Box portmanteau test
  h  <- min(8, floor(n / 4))
  lb <- Box.test(u, lag = h, type = "Ljung-Box")
  # Variance screen
  aux <- lm(I(u^2) ~ fitted + I(fitted^2))
  lm_stat <- n * summary(aux)$r.squared
  data.frame(N = n, mean = mean(u), sd = sd(u), skewness = skew,
             kurtosis = kurt, JB = jb, JB_p = pchisq(jb, 2, lower.tail = FALSE),
             lags = h, Q = unname(lb$statistic), Q_p = lb$p.value,
             LM = lm_stat, LM_p = pchisq(lm_stat, 2, lower.tail = FALSE))
}

diag <- list()
for (m in unique(resids$model)) {
  rm <- resids[resids$model == m, ]
  for (type in c("innov", "raw")) {
    for (cc in sort(unique(rm$country))) {
      rc <- rm[rm$country == cc, ]
      u  <- if (type == "raw") rc$resid_raw else rc$resid_innov
      ok <- !is.na(u)                 # first-year innovations do not exist
      diag[[length(diag) + 1]] <- data.frame(
        model = m, military = rc$military[1], block = rc$block[1],
        spec = rc$spec[1], method = rc$method[1], type = type, country = cc,
        diagnose(u[ok], rc$fitted[ok]))
    }
  }
}
diag <- do.call(rbind, diag)
diag <- diag[order(diag$military, diag$spec, diag$block, diag$method,
                   diag$type, diag$country), ]
write.csv(diag, file.path(OUT, "diagnostics_residuals.csv"), row.names = FALSE)


#------------------------------------------------------------------------------
# 6. CORRELATION OF RESIDUALS BETWEEN COUNTRIES (common years)
#------------------------------------------------------------------------------
# Common shocks make the errors of different countries correlated in the same
# year. The PCSE take this correlation into account; these figures show how
# strong it is.

couples <- list(FRUK = list(c("FR", "UK")),
                GIS  = list(c("DE", "ES"), c("DE", "IT"), c("ES", "IT")))
cross <- list()
for (m in unique(resids$model)) {
  rm <- resids[resids$model == m, ]
  for (type in c("innov", "raw")) {
    col <- if (type == "raw") "resid_raw" else "resid_innov"
    for (cp in couples[[rm$block[1]]]) {
      a  <- rm[rm$country == cp[1], c("year", col)]
      b  <- rm[rm$country == cp[2], c("year", col)]
      ab <- merge(a, b, by = "year")          # common years only
      ab <- ab[complete.cases(ab), ]
      cross[[length(cross) + 1]] <- data.frame(
        model = m, type = type, country1 = cp[1], country2 = cp[2],
        N = nrow(ab), correlation = cor(ab[[2]], ab[[3]]))
    }
  }
}
cross <- do.call(rbind, cross)
cross <- cross[order(cross$model, cross$type, cross$country1, cross$country2), ]
write.csv(cross, file.path(OUT, "diagnostics_crosscountry.csv"), row.names = FALSE)


#------------------------------------------------------------------------------
# 7. PAPER OUTPUT: ALL TABLES IN ONE WORD FILE (output/R/SCED_tables.docx)
#------------------------------------------------------------------------------
# Order: main model (FE) for Tables 2 and 3, then the pooled robustness check.
# Each table is read back from its csv file and written with officer/flextable.

titles <- c(
  "2_FE_AR1"     = "Table 2. Military expenditure and manufacturing labour productivity: country fixed effects with AR(1) errors (main model)",
  "3_FE_AR1"     = "Table 3. Military specialisation and manufacturing labour productivity: country fixed effects with AR(1) errors (main model)",
  "2_POOLED_AR1" = "Table 2 (robustness). Military expenditure and manufacturing labour productivity: pooled model with AR(1) errors",
  "3_POOLED_AR1" = "Table 3 (robustness). Military specialisation and manufacturing labour productivity: pooled model with AR(1) errors")
note_all <- paste(
  "Dependent variable: log manufacturing labour productivity. Prais-Winsten estimates",
  "with AR(1) errors and one autoregressive coefficient (rho) per country group; no",
  "deterministic trend. Panel-corrected standard errors (Beck and Katz, 1995) in",
  "parentheses. *** p<0.01, ** p<0.05, * p<0.10. Columns: (1) civilian R&D; (2) military R&D; (3) civilian and",
  "military R&D. R-squared computed on the original scale of the dependent variable.")
note_method <- c(FE_AR1     = "Country fixed effects included; country intercepts not reported.",
                 POOLED_AR1 = "Common intercept for the countries of each group.")

txt <- function(x, size = 10, bold = FALSE)      # a paragraph in Times New Roman
  fpar(ftext(x, fp_text(font.family = "Times New Roman", font.size = size, bold = bold)))

doc <- read_docx()
doc <- body_add_fpar(doc, txt("SCED: regression tables", size = 14, bold = TRUE))
doc <- body_add_fpar(doc, txt(paste("Generated by SCED_replication.R. Main model: country fixed",
  "effects with AR(1) errors. Robustness: pooled model with AR(1) errors.")))

first <- TRUE
for (method in c("FE_AR1", "POOLED_AR1")) {
  for (table in 2:3) {
    tab <- read.csv(file.path(OUT, paste0("Table", table, "_", method, ".csv")),
                    colClasses = "character")

    ft <- flextable(tab)
    # Two header rows: country group, then specification number
    ft <- set_header_labels(ft, values = setNames(
            as.list(c("", rep(paste0("(", 1:3, ")"), 2))), names(tab)))
    ft <- add_header_row(ft, values = c("", "France and United Kingdom",
                                        "Germany, Italy and Spain"),
                         colwidths = c(1, 3, 3))
    ft <- theme_booktabs(ft)
    ft <- font(ft, fontname = "Times New Roman", part = "all")
    ft <- fontsize(ft, size = 9, part = "all")
    ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")   # compact rows
    ft <- line_spacing(ft, space = 1, part = "all")
    ft <- align(ft, j = 2:7, align = "center", part = "all")
    ft <- hline(ft, i = nrow(tab) - 5, border = fp_border(width = 0.75))  # statistics rows
    ft <- width(ft, j = 1, width = 2.2)
    ft <- width(ft, j = 2:7, width = 1.1)

    if (!first) doc <- body_add_break(doc)
    first <- FALSE
    doc <- body_add_fpar(doc, txt(titles[[paste(table, method, sep = "_")]], bold = TRUE))
    doc <- body_add_flextable(doc, ft)
    doc <- body_add_fpar(doc, txt(paste("Notes:", note_all, note_method[[method]]), size = 8))
  }
}
# A4 landscape, narrow margins
doc <- body_set_default_section(doc, prop_section(
  page_size = page_size(orient = "landscape", width = 8.27, height = 11.69),
  page_margins = page_mar(top = 0.6, bottom = 0.6, left = 0.6, right = 0.6)))
# Save. If the file is open in Word it cannot be overwritten: warn and go on.
tryCatch(print(doc, target = file.path(OUT, "SCED_tables.docx")),
         error = function(e) warning("SCED_tables.docx not updated: close it in ",
                                     "Word and run the script again."))


#------------------------------------------------------------------------------
# 8. PAPER OUTPUT: DIAGNOSTIC FIGURES (ggplot2, folder output/R/figures)
#------------------------------------------------------------------------------
# Five figures summarise the diagnostics of all 24 regressions:
#   Figure_diagnostic_tests.png
#       p-values of the three tests on the AR(1) innovations (Ljung-Box,
#       Jarque-Bera, constant variance), for every table, model,
#       specification and country;
#   Figure_diagnostics_Table2_FE_AR1.png, ..._Table3_FE_AR1.png (main model)
#   Figure_diagnostics_Table2_POOLED_AR1.png, ..._Table3_POOLED_AR1.png
#       the three specifications overlaid, one colour each:
#       A. AR(1) innovations over time, by country;
#       B. autocorrelation function (ACF) of the residual u and of the
#          innovation: spikes outside the grey band (95%, white noise) are
#          significant. u should be persistent; the innovation should not;
#       C. normal Q-Q plot of the standardised innovations: the closer the
#          lines are to the diagonal, the closer the innovations are to normal.

dir.create(file.path(OUT, "figures"), showWarnings = FALSE)

# One colour per specification (fixed order) and country labels
spec_colours <- c("1" = "#2a78d6", "2" = "#eb6834", "3" = "#1baf7a")
spec_labels  <- c("1" = "(1) Civilian R&D", "2" = "(2) Military R&D",
                  "3" = "(3) Civilian + military R&D")
country_names <- c(FR = "France", UK = "United Kingdom",
                   DE = "Germany", ES = "Spain", IT = "Italy")
colour_scale <- scale_colour_manual(values = spec_colours, labels = spec_labels,
                                    name = "Specification")

# White background, black frame around each panel, no grid
theme_paper <- theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10),
        plot.title = element_text(face = "bold", size = 11),
        legend.position = "bottom")

# (a) Residuals in long format: one row per model, type, country and year
id_cols  <- c("model", "military", "method", "spec", "country", "year")
res_long <- rbind(
  data.frame(resids[, id_cols], type = "Residual u", value = resids$resid_raw),
  data.frame(resids[, id_cols], type = "Innovation", value = resids$resid_innov))
res_long <- res_long[!is.na(res_long$value), ]          # rows sorted by year

# (b) ACF at lags 1-8, with the 95% white-noise band 1.96/sqrt(n)
acf_data <- do.call(rbind, lapply(
  split(res_long, list(res_long$model, res_long$country, res_long$type), drop = TRUE),
  function(g) data.frame(g[1, c(id_cols[1:5], "type")], lag = 1:8,
                         acf  = acf(g$value, lag.max = 8, plot = FALSE)$acf[-1],
                         band = qnorm(.975) / sqrt(nrow(g)), row.names = NULL)))

# (c) Normal Q-Q data: sorted standardised innovations against normal quantiles
innov   <- res_long[res_long$type == "Innovation", ]
qq_data <- do.call(rbind, lapply(
  split(innov, list(innov$model, innov$country), drop = TRUE),
  function(g) {
    z <- sort((g$value - mean(g$value)) / sd(g$value))
    data.frame(g[1, id_cols[1:5]], theoretical = qnorm(seq_along(z) / (length(z) + 1)),
               sample = z, row.names = NULL)
  }))

# Common factors: countries in a fixed order, specifications as categories
fix_factors <- function(x) {
  x$country <- factor(country_names[x$country], levels = country_names)
  x$spec    <- factor(x$spec)
  if ("type" %in% names(x)) x$type <- factor(x$type, levels = c("Residual u", "Innovation"))
  x
}
res_long <- fix_factors(res_long)
acf_data <- fix_factors(acf_data)
qq_data  <- fix_factors(qq_data)

# --- Figures A-B-C, one for each table and model ------------------------------
for (mil in c("DefGDP", "DefInd")) {
  for (method in c("FE_AR1", "POOLED_AR1")) {
    table <- if (mil == "DefGDP") 2 else 3
    pick  <- function(x) x[x$military == mil & x$method == method, ]
    title <- paste0("Table ", table, " | ",
                    if (method == "FE_AR1") "Country FE + AR(1)" else "Pooled AR(1)",
                    " | residual diagnostics, specifications (1)-(3)")

    pA <- ggplot(pick(res_long[res_long$type == "Innovation", ]),
                 aes(year, value, colour = spec)) +
      geom_hline(yintercept = 0) +
      geom_line(linewidth = 0.45) +
      facet_wrap(~ country, nrow = 1) +
      scale_x_continuous(breaks = c(1990, 2000, 2010, 2020)) +
      colour_scale + theme_paper +
      labs(title = "AR(1) innovations over time", x = NULL, y = "Innovation")

    acf_t <- pick(acf_data)
    bands <- unique(acf_t[, c("country", "type", "band")])
    pB <- ggplot(acf_t, aes(lag, acf, colour = spec)) +
      geom_rect(data = bands, inherit.aes = FALSE, fill = "grey88",
                aes(xmin = 0.5, xmax = 8.5, ymin = -band, ymax = band)) +
      geom_hline(yintercept = 0) +
      geom_linerange(aes(ymin = 0, ymax = acf), linewidth = 0.6,
                     position = position_dodge(width = 0.75)) +
      geom_point(size = 1.1, position = position_dodge(width = 0.75)) +
      facet_grid(type ~ country) +
      scale_x_continuous(breaks = 1:8) +
      scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1)) +
      coord_cartesian(ylim = c(-1.1, 1.1), xlim = c(0.6, 8.4), expand = FALSE) +
      colour_scale + theme_paper +
      labs(title = "Autocorrelation function (grey band: 95% band under white noise)",
           x = "Lag (years)", y = "Autocorrelation")

    pC <- ggplot(pick(qq_data), aes(theoretical, sample, colour = spec)) +
      geom_abline(slope = 1, intercept = 0) +
      geom_line(linewidth = 0.45) +
      facet_wrap(~ country, nrow = 1) +
      colour_scale + theme_paper +
      labs(title = "Normal Q-Q plot of the standardised innovations",
           x = "Normal quantiles", y = "Innovation quantiles")

    fig <- ggarrange(pA, pB, pC, ncol = 1, heights = c(1, 1.6, 1),
                     labels = c("A", "B", "C"), common.legend = TRUE, legend = "bottom")
    fig <- annotate_figure(fig, top = text_grob(title, face = "bold", size = 13))
    ggsave(file.path(OUT, "figures",
                     paste0("Figure_diagnostics_Table", table, "_", method, ".png")),
           fig, width = 13, height = 11, dpi = 200, bg = "white")
  }
}

# --- Figure of the test p-values ----------------------------------------------
d_in  <- diag[diag$type == "innov", ]
keys  <- d_in[, c("military", "method", "spec", "country")]
tests <- rbind(
  data.frame(keys, test = "Ljung-Box\n(no serial correlation)", p = d_in$Q_p),
  data.frame(keys, test = "Jarque-Bera\n(normality)",           p = d_in$JB_p),
  data.frame(keys, test = "Constant\nvariance",                 p = d_in$LM_p))
tests$test    <- factor(tests$test, levels = unique(tests$test))
tests$panel   <- factor(paste0(ifelse(tests$military == "DefGDP", "Table 2", "Table 3"), " | ",
                               ifelse(tests$method == "FE_AR1", "FE + AR(1)", "Pooled AR(1)")),
                        levels = c("Table 2 | FE + AR(1)", "Table 3 | FE + AR(1)",
                                   "Table 2 | Pooled AR(1)", "Table 3 | Pooled AR(1)"))
tests$country <- factor(country_names[tests$country], levels = rev(country_names))
tests$class   <- cut(tests$p, c(-Inf, .01, .05, .10, Inf), right = FALSE,
                     labels = c("p < 0.01", "0.01 - 0.05", "0.05 - 0.10", "p > 0.10"))

p_tests <- ggplot(tests, aes(factor(spec), country, fill = class)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = ifelse(p < 0.005, "<0.01", sprintf("%.2f", p)),
                colour = class %in% c("p < 0.01", "0.01 - 0.05")), size = 2.6) +
  scale_fill_manual(values = c("p < 0.01" = "#184f95", "0.01 - 0.05" = "#3987e5",
                               "0.05 - 0.10" = "#9ec5f4", "p > 0.10" = "#f0efec"),
                    name = "p-value", drop = FALSE) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  facet_grid(test ~ panel) +
  theme_paper +
  labs(title = "Residual tests on the AR(1) innovations: p-values by country and specification",
       subtitle = "Blue cells reject the null hypothesis (in brackets) at the 10% level or below",
       x = "Specification", y = NULL)
ggsave(file.path(OUT, "figures", "Figure_diagnostic_tests.png"), p_tests,
       width = 12, height = 7.5, dpi = 200, bg = "white")


#------------------------------------------------------------------------------
# 9. PAPER OUTPUT: COEFFICIENT FIGURES (ggplot2, folder output/R/figures)
#------------------------------------------------------------------------------
# Estimates with 95% confidence intervals (estimate +/- 1.96 x PCSE), to compare
# at a glance specifications, country groups and the two models:
#   Figure_coefficients_military.png  the military variable of Tables 2 and 3;
#   Figure_coefficients_all.png       every regressor of Tables 2 and 3.
# Colour = model; filled point = significant at 5% (interval excludes zero);
# hollow point = not significant at 5%.

model_colours <- c(FE_AR1 = "#2a78d6", POOLED_AR1 = "#eb6834")
model_labels  <- c(FE_AR1     = "Country FE + AR(1) (main model)",
                   POOLED_AR1 = "Pooled AR(1) (robustness)")
group_names   <- c(FRUK = "France and United Kingdom", GIS = "Germany, Italy and Spain")

# Data: all coefficients except the intercepts
cf <- coefs[!coefs$term %in% c("_cons", "FE_UK", "FE_ES", "FE_IT"), ]
cf$lo    <- cf$b - qnorm(.975) * cf$se
cf$hi    <- cf$b + qnorm(.975) * cf$se
cf$sig   <- factor(ifelse(cf$p < .05, "Significant at 5%", "Not significant at 5%"),
                   levels = c("Significant at 5%", "Not significant at 5%"))
cf$spec  <- factor(cf$spec, levels = 1:3)
cf$group <- factor(group_names[cf$block], levels = group_names)
cf$table_label <- factor(ifelse(cf$table == 2, "Table 2\nMilitary expenditure / GDP",
                                "Table 3\nMilitary VA / manufacturing VA"))
# One row per regressor; the two military variables share the first row
cf$term_label <- ifelse(cf$term %in% c("DefGDP", "DefInd"), "Military variable",
                        labels[cf$term])
cf$term_label <- factor(cf$term_label, levels = c("Military variable",
  labels[c(core, "Log_CivGBARD", "Log_DefGBARD", crisis)]))

# Common layers: zero line, 95% intervals, points (dodged by model)
dodge <- position_dodge(width = 0.5)
coef_layers <- list(
  geom_hline(yintercept = 0, linetype = "dashed"),
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5, position = dodge),
  geom_point(size = 2, stroke = 0.8, position = dodge),
  scale_colour_manual(values = model_colours, labels = model_labels, name = NULL),
  scale_shape_manual(values = c("Significant at 5%" = 16, "Not significant at 5%" = 1),
                     name = NULL, drop = FALSE),
  scale_x_discrete(drop = FALSE),
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2)),
  theme_paper,
  theme(legend.box = "horizontal"))                   # both legends on one row

# (a) Military variable only: rows = tables, columns = country groups
p_mil <- ggplot(cf[cf$term_label == "Military variable", ],
                aes(spec, b, colour = method, shape = sig, group = method)) +
  coef_layers +
  facet_grid(table_label ~ group, scales = "free_y") +
  labs(title = "Military variables and manufacturing labour productivity",
       subtitle = "Estimates and 95% confidence intervals (panel-corrected standard errors)",
       x = "Specification", y = "Coefficient")
ggsave(file.path(OUT, "figures", "Figure_coefficients_military.png"), p_mil,
       width = 10, height = 7, dpi = 200, bg = "white")

# (b) All regressors: rows = regressors, columns = table x country group
cf$column <- factor(paste0(ifelse(cf$table == 2, "Table 2", "Table 3"), "\n", cf$group),
                    levels = c(paste0("Table 2\n", group_names), paste0("Table 3\n", group_names)))
p_all <- ggplot(cf, aes(spec, b, colour = method, shape = sig, group = method)) +
  coef_layers +
  facet_grid(term_label ~ column, scales = "free_y",
             labeller = labeller(term_label = label_wrap_gen(18))) +
  theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
  labs(title = "All coefficients of Tables 2 and 3",
       subtitle = paste("Estimates and 95% confidence intervals (panel-corrected standard",
                        "errors). Military variable: expenditure / GDP in Table 2,",
                        "military VA / manufacturing VA in Table 3"),
       x = "Specification", y = "Coefficient")
ggsave(file.path(OUT, "figures", "Figure_coefficients_all.png"), p_all,
       width = 12, height = 16, dpi = 200, bg = "white")


cat("Done. Output saved in", OUT, "\n")
