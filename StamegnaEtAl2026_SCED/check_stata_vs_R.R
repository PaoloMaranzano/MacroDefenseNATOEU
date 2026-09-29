###############################################################################
#  Military deindustrialisation and economic performance in Europe
#
#  check_stata_vs_R.R: verifies that SCED_replication.do (Stata) and
#  SCED_replication.R (R) give the same results.
#
#  Run it from the replication folder after running both scripts.
#  1. The formatted tables must be identical, character by character.
#  2. Every other output must have the same rows and labels, and numbers
#     equal up to 1e-6 (differences below this are rounding in the csv files).
###############################################################################

dir_stata <- file.path("output", "stata")
dir_R     <- file.path("output", "R")
tolerance <- 1e-6
all_ok    <- TRUE

# 1. Formatted tables -----------------------------------------------------------
cat("Formatted tables\n")
for (f in c("Table2_FE_AR1.csv", "Table3_FE_AR1.csv",
            "Table2_POOLED_AR1.csv", "Table3_POOLED_AR1.csv")) {
  same <- identical(readLines(file.path(dir_stata, f)), readLines(file.path(dir_R, f)))
  all_ok <- all_ok && same
  cat(sprintf("  %-30s %s\n", f, if (same) "identical" else "DIFFERENT"))
}

# 2. Estimates, residuals and diagnostics ----------------------------------------
cat("Numerical results\n")
for (f in c("estimates_long.csv", "residuals.csv",
            "diagnostics_residuals.csv", "diagnostics_crosscountry.csv")) {
  s <- read.csv(file.path(dir_stata, f))
  r <- read.csv(file.path(dir_R, f))
  numbers <- names(s)[sapply(s, is.numeric)]
  labels  <- setdiff(names(s), numbers)

  same_shape  <- identical(dim(s), dim(r)) && identical(names(s), names(r))
  same_labels <- same_shape && all(s[labels] == r[labels])
  same_na     <- same_shape && all(is.na(s[numbers]) == is.na(r[numbers]))
  max_diff    <- if (same_shape) max(abs(as.matrix(s[numbers]) - as.matrix(r[numbers])),
                                     na.rm = TRUE) else NA
  ok <- same_labels && same_na && max_diff < tolerance
  all_ok <- all_ok && ok
  cat(sprintf("  %-30s rows = %4d   max |Stata - R| = %.1e   %s\n",
              f, nrow(s), max_diff, if (ok) "OK" else "CHECK"))
}

cat(if (all_ok) "\nALL CHECKS PASSED: Stata and R give the same results.\n"
    else "\nSOME CHECKS FAILED: see the lines above.\n")
