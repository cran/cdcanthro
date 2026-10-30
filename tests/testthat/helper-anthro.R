# Helpers shared by the test files (testthat sources helper-*.R first).
#
# Most expected values are computed from the package's reference tables
# (L, M, S round trips): a measurement built to sit at z must come back as z.
# test-regression.R pins actual outputs for fixed children, and
# test-sas-validation.R compares with SAS output when a fixture is present.
#
# The tables live in data/ (lazy data). Reference grids, checked in
# test-reference-data.R:
#   cdc_ref_data: per sex, ages 23.5, 24.5, 25.5, ..., 239.5, 240.0 months
#   who_ref_data: per sex, 'forage' agedays 0:1856; 'forlen' 45-110 cm by 0.01

# Reference table from data/ (same as cdcanthro::cdc_ref_data).
ref_table <- function(name) as.data.frame(getExportedValue("cdcanthro", name))

# Rows of `tbl` matching each (sexn, key) pair, in the order given;
# an all-NA row where there is no match.
lookup <- function(tbl, sexn, key, key_col) {
  i <- vapply(seq_along(sexn), function(k)
    which(tbl$sexn == sexn[k] & tbl[[key_col]] == key[k])[1], integer(1))
  tbl[i, , drop = FALSE]
}

cdc_rows     <- function(sexn, age)     lookup(ref_table("cdc_ref_data"), sexn, age, "age")
who_age_rows <- function(sexn, agedays) {
  r <- ref_table("who_ref_data")
  lookup(r[r$denom == "forage", ], sexn, agedays, "agedays")
}
who_len_rows <- function(sexn, len) {
  r <- ref_table("who_ref_data")
  lookup(r[r$denom == "forlen", ], sexn, len, "len")
}

# Measurement at z-score z (inverse of the LMS transformation).
# bc_x(): plain Box-Cox, as in who_z() and for CDC BMI.
# lms_x(): CDC weight/height convention -- log form when |L| < 0.01.
bc_x  <- function(z, l, m, s) m * (1 + l * s * z)^(1 / l)
lms_x <- function(z, l, m, s) ifelse(abs(l) < 0.01, m * exp(s * z), bc_x(z, l, m, s))

# Non-integer ages (months) present in the CDC table, near the targets.
cdc_test_ages <- function(targets = c(36, 84, 150, 210)) {
  a <- unique(ref_table("cdc_ref_data")$age)
  a <- a[a %% 1 != 0 & a > 24 & a < 240]
  vapply(targets, function(t) a[which.min(abs(a - t))], numeric(1))
}

# Lengths (cm) present in the WHO weight-for-length table, near the targets.
who_test_lengths <- function(targets = c(55, 70, 85)) {
  r <- ref_table("who_ref_data")
  l <- unique(r$len[r$denom == "forlen" & r$len >= 45 & r$len <= 110])
  vapply(targets, function(t) l[which.min(abs(l - t))], numeric(1))
}

# Spec value for the extended-BMI sigma (Wei et al. 2020), as in cdc_z().
wei_sigma <- function(sexn, agemos) {
  y <- agemos / 12
  ifelse(sexn == 1,
         round(0.3728 + 0.5196 * y - 0.0091 * y^2, 8),
         round(0.8334 + 0.3712 * y - 0.0011 * y^2, 8))
}
