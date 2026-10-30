# ---- shared test grid --------------------------------------------------------
ages <- cdc_test_ages()
grid <- expand.grid(sex = 1:2, agemos = ages)
ref  <- cdc_rows(grid$sex, grid$agemos)
n    <- nrow(grid)

# ---- z-score arithmetic ------------------------------------------------------

test_that("measurements at the median give z = 0 and the 50th percentile", {
  d <- data.frame(grid, wt = ref$wm, ht = ref$hm, bmi = ref$bm)
  out <- cdc_z(d)
  for (v in c("waz", "haz", "bmiz", "orig_bmiz", "mod_waz", "mod_haz",
              "mod_bmiz", "perc_median")) {
    expect_equal(out[[v]], rep(0, n), tolerance = 1e-10, label = v)
  }
  for (v in c("wap", "hap", "bmip")) {
    expect_equal(out[[v]], rep(50, n), tolerance = 1e-10, label = v)
  }
})

test_that("LMS round trip: a measurement at z returns z", {
  for (z in c(-2, -1, 0.5, 1.5)) {   # BMI stays below the 95th percentile
    d <- data.frame(grid,
                    wt  = lms_x(z, ref$wl, ref$wm, ref$ws),
                    ht  = lms_x(z, ref$hl, ref$hm, ref$hs),
                    bmi = bc_x(z,  ref$bl, ref$bm, ref$bs))
    out <- cdc_z(d)
    expect_equal(out$waz,  rep(z, n), tolerance = 1e-8)
    expect_equal(out$haz,  rep(z, n), tolerance = 1e-8)
    expect_equal(out$bmiz, rep(z, n), tolerance = 1e-8)
    expect_equal(out$wap,  rep(100 * pnorm(z), n), tolerance = 1e-8)
    expect_equal(out$bmiz, out$orig_bmiz)   # identical below the 95th percentile
  }
})

test_that("weight and height use the log form when |L| < 0.01 (CDC SAS rule)", {
  r <- ref_table("cdc_ref_data")
  r <- r[r$age > 24 & r$age %% 1 != 0, ]
  near0 <- r[abs(r$wl) < 0.01 | abs(r$hl) < 0.01, ]
  skip_if(nrow(near0) == 0, "no reference rows with |L| < 0.01")
  d <- data.frame(sex = near0$sexn, agemos = near0$age,
                  wt = near0$wm * exp(near0$ws * 1.3),
                  ht = near0$hm * exp(near0$hs * 1.3))
  out <- cdc_z(d)
  if (any(abs(near0$wl) < 0.01))
    expect_equal(out$waz[abs(near0$wl) < 0.01], rep(1.3, sum(abs(near0$wl) < 0.01)), tolerance = 1e-10)
  if (any(abs(near0$hl) < 0.01))
    expect_equal(out$haz[abs(near0$hl) < 0.01], rep(1.3, sum(abs(near0$hl) < 0.01)), tolerance = 1e-10)
})

test_that("modified z-scores are +/-2 at the LMS +/-2 SD values", {
  for (z in c(-2, 2)) {
    d <- data.frame(grid,
                    wt  = bc_x(z, ref$wl, ref$wm, ref$ws),
                    ht  = bc_x(z, ref$hl, ref$hm, ref$hs),
                    bmi = bc_x(z, ref$bl, ref$bm, ref$bs))
    out <- cdc_z(d)
    expect_equal(out$mod_waz,  rep(z, n), tolerance = 1e-10)
    expect_equal(out$mod_haz,  rep(z, n), tolerance = 1e-10)
    expect_equal(out$mod_bmiz, rep(z, n), tolerance = 1e-10)
  }
})

# ---- extended BMI (Wei et al. 2020) -----------------------------------------

p95 <- bc_x(qnorm(0.95), ref$bl, ref$bm, ref$bs)

test_that("at the 95th percentile, bmiz = qnorm(0.95) and bmip95 = 100", {
  out <- cdc_z(data.frame(grid, bmi = p95))
  expect_equal(out$bmiz,   rep(qnorm(0.95), n), tolerance = 1e-8)
  expect_equal(out$bmip,   rep(95, n),          tolerance = 1e-8)
  expect_equal(out$bmip95, rep(100, n),         tolerance = 1e-10)
})

test_that("above the 95th percentile, extended BMI metrics follow Wei et al.", {
  bmi <- p95 * 1.3
  out <- cdc_z(data.frame(grid, bmi = bmi))
  sigma <- wei_sigma(grid$sex, grid$agemos)
  ebp <- 90 + 10 * pnorm((bmi - p95) / sigma)
  expect_equal(out$bmip,      ebp,                 tolerance = 1e-10)
  expect_equal(out$bmiz,      qnorm(ebp / 100),    tolerance = 1e-10)
  expect_equal(out$orig_bmiz, ((bmi / ref$bm)^ref$bl - 1) / (ref$bl * ref$bs), tolerance = 1e-10)
  expect_equal(out$bmip95,    rep(130, n),         tolerance = 1e-10)
  expect_equal(out$perc_median, 100 * (bmi / ref$bm - 1), tolerance = 1e-10)
})

test_that("the extended method starts at the 95th percentile, not higher", {
  bmi <- bc_x(qnorm(0.96), ref$bl, ref$bm, ref$bs)   # between the 95th and 97th
  out <- cdc_z(data.frame(grid, bmi = bmi))
  ebp <- 90 + 10 * pnorm((bmi - p95) / wei_sigma(grid$sex, grid$agemos))
  expect_equal(out$bmip,     ebp,            tolerance = 1e-10)
  expect_equal(out$orig_bmip, rep(96, n),    tolerance = 1e-8)
})

test_that("extended bmiz increases with BMI and is capped at 8.21", {
  k <- seq(1, 3, by = 0.05)
  d <- data.frame(sex = 1, agemos = ages[2], bmi = p95[3] * k)
  out <- cdc_z(d)
  expect_true(all(diff(out$bmiz) >= 0))
  huge <- cdc_z(data.frame(sex = 1:2, agemos = ages[2], bmi = 1000))
  expect_equal(huge$bmiz, c(8.21, 8.21))
  expect_equal(huge$bmip, c(100, 100))
})

# ---- age handling ------------------------------------------------------------

test_that("LMS values are interpolated linearly between reference ages", {
  a1  <- ages[2]
  r12 <- cdc_rows(c(1, 1), c(a1, a1 + 1))
  mid <- a1 + 0.5                              # an integer age ...
  d <- data.frame(sex = 1, agemos = c(mid, a1),  # ... plus a non-integer anchor, so no +0.5 shift
                  wt  = c(mean(r12$wm), r12$wm[1]),
                  bmi = c(mean(r12$bm), r12$bm[1]))
  out <- cdc_z(d, all_vars = TRUE)
  expect_equal(out$waz[1],   0, tolerance = 1e-10)
  expect_equal(out$bmi_l[1], mean(r12$bl), tolerance = 1e-12)
  expect_equal(out$bmi_m[1], mean(r12$bm), tolerance = 1e-12)
  expect_equal(out$bmi_s[1], mean(r12$bs), tolerance = 1e-12)
})

test_that("all-integer ages get 0.5 added (with a warning); input ages unchanged", {
  int  <- data.frame(sex = 1:2, agemos = c(60, 100), wt = c(18, 30))
  half <- transform(int, agemos = agemos + 0.5)
  expect_warning(out_int <- cdc_z(int), "integers")
  out_half <- cdc_z(half)
  expect_equal(out_int$waz, out_half$waz)
  expect_equal(out_int$agemos, c(60, 100))
})

test_that("ages from 24.0 to 24.5 months get values (documented lower limit)", {
  # The table has rows at 23.5 and 24.5 but none at 24.0, so these ages
  # need the 23.5 row for interpolation.
  r <- cdc_rows(c(1, 1), c(23.5, 24.5))
  a <- c(24.0, 24.2, 24.49)
  w <- (a - 23.5)                       # weight on the 24.5 row
  d <- data.frame(sex = 1, agemos = a, wt = r$wm[1] * (1 - w) + r$wm[2] * w)
  out <- cdc_z(d)
  expect_false(anyNA(out$waz))
  expect_equal(out$waz, rep(0, 3), tolerance = 1e-10)
  expect_true(is.na(cdc_z(data.frame(sex = 1, agemos = c(23.99, 30.5), wt = 13))$waz[1]))
})

test_that("ages outside 24 to < 240 months give NA but keep the row", {
  d <- data.frame(sex = 1, agemos = c(12.5, 23.9, 241.5, NA, ages[1]), wt = 15)
  out <- cdc_z(d)
  expect_equal(nrow(out), 5L)
  expect_true(all(is.na(out$waz[1:4])))
  expect_false(is.na(out$waz[5]))
})

# ---- sex coding --------------------------------------------------------------

test_that("equivalent sex codings give identical results", {
  base <- cdc_z(data.frame(sex = c(1, 2), agemos = ages[2], wt = 20))$waz
  codes <- list(c("M", "F"), c("male", "female"), c("b", "g"), c("BOYS", "GIRLS"))
  for (s in codes) {
    expect_equal(cdc_z(data.frame(sex = s, agemos = ages[2], wt = 20))$waz, base,
                 label = paste(s, collapse = "/"))
  }
  expect_equal(cdc_z(data.frame(sex = c(0, 1), agemos = ages[2], wt = 20),
                     sex_code = 0:1)$waz, base)
  expect_equal(cdc_z(data.frame(SEX = c(1, 2), agemos = ages[2], wt = 20))$waz, base)
})

test_that("unrecognized sex gives NA for that row only", {
  out <- cdc_z(data.frame(sex = c(1, 9, NA), agemos = ages[2], wt = 20))
  expect_false(is.na(out$waz[1]))
  expect_true(all(is.na(out$waz[2:3])))
})

test_that("problems with the sex column or sex_code are errors", {
  expect_error(cdc_z(data.frame(agemos = ages[2], wt = 20)), "No sex column")
  expect_error(cdc_z(data.frame(sex = 1, SEX = 1, agemos = ages[2], wt = 20)), "Multiple")
  expect_error(cdc_z(data.frame(sex = 9, agemos = ages[2], wt = 20)), "No valid sex")
  expect_error(cdc_z(data.frame(sex = 1, agemos = ages[2], wt = 20), sex_code = 1), "sex_code")
  expect_error(cdc_z(data.frame(sex = 1, agemos = ages[2], wt = 20), sex_code = c(1, 1)), "sex_code")
})

# ---- arguments ---------------------------------------------------------------

d0 <- data.frame(sex = c(1, 2), age = ages[2:3], weight = c(20, 30), height = c(110, 130))

test_that("column names can be quoted, unquoted, or positional", {
  a <- cdc_z(d0, age = age, wt = weight, ht = height)
  b <- cdc_z(d0, age = "age", wt = "weight", ht = "height")
  c <- cdc_z(d0, age, weight, height)
  expect_identical(a, b)
  expect_identical(a, c)
})

test_that("a supplied column that doesn't exist is warned about", {
  expect_warning(cdc_z(d0, age = age, wt = weight, ht = heigth), "not a column")
})

test_that("missing or non-numeric inputs are errors", {
  expect_error(cdc_z(d0[c("sex", "age")], age = age), "At least one")
  expect_error(cdc_z(d0, age = agee, wt = weight), "not found")
  expect_error(cdc_z(transform(d0, weight = as.character(weight)), age, weight), "numeric")
  expect_error(cdc_z(transform(d0, age = as.character(age)), age, weight), "numeric")
  expect_error(cdc_z(list(sex = 1, age = 60.5, wt = 20)), "data.frame")
})

test_that("an all-NA measurement column is accepted", {
  d <- data.frame(sex = 1, age = 35.5, wt = 13, ht = NA, bmi = 20)
  out <- cdc_z(d, age, wt, ht, bmi = bmi)
  expect_true(is.na(out$haz))
  expect_false(is.na(out$waz))
  expect_error(cdc_z(transform(d, ht = "95"), age, wt, ht), "numeric")
})

test_that("BMI is computed from weight and height when not supplied", {
  out <- cdc_z(d0, age, weight, height)
  expect_equal(out$bmi, d0$weight / (d0$height / 100)^2)
  expect_false(anyNA(out$bmiz))
})

test_that("only metrics computable from the inputs are returned", {
  out <- cdc_z(d0, age, wt = weight)
  expect_true("waz" %in% names(out))
  expect_false(any(c("haz", "bmiz", "bmi") %in% names(out)))
})

test_that("zero or negative measurements are treated as missing", {
  d <- data.frame(sex = 1, agemos = ages[2], wt = c(0, -3, 20), ht = c(110, 0, -1))
  out <- cdc_z(d)
  expect_true(all(is.na(out$waz[1:2])))
  expect_true(all(is.na(out$haz[2:3])))
  expect_true(all(is.na(out$bmiz)))
})

# ---- input and output plumbing ----------------------------------------------

test_that("the returned object has the same class as the input", {
  expect_s3_class(cdc_z(d0, age, weight), "data.frame", exact = TRUE)
  expect_s3_class(cdc_z(data.table::as.data.table(d0), age, weight), "data.table")
  skip_if_not_installed("tibble")
  expect_s3_class(cdc_z(tibble::as_tibble(d0), age, weight), "tbl_df")
})

test_that("the caller's data.table is not modified", {
  dt <- data.table::data.table(sex = 1, agemos = ages[2], wt = c(0, 20), waz = 9)
  before <- data.table::copy(dt)
  suppressWarnings(cdc_z(dt))
  expect_identical(names(dt), names(before))
  expect_equal(as.data.frame(dt), as.data.frame(before))
})

test_that("rows keep their input order", {
  set.seed(1)
  d <- data.frame(id = 1:40, sex = sample(1:2, 40, TRUE),
                  agemos = sample(c(ages, 300.5), 40, TRUE), wt = runif(40, 12, 70))
  shuffled <- d[sample(40), ]
  a <- cdc_z(d)
  b <- cdc_z(shuffled)
  expect_equal(b$id, shuffled$id)
  expect_equal(b[order(b$id), "waz"], a$waz)
})

test_that("input columns with output names are renamed, with a warning", {
  d <- data.frame(sex = 1, agemos = ages[2], wt = 20, waz = 99)
  expect_warning(out <- cdc_z(d), "renamed")
  expect_equal(out$waz0, 99)
  expect_false(out$waz == 99)
})

test_that("zero-row input returns zero rows with the usual columns", {
  full <- cdc_z(d0, age, weight, height)
  empty <- cdc_z(d0[0, ], age, weight, height)
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(full))
})

test_that("all_vars = TRUE adds the extra metrics", {
  extra <- c("bmi_l", "bmi_m", "bmi_s", "sigma", "dist_median", "adj_dist_median",
             "adj_perc_median", "log_perc_median", "adj_log_perc_median")
  expect_false(any(extra %in% names(cdc_z(d0, age, weight, height))))
  expect_true(all(extra %in% names(cdc_z(d0, age, weight, height, all_vars = TRUE))))
})

test_that("cdcanthro() warns once and matches cdc_z()", {
  state <- cdcanthro:::.cdcanthro_state
  state$warned <- FALSE
  expect_warning(old <- cdcanthro(d0, age, weight, height), "deprecated")
  expect_identical(old, cdc_z(d0, age, weight, height))
  expect_no_warning(cdcanthro(d0, age, weight, height))
})
