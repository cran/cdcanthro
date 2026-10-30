# ---- shared test grid --------------------------------------------------------
days  <- c(45, 200, 500, 700, 900, 1500)        # both sides of 730 days
wgrid <- expand.grid(sex = 1:2, agedays = days)
wref  <- who_age_rows(wgrid$sex, wgrid$agedays)
wn    <- nrow(wgrid)

at_z <- function(z, r = wref) {
  data.frame(wgrid,
             wt     = bc_x(z, r$wei_l,   r$wei_m,   r$wei_s),
             lenhei = bc_x(z, r$len_l,   r$len_m,   r$len_s),
             bmi    = bc_x(z, r$bmi_l,   r$bmi_m,   r$bmi_s),
             headc  = bc_x(z, r$headc_l, r$headc_m, r$headc_s))
}

# ---- z-score arithmetic ------------------------------------------------------

test_that("measurements at the median give z = 0 and no BIV flags", {
  out <- who_z(at_z(0))
  for (v in c("who_waz", "who_lhaz", "who_bmiz", "who_headcz"))
    expect_equal(out[[v]], rep(0, wn), tolerance = 1e-10, label = v)
  for (f in c("fwaz", "flhaz", "fbmiz", "fheadcz"))
    expect_equal(out[[f]], rep(0L, wn), label = f)
})

test_that("LMS round trip within +/-3: a measurement at z returns z", {
  for (z in c(-2.5, -1, 1, 2.5)) {
    out <- who_z(at_z(z))
    for (v in c("who_waz", "who_lhaz", "who_bmiz", "who_headcz"))
      expect_equal(out[[v]], rep(z, wn), tolerance = 1e-8, label = paste(v, z))
  }
})

test_that("round trip holds where L is close to 0 (weight 603-709 days, BMI 91-118)", {
  r <- ref_table("who_ref_data")
  r <- r[r$denom == "forage", ]
  for (pair in list(c("wei_l", "wei_m", "wei_s", "wt",  "who_waz"),
                    c("bmi_l", "bmi_m", "bmi_s", "bmi", "who_bmiz"))) {
    near0 <- r[abs(r[[pair[1]]]) < 0.01, ]
    expect_gt(nrow(near0), 0)
    for (z in c(-2, 2)) {
      d <- data.frame(sex = near0$sexn, agedays = near0$agedays)
      d[[pair[4]]] <- bc_x(z, near0[[pair[1]]], near0[[pair[2]]], near0[[pair[3]]])
      expect_equal(who_z(d)[[pair[5]]], rep(z, nrow(near0)), tolerance = 1e-8,
                   label = paste(pair[5], z))
    }
  }
})

test_that("weight and BMI beyond +/-3 use the WHO SD3 correction", {
  sd <- function(z, l, m, s) bc_x(z, l, m, s)
  r <- wref
  for (k in c(1, 2)) {
    d <- data.frame(wgrid,
      wt  = sd(3, r$wei_l, r$wei_m, r$wei_s) +
              k * (sd(3, r$wei_l, r$wei_m, r$wei_s) - sd(2, r$wei_l, r$wei_m, r$wei_s)),
      bmi = sd(3, r$bmi_l, r$bmi_m, r$bmi_s) +
              k * (sd(3, r$bmi_l, r$bmi_m, r$bmi_s) - sd(2, r$bmi_l, r$bmi_m, r$bmi_s)))
    out <- who_z(d)
    expect_equal(out$who_waz,  rep(3 + k, wn), tolerance = 1e-10)
    expect_equal(out$who_bmiz, rep(3 + k, wn), tolerance = 1e-10)
  }
  d <- data.frame(wgrid,
    wt = sd(-3, r$wei_l, r$wei_m, r$wei_s) -
           (sd(-2, r$wei_l, r$wei_m, r$wei_s) - sd(-3, r$wei_l, r$wei_m, r$wei_s)))
  expect_equal(who_z(d)$who_waz, rep(-4, wn), tolerance = 1e-10)
})

test_that("length/height and head circumference are not SD3-corrected", {
  out <- who_z(at_z(4))
  expect_equal(out$who_lhaz,   rep(4, wn), tolerance = 1e-8)
  expect_equal(out$who_headcz, rep(4, wn), tolerance = 1e-8)
})

test_that("BIV flags follow the WHO plausibility ranges", {
  set.seed(2)
  z <- runif(300, -8, 8)
  g <- wgrid[rep(seq_len(wn), length.out = 300), ]
  r <- who_age_rows(g$sex, g$agedays)
  d <- data.frame(g,
                  wt     = bc_x(z,  r$wei_l,   r$wei_m,   r$wei_s),
                  lenhei = bc_x(z,  r$len_l,   r$len_m,   r$len_s),
                  headc  = bc_x(z,  r$headc_l, r$headc_m, r$headc_s))
  d$wt[!is.finite(d$wt)] <- NA
  out <- who_z(d)
  flag <- function(x, lo, hi) as.integer(x < lo | x > hi)
  expect_equal(out$fwaz,    flag(out$who_waz,    -6, 5))
  expect_equal(out$flhaz,   flag(out$who_lhaz,   -6, 6))
  expect_equal(out$fbmiz,   flag(out$who_bmiz,   -5, 5))
  expect_equal(out$fheadcz, flag(out$who_headcz, -5, 5))
  expect_true(any(out$fwaz == 1, na.rm = TRUE))   # the test reached both sides
  expect_true(any(out$fwaz == 0, na.rm = TRUE))
})

# ---- weight-for-length ------------------------------------------------------

test_that("weight-for-length round trip for ages <= 730 days", {
  lens <- who_test_lengths()
  g <- expand.grid(sex = 1:2, lenhei = lens)
  r <- who_len_rows(g$sex, g$lenhei)
  for (z in c(-2, 1, 2.5)) {
    out <- who_z(data.frame(g, agedays = 300, wt = bc_x(z, r$wfl_l, r$wfl_m, r$wfl_s)))
    expect_equal(out$who_wflz, rep(z, nrow(g)), tolerance = 1e-8)
  }
})

test_that("weight-for-length is interpolated between reference lengths", {
  r <- ref_table("who_ref_data")
  l <- sort(unique(r$len[r$denom == "forlen" & r$sexn == 2 & r$len >= 69 & r$len <= 71]))
  r12 <- who_len_rows(c(2, 2), l[1:2])
  out <- who_z(data.frame(sex = 2, agedays = 300, lenhei = mean(l[1:2]), wt = mean(r12$wfl_m)))
  expect_equal(out$who_wflz, 0, tolerance = 1e-10)
})

test_that("weight-for-length is NA after 730 days or outside 45-110 cm", {
  lens <- who_test_lengths()
  out <- who_z(data.frame(sex = 1, agedays = c(730, 731, 300, 300),
                          lenhei = c(lens[3], lens[3], 44, 111), wt = 10))
  expect_false(is.na(out$who_wflz[1]))
  expect_true(all(is.na(out$who_wflz[2:4])))
  expect_true(all(is.na(out$fwflz[2:4])))
})

# ---- age handling ------------------------------------------------------------

test_that("ages above 1826 days or missing give NA", {
  out <- who_z(data.frame(sex = 1, agedays = c(1826, 1827, NA), wt = 18))
  expect_false(is.na(out$who_waz[1]))
  expect_true(all(is.na(out$who_waz[2:3])))
  expect_true(all(is.na(out$fwaz[2:3])))
})

test_that("non-integer agedays are rounded, with a warning", {
  d <- data.frame(sex = 1, agedays = c(200.2, 500.4), wt = c(7, 11))
  expect_warning(a <- who_z(d), "rounded")
  b <- who_z(transform(d, agedays = round(agedays)))
  expect_equal(a$who_waz, b$who_waz)
})

# ---- arguments and plumbing --------------------------------------------------

w0 <- data.frame(sex = c(1, 2), days = c(200, 900), kg = c(7.5, 13), cm = c(66, 90))

test_that("column names can be quoted or unquoted", {
  expect_identical(who_z(w0, agedays = days, wt = kg, lenhei = cm),
                   who_z(w0, agedays = "days", wt = "kg", lenhei = "cm"))
})

test_that("missing measurements are skipped, typos warned, none at all is an error", {
  out <- who_z(w0, agedays = days, wt = kg)
  expect_true(all(c("who_waz", "fwaz") %in% names(out)))
  expect_false(any(c("who_lhaz", "who_headcz", "who_bmiz", "who_wflz") %in% names(out)))
  expect_warning(who_z(w0, agedays = days, wt = kg, lenhei = cmm), "not a column")
  expect_error(who_z(w0[c("sex", "days")], agedays = days), "No measurement")
})

test_that("an all-NA measurement column is accepted", {
  d <- data.frame(sex = 1, agedays = 300, wt = 8, lenhei = NA, headc = NA)
  out <- who_z(d)
  expect_false(is.na(out$who_waz))
  expect_true(is.na(out$who_lhaz))
  expect_true(is.na(out$who_headcz))
  expect_true(is.na(out$flhaz))
  expect_error(who_z(transform(d, lenhei = "68")), "numeric")
})

test_that("BMI is computed and added only when not supplied", {
  out <- who_z(w0, agedays = days, wt = kg, lenhei = cm)
  expect_equal(out$bmi, w0$kg / (w0$cm / 100)^2)
  out2 <- who_z(transform(w0, bmi = 16), agedays = days, wt = kg, lenhei = cm)
  expect_equal(out2$bmi, c(16, 16))
})

test_that("sex_code handles 0/1 coding", {
  a <- who_z(w0, agedays = days, wt = kg)
  b <- who_z(transform(w0, sex = sex - 1), agedays = days, wt = kg, sex_code = 0:1)
  expect_equal(a$who_waz, b$who_waz)
})

test_that("the returned object has the same class as the input", {
  expect_s3_class(who_z(w0, agedays = days, wt = kg), "data.frame", exact = TRUE)
  expect_s3_class(who_z(data.table::as.data.table(w0), agedays = days, wt = kg), "data.table")
  skip_if_not_installed("tibble")
  expect_s3_class(who_z(tibble::as_tibble(w0), agedays = days, wt = kg), "tbl_df")
})

test_that("the caller's data.table is not modified", {
  dt <- data.table::data.table(sex = 1, agedays = 300, wt = c(0, 8), lenhei = c(-1, 68))
  before <- data.table::copy(dt)
  who_z(dt)
  expect_identical(names(dt), names(before))
  expect_equal(as.data.frame(dt), as.data.frame(before))
})

test_that("rows keep their input order", {
  set.seed(3)
  d <- data.frame(id = 1:40, sex = sample(1:2, 40, TRUE),
                  agedays = sample(c(days, 2000), 40, TRUE), wt = runif(40, 3, 20))
  shuffled <- d[sample(40), ]
  a <- who_z(d)
  b <- who_z(shuffled)
  expect_equal(b$id, shuffled$id)
  expect_equal(b[order(b$id), "who_waz"], a$who_waz)
})

test_that("input columns with output names are renamed, with a warning", {
  expect_warning(out <- who_z(data.frame(sex = 1, agedays = 300, wt = 8, fwaz = 5)), "renamed")
  expect_equal(out$fwaz0, 5)
})

test_that("zero-row input returns zero rows with the usual columns", {
  full  <- who_z(w0, agedays = days, wt = kg, lenhei = cm)
  empty <- who_z(w0[0, ], agedays = days, wt = kg, lenhei = cm)
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(full))
})
