# gradual_z() without the message about absent measures (tested separately)
gz <- function(...) suppressMessages(gradual_z(...))

g0 <- data.frame(
  agedays  = c(0, 365, 730, 731, 1000, 1278, 1825, 1826, 3000),
  who_waz  = c(0.1, 0.2, 0.9, 0.9, 0.7, 0.6, 0.5, 0.4, NA),
  waz      = c(NA, NA, 0.7, 0.7, 0.5, 0.3, 0.3, 0.3, 1.2),
  who_lhaz = 0.2, haz = -0.4,
  who_bmiz = 1.0, bmiz = 0.1
)

test_that("WHO through 730 days, CDC from 1826 days, linear blend between", {
  out <- gradual_z(g0)
  who <- g0$agedays <= 730
  cdc <- g0$agedays >= 1826
  mid <- !who & !cdc
  y <- g0$agedays[mid] / 365.25
  expect_equal(out$grad_waz[who], g0$who_waz[who])
  expect_equal(out$grad_waz[cdc], g0$waz[cdc])
  expect_equal(out$grad_waz[mid],
               (g0$who_waz[mid] * (5 - y) + g0$waz[mid] * (y - 2)) / 3, tolerance = 1e-12)
  expect_equal(out$grad_haz[mid], (0.2 * (5 - y) - 0.4 * (y - 2)) / 3, tolerance = 1e-12)
})

test_that("the blend is continuous at both ends", {
  d <- data.frame(agedays = c(730, 731, 1825, 1826), who_waz = 1, waz = -1)
  out <- gz(d)
  # One day moves the blend by (WHO - CDC) / (3 * 365.25), about 0.0018 here
  expect_lt(abs(out$grad_waz[2] - out$grad_waz[1]), 0.005)
  expect_lt(abs(out$grad_waz[4] - out$grad_waz[3]), 0.005)
})

test_that("missing age gives NA; negative age warns", {
  out <- gz(data.frame(agedays = c(NA, 1000), who_waz = 1, waz = 0))
  expect_true(is.na(out$grad_waz[1]))
  expect_warning(gz(data.frame(agedays = -1, who_waz = 1, waz = 0)), "negative")
})

test_that("incomplete pairs warn, absent pairs message, none at all warns", {
  expect_warning(out <- gz(data.frame(agedays = 900, who_waz = 1, waz = 0, haz = 1)),
                 "Skipping haz")
  expect_true("grad_waz" %in% names(out))
  expect_false("grad_haz" %in% names(out))
  msgs <- capture_messages(gradual_z(data.frame(agedays = 900, who_waz = 1, waz = 0)))
  expect_match(msgs, "Skipping haz", all = FALSE)
  expect_match(msgs, "Skipping bmiz", all = FALSE)
  expect_warning(gz(data.frame(agedays = 900, x = 1)), "No complete")
})

test_that("bad inputs are errors", {
  expect_error(gz(data.frame(agedays = 900, who_waz = 1), whowtz = who_waz, cdcwtz = who_waz),
               "both the WHO and CDC")
  expect_error(gz(data.frame(age = 900, who_waz = 1, waz = 0)), "agedays")
  expect_error(gz(data.frame(agedays = "900", who_waz = 1, waz = 0)), "numeric")
  expect_error(gz(data.frame(agedays = 900, who_waz = "1", waz = 0)), "numeric")
  expect_error(gz(list(agedays = 900)), "data.frame")
})

test_that("column names can be quoted or unquoted", {
  d <- data.frame(days = 1000, w = 1, c = 0)
  expect_identical(
    gz(d, agedays = days, whowtz = w, cdcwtz = c),
    gz(d, agedays = "days", whowtz = "w", cdcwtz = "c"))
})

test_that("columns are grouped WHO, CDC, gradual; other columns first", {
  d <- data.frame(id = 1, bmiz = 0.1, waz = 0.5, agedays = 1000, who_waz = 1,
                  who_bmiz = 1, haz = 0, who_lhaz = 0)
  out <- gradual_z(d)
  expect_identical(names(out),
    c("id", "agedays", "who_waz", "waz", "grad_waz", "who_lhaz", "haz", "grad_haz",
      "who_bmiz", "bmiz", "grad_bmiz"))
})

test_that("an existing output column is overwritten with a warning", {
  d <- data.frame(agedays = 1000, who_waz = 1, waz = 0, grad_waz = 99)
  expect_warning(out <- gz(d), "Overwriting")
  expect_false(out$grad_waz == 99)
})

test_that("the returned object has the same class as the input", {
  expect_s3_class(gz(g0), "data.frame", exact = TRUE)
  expect_s3_class(gz(data.table::as.data.table(g0)), "data.table")
  skip_if_not_installed("tibble")
  expect_s3_class(gz(tibble::as_tibble(g0)), "tbl_df")
})

test_that("who_z() -> cdc_z() -> gradual_z(data) works with default names", {
  d <- data.frame(sex = rep(1:2, 4),
                  agedays = rep(c(400, 1000, 1500, 2500), each = 2),
                  wt = rep(c(10, 14, 17, 25), each = 2),
                  lenhei = rep(c(76, 91, 103, 125), each = 2))
  d$agemos <- d$agedays / 30.4375
  d$ht <- d$lenhei
  d <- who_z(d)
  d <- cdc_z(d)
  out <- gradual_z(d)
  expect_true(all(c("grad_waz", "grad_haz", "grad_bmiz") %in% names(out)))
  early <- out$agedays <= 730
  late  <- out$agedays >= 1826
  expect_equal(out$grad_waz[early], out$who_waz[early])
  expect_equal(out$grad_waz[late],  out$waz[late])
  expect_false(anyNA(out$grad_waz))
})
