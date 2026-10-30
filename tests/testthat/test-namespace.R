# Other packages and parallel workers call cdcanthro::cdc_z() without
# library(cdcanthro). Every other test attaches the package, so only a fresh
# R session can show whether the functions still find their reference
# tables in data/. This needs the functions to refer to the tables as
# cdcanthro::cdc_ref_data and cdcanthro::who_ref_data.

test_that("functions work when the package is not attached", {
  skip_if_not_installed("callr")
  skip_if_not(testthat::is_checking(),
              "runs under R CMD check, against the installed package")
  res <- callr::r(function() {
    c(cdc = cdcanthro::cdc_z(data.frame(sex = 1, agemos = 60.5, wt = 18))$waz,
      who = cdcanthro::who_z(data.frame(sex = 1, agedays = 300, wt = 8))$who_waz)
  })
  expect_true(all(is.finite(res)))
})
