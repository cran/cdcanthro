# Add to test-who_z.R (kept separate here so it can be pasted in).
test_that("negative agedays warn and give NA; the row is kept", {
  d <- data.frame(sex = 1, agedays = c(-5, 300), wt = c(8, 8))
  expect_warning(out <- who_z(d), "negative 'agedays'")
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$who_waz[1]) && is.finite(out$who_waz[2]))
})
