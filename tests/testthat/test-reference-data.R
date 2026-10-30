# Structure of the reference tables. The functions rely on these facts:
# cdc_z() and who_z() keep rows aligned only if each (sex, age) or
# (sex, length) key appears at most once, and the age/length limits in the
# documentation come from the table grids.

test_that("cdc_ref_data has one row per sex and age on the expected grid", {
  r <- ref_table("cdc_ref_data")
  expected_ages <- c(23.5, seq(24.5, 239.5, by = 1), 240)
  for (s in 1:2) {
    expect_equal(sort(r$age[r$sexn == s]), expected_ages, label = paste("CDC ages, sex", s))
  }
  expect_equal(nrow(r), 2L * length(expected_ages))
  expect_false(anyDuplicated(r[c("sexn", "age")]) > 0)
  lms <- c("wl", "wm", "ws", "hl", "hm", "hs", "bl", "bm", "bs", "mref", "sref")
  expect_false(anyNA(r[lms]))
  expect_true(all(r[c("wm", "ws", "hm", "hs", "bm", "bs")] > 0))
})

test_that("who_ref_data has one row per sex and age / length on the expected grids", {
  r <- ref_table("who_ref_data")
  fa <- r[r$denom == "forage", ]
  fl <- r[r$denom == "forlen", ]
  for (s in 1:2) {
    expect_equal(sort(fa$agedays[fa$sexn == s]), 0:1856, label = paste("WHO agedays, sex", s))
    expect_equal(sort(fl$len[fl$sexn == s]), round(seq(45, 110, by = 0.01), 2),
                 label = paste("WHO lengths, sex", s))
  }
  expect_false(anyDuplicated(fa[c("sexn", "agedays")]) > 0)
  expect_false(anyDuplicated(fl[c("sexn", "len")]) > 0)
  expect_false(anyNA(fa[c("wei_l", "wei_m", "wei_s", "len_l", "len_m", "len_s",
                          "bmi_l", "bmi_m", "bmi_s", "headc_l", "headc_m", "headc_s")]))
  expect_false(anyNA(fl[c("wfl_l", "wfl_m", "wfl_s")]))
  expect_true(all(fa$len_l == 1) && all(fa$headc_l == 1))
  expect_false(any(fa$wei_l == 0 | fa$bmi_l == 0))   # Box-Cox would give 0/0
})
