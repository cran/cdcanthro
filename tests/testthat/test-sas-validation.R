# Agreement with the CDC SAS program.
#
# To use: save SAS output as tests/testthat/fixtures/sas_cdc.csv with the
# inputs (sex, agemos, wt, ht, bmi) and one column per SAS result, named
# sas_<cdc_z output name>, e.g. sas_waz, sas_haz, sas_bmiz, sas_bmip,
# sas_mod_bmiz, sas_bmip95. Only the sas_* columns present are compared.
#
# Ages: cdc_z() adds 0.5 when every age is a whole number. Use ages with
# decimals (or include at least one) so both programs see the same ages.
# Tolerance: set it to match the digits SAS wrote to the CSV.

test_that("cdc_z() agrees with the CDC SAS program", {
  f <- test_path("fixtures", "sas_cdc.csv")
  skip_if_not(file.exists(f), "fixtures/sas_cdc.csv not present")
  tol <- 1e-6

  sas <- utils::read.csv(f)
  out <- cdc_z(sas[c("sex", "agemos", "wt", "ht", "bmi")])
  vars <- sub("^sas_", "", grep("^sas_", names(sas), value = TRUE))
  expect_gt(length(vars), 0)

  for (v in vars) {
    r <- out[[v]]
    s <- sas[[paste0("sas_", v)]]
    expect_false(is.null(r), label = paste(v, "is not a cdc_z() output column"))
    expect_identical(is.na(r), is.na(s), label = paste("NA pattern of", v))
    expect_lt(max(abs(r - s), na.rm = TRUE), tol, label = paste("max |R - SAS| for", v))
  }
})
