# cdcanthro 0.2.0

## New features

* `wt`, `ht`, and `bmi` arguments are now optional. The function 
  computes whatever metrics can be derived from the supplied 
  variables and returns `NA` for those that cannot.
* Output class now matches input class (`data.frame`, `data.table`, 
  or tibble).
* Added internal caching of processed reference data for faster 
  repeated calls.
* Provides information on missing variables and missing data

## Bug fixes

* Fixed invalid use of `:=` inside `{}` in the extended BMI block.

## Internal changes

* Pre-computed `qnorm()` constants and moved `lms_pct()` to 
  package level.
