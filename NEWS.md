# cdcanthro 0.6.0 (2026-11-16)

* Unrecognized sex codes now produce a warning that reports how many rows were
  affected and which values were seen; those rows get NA as before. Text codes
  are matched as whole words (m, male, b, boy, f, female, g, girl, and their
  plurals, in any case) rather than by first letter, so a value such as "Masc"
  is now NA with a warning instead of being read as male.
* who_z() warns when agedays is negative; those rows get NA as before.
* cdc_z(): the log form of the z-score used when |L| < 0.01 (CDC SAS
  convention) now applies to height only. L for weight is never within 0.19 of
  zero in the CDC tables, so no weight z-score changes.
* The reference-table lookup in cdc_z() and who_z() is now done by index
  rather than a join, which uses less memory on large data. No computed value
  changes; the regression and SAS-comparison tests are unchanged.
* Documentation: the help pages for cdc_z(), who_z() and gradual_z() have been
  rewritten with new examples; all_vars outputs, the WHO adjustment beyond
  +/-3 SD, and the modified z-scores are now described; NHanes and
  who_ref_data document every column; a README was added. The Hales et al.
  (2022) NCHS report on extended BMI metrics is cited alongside Wei et al.
* The package source is now on GitHub (https://github.com/CDC-DNPAO/cdcanthro);
  please report problems at https://github.com/CDC-DNPAO/cdcanthro/issues.
* The SAS validation fixture was reduced from 100,000 to 10,000 children,
  cutting the package size from 8.3 MB to 1.2 MB.

# cdcanthro 0.5.0 (2026-10-30)

* Zero and negative measurements now become NA rather than producing Inf or nonsense values.
* Fewer warnings and messages
* Stricter input checks, such as when one column, cdc_z(data, age, ht), is passed for 
  two measures
* A test suite now exists

# cdcanthro 0.4.0 (2026-08-30)

* Add two new functions to the package, who_z and gradual_z. 
* who_z calculates z-scores based on the WHO growth standards for 
  children up to 1826 days (5 years). The CDC recommendation is to use the WHO charts 
  for children under age 2.0 years, and then to use the CDC charts for older children. 
  This can result in a discontinuity at age 2.0 years, when the reference standard 
  changes from WHO to CDC.  
* gradual_z smooths z-scores between ages 2.0 and 5.0 years using a 
  weighted average of the WHO and CDC z-scores, reducing the discontinuity at age 2.0 years.
  Daymont C, et al. Creation and Evaluation of New Growth Charts With a Gradual Transition From 
  WHO to CDC Values. Pediatrics 2025;156(3): e2025070697.
* Modified all functions so that arguments can be quoted or unquoted
* Added a sex_code argument to cdc_z and who_z that may be useful for people using 0/1 for
  coding sex
* Also note that the cdcanthro function will be phased out in the future, and calling   
  cdcanthro() now gives a warning. The new name is cdc_z.

# cdcanthro 0.3.0

* Added logarithmic form of the z-score calculation when |L| < 0.01, to
  match the CDC SAS implementation for weight and height. BMI_L never approaches 0.
* Fixed documentation formatting for DOIs in references.
* If input data has ages outside the reference range [24,240 months), the function
  warns about this and gives the N. These rows are kept in the output.
