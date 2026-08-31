# cdcanthro 0.4.0

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

#  cdcanthro 0.3.0

* Added logarithmic form of the z-score calculation when |L| < 0.01, to
  match the CDC SAS implementation for weight and height. BMI_L never approaches 0.
* Fixed documentation formatting for DOIs in references.
* If input data has ages outside the reference range [24,240 months), the function
  warns about this and gives the N. These rows are kept in the output.
