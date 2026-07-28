# cdcanthro 0.3.0

* Added logarithmic form of the z-score calculation when |L| < 0.01, to
  match the CDC SAS implementation for weight and height. BMI_L never approaches 0.
* Fixed documentation formatting for DOIs in references.
* If input data has ages outside the reference range [24,240 months), the function
  warns about this and gives the N. These rows are kept in the output