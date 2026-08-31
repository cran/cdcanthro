\docType{package}
\name{cdcanthro-package}
\alias{cdcanthro-package}
\alias{_PACKAGE}
\title{cdcanthro: Sex- and Age-Standardized Metrics from the CDC and WHO Growth Charts}
\description{
Calculation of sex- and age-standardized growth metrics using
    the LMS method (lambda-mu-sigma). The function cdc_z(), previously named cdcanthro(),
    uses the 2000 CDC growth charts for children and adolescents with ages of >= 24.0 to
    < 240 months, to generate sex- and age-standardized z-scores and percentiles for weight, height,
    and body mass index (BMI). The output includes extended BMI z-scores for values
    above the 95th percentile to more accurately characterize children with very high BMIs. 

    Because CDC recommends the WHO Growth Standards for children under 2 years of age, there is also
    a function, who_z(), which computes sex- and age- (or length-)
    standardized metrics for children under five years of age.  
    
    There is also a function, gradual_z(), which smooths the discontinuities at age 2.0 
    resulting, in large part, from the use of different reference data.  For example, a girl 
    weighing 9.9 kg would be at the 10th percentile the day before her second birthday 
    and at the 2nd percentile the following day.
    }
\author{
David S Freedman

Maintainer: David S Freedman <DavidSFreedman@gmail.com>

Other contributors:
\itemize{
  \item Carrie Daymont \email{cdaymont@pennstatehealth.psu.edu} [contributor]}
}