\name{NHanes}
\alias{NHanes}
\docType{data}
\title{Sample Anthropometric Data from NHANES}
\description{
Body measurements of 7493 children and adolescents (ages 0 to 239 months)
examined in the 2015--2016 and 2017--2018 cycles of the National Health and
Nutrition Examination Survey (NHANES). Used in the examples for
\code{\link{cdc_z}} and \code{\link{who_z}}.
}
\usage{
data(NHanes)
}
\format{
A \code{data.table} with 7493 rows and 9 variables:
\describe{
  \item{\code{seqn}}{NHANES respondent sequence number.}
  \item{\code{sex}}{1 = boys, 2 = girls.}
  \item{\code{agemos}}{Age in completed months (an integer), as released by
    NHANES; 36 represents the interval [36, 37) months.}
  \item{\code{wt}}{Weight (kg).}
  \item{\code{length}}{Recumbent length (cm); measured in children under
    about 4 years of age.}
  \item{\code{ht}}{Standing height (cm); measured from 2 years of age.}
  \item{\code{bmi}}{Body mass index (kg/m^2), from standing height.}
  \item{\code{headc}}{Head circumference (cm); measured only in infants
    (here, through 258 days of age).}
  \item{\code{agedays}}{Age in days (used by \code{who_z}).}
}
}
\source{
National Center for Health Statistics, NHANES Body Measures (BMX) and
Demographics (DEMO) files.
\url{https://wwwn.cdc.gov/nchs/nhanes/}
}
\keyword{datasets}
