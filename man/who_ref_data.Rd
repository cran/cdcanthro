\name{who_ref_data}
\alias{who_ref_data}
\docType{data}
\title{WHO Reference Data}
\description{
LMS parameters (L, M, S) in the WHO charts for age- and sex-standardized growth metrics.
}

\format{
A data.table with 16716 rows: columns for sex (\code{sexn}, 1 = boys, 2 = girls),
age in days (\code{agedays}, 0 to 1856), length (\code{len}, 45 to 110 cm), and LMS values.
Variable names represent weight (\code{wei}), length or height (\code{len}),
BMI (\code{bmi}), weight-for-length (\code{wfl}), head circumference (\code{headc}),
arm circumference (\code{armc}), triceps skinfold (\code{tsf}), or subscapular
skinfold (\code{ssf}). This prefix is followed by '_' and then by 'l', 'm', or 's'
representing the LMS parameters. The arm circumference and skinfold parameters
(available from 91 days of age) are not currently used by \code{\link{who_z}}.

In contrast to the sex- and age-standardized metrics, weight-for-length is weight standardized
for sex and length; it represents the distribution of weights for a specific length. Note that
the length/height reference values are given in 0.01 cm increments.

The \code{denom} variable is \code{"forage"} for rows indexed by age
(\code{agedays}) and \code{"forlen"} for weight-for-length rows (indexed by \code{len}).
}

\usage{
data(who_ref_data)
}

\source{
\url{https://www.who.int/toolkits/child-growth-standards/standards}
}
\keyword{datasets}
