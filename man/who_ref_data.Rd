\name{who_ref_data}
\alias{who_ref_data}
\docType{data}
\title{WHO Reference Data}
\description{
LMS parameters (L, M, S) in the WHO charts for age- and sex-standardized growth metrics.
}

\format{
A data.table with columns for sex (sexn coded as 1 (boys) / 2 (girls)), age in days,
and LMS values. Variable names represent the weight (wei), length or height (len),
BMI (bmi), weight-for-length (wfl), or head circumference (headc).  This prefix is
followed by '_' and then by 'l', 'm', or 's' representing the LMS parameters.

Sexn is coded as '1' (boys) and '2' (girls).

In contrast to the sex- and age-standardized metrics, weight-for-length is weight standardized
for sex and length; it represents the distribution of weights for a specific length.

The 'denom' variable indicates if the row represents age-standardization or len-standardization.
}

\usage{
data(who_ref_data)
}

\source{
\url{https://www.who.int/toolkits/child-growth-standards/standards}
}
\keyword{datasets}
