\name{cdc_ref_data}
\alias{cdc_ref_data}
\docType{data}
\title{CDC Growth Chart Reference Data (2000)}
\description{
LMS parameters (L, M, S) for age- and sex-standardized growth metrics.
}

\format{
A data.table with columns for sex, age, and LMS values. Parameter names represent 
the first letter of weight, height, or BMI; this is follow by 'l', 'm', or 's' for the
LMS parameters. 'mref' and 'sref' are the M and S parameters at 240 months of age. For
sexn, '1' are boys and '2' are girls.
}

\usage{
data(cdc_ref_data)
}

\source{
\url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
}
\keyword{datasets}