\name{cdcanthro}

\alias{cdcanthro}
\title{
Generate Sex- and Age-Standardized Weight, Height, and BMI Metrics From the CDC Growth Charts
}
\description{
Generate z-scores, percentiles, and other sex- and age-standardized metrics
for weight, height, and BMI based on the 2000 CDC growth charts.
Has a single function, \code{cdcanthro}.

The BMI metrics include z-scores and percentiles, along with newer metrics
such as extended BMIz and percent of the 50th and 95th percentiles. The
extended BMI metrics are used for BMIs above the 95th percentile.
}
\usage{
cdcanthro(data, age, wt, ht, bmi, all = FALSE)
}

\arguments{
  \item{data}{a \code{data.frame}, \code{data.table}, or tibble (\code{tbl_df}) containing the input data. The class of the returned object matches the class of \code{data}.}
  \item{age}{age in months, specified as accurately as possible. \strong{Required.}}
  \item{wt}{weight in kg. \emph{Optional.} If omitted, WAZ-related metrics will be \code{NA}.}
  \item{ht}{height in cm. \emph{Optional.} If omitted, HAZ-related metrics will be \code{NA}.}
  \item{bmi}{BMI in \code{kg/m^2}. \emph{Optional.} If omitted but both \code{wt} and \code{ht} are supplied, BMI is computed as \code{wt / (ht / 100)^2}. If all three of \code{wt}, \code{ht}, and \code{bmi} are omitted, the function stops with an error.}
  \item{all}{logical. If \code{TRUE}, returns additional intermediate variables (L, M, S values; sigma; and the distance-from-median metrics from Freedman et al.). Default is \code{FALSE}.}
}
\details{
The function expects the child's sex to be named \code{sex}; either upper- or
lower-case is fine. You cannot have both \code{SEX} and \code{sex} in your data.
Values can be coded as \code{boys/girls}, \code{b/g}, \code{male/female},
\code{m/f}, or \code{1/2}. Character values can be in upper- or lower-case;
only the first character is considered.

Weight is in kg, height is in cm, and BMI is in \code{kg/m^2}.

At least one of \code{wt}, \code{ht}, or \code{bmi} must be present. Metrics
that cannot be calculated from the supplied variables will be returned as
\code{NA}, and warnings will indicate which variables were missing. For
example, supplying only \code{wt} returns WAZ but leaves HAZ and BMI metrics
as \code{NA}; supplying only \code{bmi} returns BMI metrics but leaves WAZ
and HAZ as \code{NA}.

Age in months should be given as accurately as possible because the function
linearly interpolates between ages.

If all ages are given as completed number of months (e.g., NHANES), add 0.5
because values such as 36 represent the interval \code{[36, 37)} months.

If age is in days, divide by 30.4375 so that a child who is 3672 days old
would have an age in months of 120.641. For additional information on age,
see the documentation for \code{agemos} at the CDC SAS program page (see
\code{\link{cdcanthro}} \sQuote{See Also} section).

If \code{all = TRUE}, all variables in the Freedman et al. paper will be
output, along with the L, M, and S values for each child and the value of
sigma for the half-normal distribution.

For children without obesity, BMI z-scores are calculated as
\deqn{Z = ((BMI / M)^L - 1) / (L \times S)}
where \code{BMI} is the child's BMI, \code{L} is the Box-Cox transformation
for normality for the child's sex and age, \code{M} is the median, and
\code{S} is the coefficient of variation. Reference data are the merged
LMS files from the CDC (see \sQuote{See Also}).

For children with obesity, BMI percentiles are calculated as
\deqn{90 + 10 \times \Phi((BMI - p95) / \sigma)}
where \code{p95} is the sex- and age-specific 95th percentile, and
\code{sigma} is the scale parameter of the half-normal distribution. Values
of sigma for children with obesity are based on formulas in Wei et al.
(2020).

The class of the returned object matches the class of the input \code{data}:
a \code{data.frame} input returns a \code{data.frame}, a \code{data.table}
returns a \code{data.table}, and a tibble (\code{tbl_df}) returns a tibble
(provided the \pkg{tibble} package is installed; otherwise a
\code{data.frame} is returned with a warning).
}

\value{
A \code{data.table}, \code{data.frame}, or tibble containing the original
data and various weight, height, and BMI metrics.

Variables in output:
\describe{
  \item{\code{waz}, \code{haz}, \code{bmiz}}{Sex- and age-standardized z-scores for weight, height, and BMI. \code{bmiz} is based on the 2000 CDC growth charts for non-obese children and extended BMIz for obese children.}
  \item{\code{mod_waz}, \code{mod_haz}, \code{mod_bmiz}}{Modified z-scores.}
  \item{\code{orig_bmip}, \code{orig_bmiz}}{Original BMI percentiles and z-scores based on the LMS method. \code{orig_bmiz} and \code{bmiz} are identical for children with BMI at or below the 95th percentile.}
  \item{\code{bmip95}}{BMI expressed as a percentage of the 95th percentile; 120 percent is the lower threshold for severe obesity.}
}

If \code{all = TRUE}, additional BMI metrics described in Freedman et al.
are also returned. These express BMI as distance or percent distance from
the median. To convert to percent of the median, add 100 to the values.
}

\note{
The input data must not contain a column named \code{seq_}, as this name is
used internally to preserve row order. Rename any such column before calling
\code{cdcanthro()}.

\code{library(cdcanthro)} will also load (but not attach) the
\pkg{data.table} and \pkg{tibble} packages.

Do not quote the argument names. Use \code{cdcanthro(data, age, wt, ht, bmi)},
not \code{cdcanthro(data, 'age', 'wt', 'ht', 'bmi')}.
}

\seealso{
CDC LMS percentile data files: \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}

CDC extended BMI data files: \url{https://www.cdc.gov/growthcharts/extended-bmi-data-files.htm}

CDC SAS programs and \code{agemos} documentation: \url{https://www.cdc.gov/growth-chart-training/hcp/computer-programs/sas.html}
}

\references{
Kuczmarski RJ, Ogden CL, Guo SS, Grummer-Strawn LM, Flegal KM, Mei Z, et al.
2000 CDC Growth Charts for the United States: methods and development.
\emph{Vital and Health Statistics Series 11, Data from the National Health Survey}
2002;11:1--190.

Wei R, Ogden CL, Parsons VL, Freedman DS, Hales CM.
A method for calculating BMI z-scores and percentiles above the 95th percentile
of the CDC growth charts. \emph{Annals of Human Biology} 2020;47:514--21.

Freedman DS, Woo JG, Ogden CL, Xu JH, Cole TJ.
Distance and Percent Distance from Median BMI as Alternatives to BMI z-score.
\emph{British Journal of Nutrition} 2019;124:1--8.
}

\author{
David Freedman
}

\examples{
data <- data.table::CJ(sex = 1:2, agem = 120.5, wtk = c(30, 65), htc = c(135, 144))
data[, bmi := wtk / (htc / 100)^2]
out <- cdcanthro(data, age = agem, wt = wtk, ht = htc, bmi = bmi)
round(out, 2)[1:5]

out <- cdcanthro(data, age = agem, wt = wtk, ht = htc, bmi = bmi, all = TRUE)
round(out, 2)[1:5]

d <- data.frame(sex = c(1, 2, 1, 2, 2),
                age = c(141, 54, 217, 155, 52),
                wt  = c(57, 25, 72, NA, 17.7),
                ht  = c(143, 102, 166, 169, 105))
d <- cdcanthro(d, age, wt, ht)
round(d, 2)

d <- data.frame(sex = c(1, 2, 1),
                age = c(120.5, 60.5, 180.5),
                wt  = c(40, 25, 65))
d <- tibble::as_tibble(d)
cdcanthro(d, age, wt)

NHanes[, agemos := agemos + 0.5]   # age was given as completed months
out <- cdcanthro(NHanes, agemos, wt, ht, bmi)
round(out, 2)
}
