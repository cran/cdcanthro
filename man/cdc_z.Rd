\name{cdc_z}
\alias{cdc_z}
\alias{cdcanthro}
\title{
Generate Sex- and Age-Standardized Weight, Height, and BMI Metrics From the CDC Growth Charts
}
\description{
Generate z-scores, percentiles, and other sex- and age-standardized metrics
for weight, height, and BMI based on the 2000 CDC growth charts. For children with obesity,
the extended BMI z-scores and percentiles are calculated.  The calculations are restricted
to children with ages between 24.0 (inclusive) and < 240 months.  For younger children, use \code{who_z}.  
Note that \code{cdc_z} is the preferred name; the previous name, \code{cdcanthro}, will work but is deprecated.

The BMI metrics include z-scores and percentiles, along with newer metrics
such as extended BMIz and percent of the 50th and 95th percentiles. The
extended BMI metrics are used for BMIs above the 95th percentile.
}
\usage{
cdc_z(data, age = agemos, wt = wt, ht = ht, bmi = bmi, 
            sex_code = 1:2, all_vars = FALSE)
cdcanthro(data, age = agemos, wt = wt, ht = ht, bmi = bmi, 
            sex_code = 1:2, all_vars = FALSE)
}
\arguments{
  \item{data}{a \code{data.frame}, \code{data.table}, or tibble (\code{tbl_df}) containing the input data. The class of the returned object matches the class of \code{data}.}
  \item{age}{Age in months, specified as accurately as possible. \strong{Required.}}
  \item{wt}{Weight in kg. \emph{Optional.} If omitted, WAZ-related metrics will be missing.}
  \item{ht}{Height in cm. \emph{Optional.} If omitted, HAZ-related metrics will be missing.}
  \item{bmi}{BMI in \code{kg/m^2}. \emph{Optional.} If omitted but both \code{wt} and \code{ht} are supplied, BMI is computed as \code{wt / (ht / 100)^2}. If all three of \code{wt}, \code{ht}, and \code{bmi} are omitted, the function stops with an error.}
  \item{sex_code}{Numeric, single-digit coding for sex if other than 1 for boys and 2 for girls. 
       \emph{Optional.}}
  \item{all_vars}{logical. If \code{TRUE}, returns additional intermediate variables 
       (L, M, S values; sigma; and the distance-from-median metrics from Freedman et al.). 
       Default is \code{FALSE}.}
}
\details{
Argument values can be quoted (e.g., age='agemos') or unquoted (age = agemos).

The function expects the child's sex to be named \code{sex}; either upper- or
lower-case is fine. You cannot have both \code{SEX} and \code{sex} in your data.
Values can be coded as \code{boys/girls}, \code{b/g}, \code{male/female},
\code{m/f}, or \code{1/2}. Character values can be upper- or lower-case, and 
only the first character is considered. Be aware that a code of 'missing' will be interpreted 
as a boy.  

If sex is numeric and not coded as 1/2, specify this in the optional \code{sex_code} argument; 
the code for boys is the first number.  For example, 0/1 coding would be specified as 
\code{sex_code} = 0:1 or \code{sex_code} = c(0,1).

Weight is in kg, height is in cm, and BMI is in \code{kg/m^2}.

At least one of \code{wt}, \code{ht}, or \code{bmi} must be present. Metrics that cannot be 
calculated from the supplied variables are silently omitted from the output. If a supplied 
column name doesn't exist in data (a likely typo), a warning is issued.

Age in months should be given as accurately as possible because the function
linearly interpolates between completed months of age.  If ages are given as the 
completed number of months (as in NHANES), add 0.5 because values such as 36 represent the 
interval \code{[36, 37)} months. If the function detects integer numbers for all ages, it
will add 0.5 to all age values during the calculations; the input ages are not changed.

If age is in days, divide by 30.4375 so that a child who is 3672 days old
would have an age in months of 120.641. For additional information on age,
see the \sQuote{See Also} section below.

If \code{all_vars = TRUE}, all variables in the Freedman et al. paper will be
output, along with the L, M, and S values for each child and the value of
sigma for the half-normal distribution.

For children without obesity, BMI z-scores are calculated as
\deqn{Z = ((BMI / M)^L - 1) / (L \times S)}
where \code{BMI} is the child's BMI, \code{L} is the Box-Cox transformation
for normality for the child's sex and age, \code{M} is the median, and
\code{S} is the coefficient of variation. Reference data are the merged
LMS files from the CDC (see \sQuote{See Also}).

For children with obesity, BMI percentiles are calculated as
\deqn{90 + 10 \times \Phi((\mathrm{BMI} - \mathrm{p95}) / \sigma)}{90 + 10 * Phi((BMI - p95) / sigma)}
where \code{p95} is the sex- and age-specific 95th percentile, \eqn{\Phi} is the
CDF of a N(0, 1) distribution (\code{pnorm()}), and \code{sigma} is the scale parameter
of the half-normal distribution. Values of sigma for children with obesity
are based on formulas in Wei et al.

The class of the returned object matches the class of the input \code{data}:
a \code{data.frame} input returns a \code{data.frame}, a \code{data.table}
returns a \code{data.table}, and a tibble (\code{tbl_df}) returns a tibble
(provided the \pkg{tibble} package is installed; otherwise a
\code{data.frame} is returned with a warning).
}
\value{
A \code{data.table}, \code{data.frame}, or tibble containing the original
data and various weight, height, and BMI metrics.

If your dataset has ages that are < 24 or >= 240 months, these rows will be present in the output
dataset.  All calculated metrics will be NA. If this is not what you want, delete these rows before
running \code{cdc_z()}.

Variables in output:
\describe{
  \item{\code{waz}, \code{haz}, \code{bmiz}}{Sex- and age-standardized z-scores for weight, height, and BMI. \code{bmiz} is based on the 2000 CDC growth charts for non-obese children and extended BMIz for obese children.}
  \item{\code{wap}, \code{hap}, \code{bmip}}{Sex- and age-standardized percentiles for weight, height, and BMI. \code{bmip} is based on the extended BMI metrics for obese children.}
  \item{\code{mod_waz}, \code{mod_haz}, \code{mod_bmiz}}{Modified z-scores used to flag outliers; see the CDC SAS program.}
  \item{\code{orig_bmip}, \code{orig_bmiz}}{Original BMI percentiles and z-scores based solely on the LMS method. \code{orig_bmiz} and \code{bmiz} are identical for BMIs at or below the 95th percentile.}
  \item{\code{p95}}{The 95th percentile for BMI in the reference data for each sex and age.}
  \item{\code{bmip95}}{BMI expressed as a percentage of the 95th percentile; 120 percent is the lower threshold for severe obesity.}
  \item{\code{perc_median}}{Percent difference between the child's BMI and the median (50th percentile) BMI for the child's sex and age.}
}

If \code{all_vars = TRUE}, additional BMI metrics described in Freedman et al.
are also returned. These express BMI as distance or percent distance from
the median. To convert to percent of the median, add 100 to the values.
}
\note{
Argument names can be quoted or unquoted: \code{cdc_z(data, age, wt, ht, bmi)},
and \code{cdc_z(data, 'age', 'wt', 'ht', 'bmi')} produce the same result.
}
\seealso{
\code{who_z} for standardized values for children < age 2.0 y based on the WHO charts

CDC LMS percentile data files: \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}

CDC extended BMI data files: \url{https://www.cdc.gov/growthcharts/extended-bmi-data-files.htm}

CDC SAS programs and more information on \code{agemos} in Table 1 of
\url{https://www.cdc.gov/growth-chart-training/hcp/computer-programs/sas.html}
}
\references{
Kuczmarski RJ, Ogden CL, Guo SS, Grummer-Strawn LM, Flegal KM, Mei Z, et al.
2000 CDC Growth Charts for the United States: methods and development.
\emph{Vital and Health Statistics Series 11, Data from the National Health Survey}
2002;11:1--190.

Wei R, Ogden CL, Parsons VL, Freedman DS, Hales CM.
A method for calculating BMI z-scores and percentiles above the 95th percentile
of the CDC growth charts. \emph{Annals of Human Biology} 2020;47(6):514--521.
\doi{10.1080/03014460.2020.1808065}

Freedman DS, Woo JG, Ogden CL, Xu JH, Cole TJ.
Distance and Percent Distance from Median BMI as Alternatives to BMI z-score.
\emph{British Journal of Nutrition} 2019;124:1--8.
}
\author{David Freedman \email{DavidSFreedman@gmail.com}}
\examples{
data <- data.table::CJ(sex = 1:2, agem = 120.5, wtk = c(30, 65), htc = c(135, 144))
data[, let(bmi = wtk / (htc / 100)^2)]
out <- cdc_z(data, age = agem, wt = wtk, ht = htc, bmi = bmi)
round(out, 2)[1:5]

out <- cdc_z(data, age = agem, wt = wtk, ht = htc, bmi = bmi, all_vars = TRUE)
round(out, 2)[1:5]

d <- data.frame(sex = c(1, 2, 1, 2, 2),
                age = c(141, 54, 217, 155, 52),
                wt  = c(57, 25, 72, NA, 17.7),
                ht  = c(143, 102, 166, 169, 105))
d <- cdc_z(d, age, wt, ht)
round(d, 2)

d <- data.frame(sex = c(0, 1, 0),
                age = c(120.5, 60.5, 180.5),
                wt  = c(40, 25, 65))
cdc_z(d, age, wt, sex_code = 0:1) # note use of sex_code

# tibble input returns a tibble (requires the tibble package)
if (requireNamespace("tibble", quietly = TRUE)) {
  d_tbl <- tibble::as_tibble(d)
  cdc_z(d_tbl, age, wt, sex_code = 0:1)
}

head(NHanes) # age is given as completed months
d <- data.table::copy(NHanes)
head(cdc_z(d, agemos, wt, ht, bmi))[,.(seqn, agemos, waz, haz, bmiz)]
d[, let(agemos = agemos + 0.5)]
head(cdc_z(d, agemos, wt, ht, bmi))[,.(seqn, agemos, waz, haz, bmiz)]
# same Z-scores
}
