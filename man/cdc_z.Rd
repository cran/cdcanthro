\name{cdc_z}
\alias{cdc_z}
\alias{cdcanthro}
\title{
Generate Sex- and Age-Standardized Weight, Height, and BMI Metrics From the CDC Growth Charts
}
\description{
Generate z-scores, percentiles, and other sex- and age-standardized metrics
for weight, height, and BMI based on the 2000 CDC growth charts. For children with obesity,
the extended BMI z-scores and percentiles are calculated. The calculations are restricted
to children with ages from 24.0 (inclusive) to < 240 months; for younger children, use
\code{\link{who_z}}. \code{cdc_z} is the preferred name; the previous name,
\code{cdcanthro}, still works but is deprecated.

The BMI metrics include z-scores and percentiles, along with newer metrics such as the extended
BMIz and BMI expressed as a percentage of the 50th and 95th percentiles. The extended BMI
metrics are used for BMIs at or above the 95th percentile.
}
\usage{
cdc_z(data, age = agemos, wt = wt, ht = ht, bmi = bmi, 
            sex_code = 1:2, all_vars = FALSE)
cdcanthro(data, age = agemos, wt = wt, ht = ht, bmi = bmi, 
            sex_code = 1:2, all_vars = FALSE)
}
\arguments{
  \item{data}{a \code{data.frame}, \code{data.table}, or tibble (\code{tbl_df}) containing the input data. 
        The class of the returned object matches the class of \code{data}.}
  \item{age}{Age in months, specified as accurately as possible. \strong{Required.}
        Rows with ages < 24 or >= 240 months are kept, with \code{NA} for every metric.}
  \item{wt}{Weight in kg. \emph{Optional.} If omitted, the weight metrics (\code{waz}, \code{wap},
        \code{mod_waz}) are not included in the output.}
  \item{ht}{Height in cm. \emph{Optional.} If omitted, the height metrics (\code{haz}, \code{hap},
        \code{mod_haz}) are not included in the output.}
  \item{bmi}{BMI in \code{kg/m^2}. \emph{Optional.} If omitted but both \code{wt} and \code{ht} are supplied, 
        BMI is computed as \code{wt / (ht / 100)^2}. If all three of \code{wt}, \code{ht}, and \code{bmi} are omitted, 
        the function stops with an error.}
  \item{sex_code}{Numeric vector of length 2, \code{c(male_code, female_code)}, giving
       the codes used for sex if other than 1 for boys and 2 for girls. \emph{Optional.}}
  \item{all_vars}{logical. If \code{TRUE}, also returns the L, M, and S values for BMI,
       \code{sigma}, and the distance-from-median metrics of Freedman et al.; see Value.
       Default is \code{FALSE}.}
}
\details{
Column names can be given unquoted (\code{age = agemos}), quoted (\code{age = 'agemos'}),
or by position; \code{cdc_z(data, age, wt, ht, bmi)} and
\code{cdc_z(data, 'age', 'wt', 'ht', 'bmi')} give the same result. Naming the arguments
is recommended, since a positional value can silently land in the wrong slot.

The function expects the child's sex to be named \code{sex}; either upper- or
lower-case is fine. You cannot have both \code{SEX} and \code{sex} in your data.
Values can be coded as \code{1/2}, \code{boy(s)/girl(s)}, \code{b/g}, \code{male(s)/female(s)},
or \code{m/f}, in upper- or lower-case.  

If sex is numeric and not coded as 1/2, specify this in the optional \code{sex_code} argument; 
the code for boys is the first number.  For example, 0/1 coding would be specified as 
\code{sex_code} = 0:1 or \code{sex_code} = c(0,1).

Weight is in kg, height is in cm, and BMI is in \code{kg/m^2}. Zero and negative
measurements are treated as missing.

At least one of \code{wt}, \code{ht}, or \code{bmi} must be present. Metrics that cannot be 
calculated from the supplied variables are silently omitted from the output. If a supplied 
column name doesn't exist in data (a likely typo), a warning is issued. If \code{data} already
has a column with the name of an output variable (for example \code{bmiz}), that column is
renamed with a \code{0} suffix (\code{bmiz0}) and a warning is issued.

Age in months should be given as accurately as possible: the reference values are tabulated at
half-month ages (23.5, 24.5, \ldots, 239.5, 240), and the L, M, and S values are linearly
interpolated to each child's age. If ages are given as the completed number of months (as in
NHANES), add 0.5, because a value such as 36 represents the interval \code{[36, 37)} months.
If every age is a whole number, the function adds 0.5 itself during the calculations and
warns that it has done so; the input ages are not changed.

If age is in days, divide by 30.4375 so that a child who is 3672 days old
would have an age in months of 120.641. For additional information on age,
see the \sQuote{See Also} section below.

If \code{all_vars = TRUE}, all variables in the Freedman et al. paper will be
output, along with the L, M, and S values for each child and the value of
sigma for the half-normal distribution.

Weight, height, and (for children without obesity) BMI z-scores are calculated as
\deqn{Z = ((X / M)^L - 1) / (L \times S)}
where \code{X} is the child's measurement, \code{L} is the Box-Cox power
for normality for the child's sex and age, \code{M} is the median, and
\code{S} is the coefficient of variation. When \eqn{|L| < 0.01} the log form
\eqn{Z = \log(X / M) / S} is used, as in the CDC SAS program. Reference data are the
merged LMS files from the CDC (see \sQuote{See Also}).

For children with obesity (BMI at or above the 95th percentile), BMI percentiles are
calculated as
\deqn{90 + 10 \times \Phi((\mathrm{BMI} - \mathrm{p95}) / \sigma)}{90 + 10 * Phi((BMI - p95) / sigma)}
where \code{p95} is the sex- and age-specific 95th percentile, \eqn{\Phi} is the
CDF of a N(0, 1) distribution (\code{pnorm()}), and \code{sigma} is the scale parameter
of a half-normal distribution, given by the sex-specific formulas in Wei et al. and
Hales et al. The extended z-score is the normal quantile of this percentile, capped at 8.21.

The modified z-scores (\code{mod_waz}, \code{mod_haz}, \code{mod_bmiz}) express the
distance of a measurement from the median in units of half the distance between the median
and the +2 SD value (for measurements above the median) or the -2 SD value (below the
median). They are used in the CDC SAS program to flag biologically implausible values.

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

Variables in output (only those computable from the supplied measurements are present):
\describe{
  \item{\code{bmi}}{Added only when BMI was computed from \code{wt} and \code{ht}.}
  \item{\code{waz}, \code{haz}, \code{bmiz}}{Sex- and age-standardized z-scores for weight, height, and BMI. \code{bmiz} is the LMS z-score for children without obesity and the extended BMIz for children with obesity.}
  \item{\code{wap}, \code{hap}, \code{bmip}}{Sex- and age-standardized percentiles for weight, height, and BMI. \code{bmip} is the extended BMI percentile for children with obesity.}
  \item{\code{mod_waz}, \code{mod_haz}, \code{mod_bmiz}}{Modified z-scores used to flag biologically implausible values; see Details and the CDC SAS program.}
  \item{\code{orig_bmip}, \code{orig_bmiz}}{BMI percentiles and z-scores based solely on the LMS method. \code{orig_bmiz} and \code{bmiz} are identical for BMIs below the 95th percentile.}
  \item{\code{p95}}{The 95th percentile of BMI in the reference data for the child's sex and age.}
  \item{\code{bmip95}}{BMI expressed as a percentage of the 95th percentile; 120 percent is the lower threshold for severe obesity.}
  \item{\code{perc_median}}{Percent difference between the child's BMI and the median (50th percentile) BMI for the child's sex and age.}
}

If \code{all_vars = TRUE}, the following are also returned:
\describe{
  \item{\code{bmi_l}, \code{bmi_m}, \code{bmi_s}}{The L, M, and S values for BMI at the child's sex and age.}
  \item{\code{sigma}}{The scale parameter of the half-normal distribution used for the extended BMI metrics.}
  \item{\code{dist_median}}{BMI minus the median BMI (kg/m^2).}
  \item{\code{adj_dist_median}}{\code{dist_median} adjusted for the age-related change in the spread of BMI, by scaling to the BMI distribution at 240 months (20 years).}
  \item{\code{adj_perc_median}}{\code{perc_median} adjusted in the same way.}
  \item{\code{log_perc_median}, \code{adj_log_perc_median}}{Percent distance from the median on the log scale, unadjusted and adjusted.}
}
These metrics are described in Freedman et al. To convert a percent distance from the median
to percent of the median, add 100.
}
\seealso{
\code{\link{who_z}} for standardized values for children under 5 years based on the WHO standards,
and \code{\link{gradual_z}} for a weighted average of the WHO and CDC z-scores between 2 and 5 years.
\code{\link{NHanes}} for the example data.

CDC LMS percentile data files: \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}

CDC extended BMI data files: \url{https://www.cdc.gov/growthcharts/extended-bmi-data-files.htm}

CDC SAS programs and more information on \code{agemos} in Table 1 of
\url{https://www.cdc.gov/growth-chart-training/hcp/computer-programs/sas.html}
}
\references{
Kuczmarski RJ, Ogden CL, Guo SS, Grummer-Strawn LM, Flegal KM, Mei Z, et al.
2000 CDC Growth Charts for the United States: methods and development.
\emph{Vital and Health Statistics Series 11, Data from the National Health Survey}
2002;11(246):1--190.

Wei R, Ogden CL, Parsons VL, Freedman DS, Hales CM.
A method for calculating BMI z-scores and percentiles above the 95th percentile
of the CDC growth charts. \emph{Annals of Human Biology} 2020;47(6):514--521.
\doi{10.1080/03014460.2020.1808065}

Hales CM, Freedman DS, Akinbami L, Wei R, Ogden CL.
Evaluation of alternative body mass index (BMI) metrics to monitor weight status in
children and adolescents with extremely high BMI using CDC BMI-for-age growth charts.
\emph{Vital and Health Statistics Series 2} 2022;(197):1--42.
\doi{10.15620/cdc:121711} \url{https://stacks.cdc.gov/view/cdc/121711}

Freedman DS, Woo JG, Ogden CL, Xu JH, Cole TJ.
Distance and Percent Distance from Median BMI as Alternatives to BMI z-score.
\emph{British Journal of Nutrition} 2020;124(5):493--500.
\doi{10.1017/S0007114519002046}
}
\author{David Freedman \email{DavidSFreedman@gmail.com}}
\examples{
# Two boys and two girls, all 10 years old (120.5 months). bmi is
# computed from wt and ht because it is not supplied.
d <- data.frame(sex = c(1, 1, 2, 2), agemos = 120.5,
                wt = c(30, 65, 30, 65), ht = c(135, 144, 135, 144))
out <- cdc_z(d, age = agemos, wt = wt, ht = ht)
round(out, 2)

# The heavier child of each sex has a BMI above the 95th percentile, so
# bmiz and bmip are the extended values. orig_bmiz and orig_bmip are the
# LMS values, which compress very high BMIs into a narrow range.
out[, c("sex", "bmi", "bmip95", "orig_bmiz", "bmiz", "orig_bmip", "bmip")]

# all_vars = TRUE adds L, M, S, sigma and the distance-from-median metrics
names(cdc_z(d, age = agemos, wt = wt, ht = ht, all_vars = TRUE))

# Column names can also be quoted or given by position. Whole-number ages
# are completed months, so 0.5 is added (with a warning). A missing
# measurement gives NA for that child's metrics, and an age outside 24 to
# < 240 months (here, 20) gives NA for all metrics, but the row is kept.
d <- data.frame(sex = c(1, 2, 1, 2, 2),
                age = c(141, 54, 217, 155, 20),
                wt  = c(57, 25, 72, NA, 11),
                ht  = c(143, 102, 166, 169, 85))
round(cdc_z(d, "age", "wt", "ht"), 2)

# Sex coded 0/1 (boys = 0) needs sex_code; text codes are recognized directly
d <- data.frame(sex = c(0, 1, 0), age = c(120.5, 60.5, 180.5), wt = c(40, 25, 65))
cdc_z(d, age, wt, sex_code = 0:1)
d$sex <- c("M", "F", "boy")
cdc_z(d, age, wt)

# tibble input returns a tibble (requires the tibble package)
if (requireNamespace("tibble", quietly = TRUE)) {
  cdc_z(tibble::as_tibble(d), age, wt)
}

# NHanes (a data.table): ages are completed months, so cdc_z() adds 0.5
# and warns. Adding 0.5 yourself gives the same result without the warning.
library(data.table)
head(NHanes)
out1 <- cdc_z(NHanes, agemos, wt, ht, bmi)
d <- copy(NHanes)[, agemos := agemos + 0.5]
out2 <- cdc_z(d, agemos, wt, ht, bmi)
all.equal(out1$bmiz, out2$bmiz)
head(out2[, .(seqn, sex, agemos, waz, haz, bmiz, bmip)])
}
