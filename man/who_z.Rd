\name{who_z}
\alias{who_z}
\title{Sex- and Age- (or Length-) Standardized Weight, Length/Height, BMI,
and Head Circumference Metrics from the WHO Child Growth Standards}
\description{
Calculates z-scores and biologically-implausible-value (BIV) flags for
weight-for-age, length/height-for-age, BMI-for-age, head-circumference-for-age,
and weight-for-length, based on the WHO Child Growth Standards. The original
data are returned together with the calculated metrics.

This function is included in the cdcanthro package because CDC recommends using
the WHO Standards for children under 24 months of age and the CDC Growth Charts
(\code{\link{cdc_z}}) from 24 to < 240 months. The use of different references, along
with the change from recumbent length to standing height at age 2.0 years, creates
discontinuities at age 2.0 years (730 days); \code{\link{gradual_z}} smooths them.
\url{https://www.cdc.gov/growth-chart-training/hcp/overview/recommended.html}
}

\usage{
who_z(data,
          agedays = agedays,
          wt = wt,
          lenhei = lenhei,
          bmi = bmi,
          headc = headc,
          sex_code = 1:2)
}
\arguments{
  \item{data}{a \code{data.frame}, \code{data.table}, or tibble (\code{tbl_df}) containing the 
        input data. The class of the returned object matches the class of \code{data}.
        Must contain a variable named \code{sex} or \code{SEX} (but not both).}
  \item{agedays}{Age in completed days. \strong{Required.} Non-integer values are rounded
        to the nearest day, with a warning. If age is given as the completed number of months
        (as in NHANES), add 0.5 months, multiply by 30.4375 to convert to days, and round.
        Rows with ages over 1826 days (or negative) are kept, with \code{NA} for every metric.}
  \item{wt}{Weight in kg. \emph{Optional.}}
  \item{lenhei}{Recumbent length (ages <= 730 days) or standing height
        (ages > 730 days), in cm. \emph{Optional.} There is a difference of about 0.7 cm in
        the median between 730 and 731 days in the reference data, where the measurement
        changes from recumbent length to standing height; the same \code{lenhei} value
        therefore gives different z-scores on the two days. For weight-for-length, the
        reference data are tabulated in 0.01 cm increments and other lengths are linearly
        interpolated.}
  \item{bmi}{Body mass index, kg/m^2. \emph{Optional.} If not supplied but both \code{wt}
        and \code{lenhei} are, BMI is calculated as \code{wt / (lenhei/100)^2} and returned.}
  \item{headc}{Head circumference in cm. \emph{Optional.}}
  \item{sex_code}{Numeric vector of length 2, \code{c(male_code, female_code)}, giving
       the codes used for sex if other than 1 for boys and 2 for girls. \emph{Optional.}}
}
\details{
Column names can be given with or without quotation marks. Naming the arguments rather than
relying on position is recommended, e.g. \code{who_z(d, agedays = age, wt = weight)}; 
otherwise a value can silently land in the wrong slot. 
Any absent measurement is skipped, and its z-score is not returned.

The function expects the child's sex to be named \code{sex}; either upper- or
lower-case is fine. You cannot have both \code{SEX} and \code{sex} in your data.
Values can be coded as \code{1/2}, \code{boy(s)/girl(s)}, \code{b/g}, \code{male(s)/female(s)},
or \code{m/f}, in upper- or lower-case. 

If sex is numeric and not coded as 1/2, specify this in the optional \code{sex_code} argument; 
the code for boys is the first number.  For example, 0/1 coding would be specified 
as \code{sex_code} = 0:1 or \code{sex_code} = c(0,1).

At least one of \code{wt}, \code{lenhei}, \code{bmi} or \code{headc} must be present. Metrics that cannot 
be calculated from the supplied variables are silently omitted from the output. If a supplied 
column name doesn't exist in data (a likely typo), a warning is issued. If \code{data} already
has a column with the name of an output variable (for example \code{fwaz}), that column is
renamed with a \code{0} suffix and a warning is issued. Zero and negative measurements are
treated as missing.

Age in the WHO reference data goes from 0 to 1856 days (1856/365.25 = 5.08 y), 
but calculations in who_z go up to 1826 days (4.993 y). Weight-for-length Z
is calculated only for children <= 730 days of age with a length between 45 and 110 cm.

z-scores are calculated with the LMS method, \eqn{Z = ((X / M)^L - 1) / (L \times S)}.
For the weight-based indicators (weight-for-age, weight-for-length, and BMI-for-age), WHO's
restricted application of the method is used beyond \eqn{\pm 3}: the z-score is
recomputed as \eqn{3 + (X - SD3)/(SD3 - SD2)} above +3 and \eqn{-3 - (X - SD3neg)/(SD3neg - SD2neg)}
below -3, where SD2 and SD3 are the measurements at 2 and 3 standard deviations. This makes
extreme z-scores a linear function of the measurement rather than the result of extrapolating
the Box-Cox transformation. Length/height-for-age and head-circumference-for-age are not
adjusted (WHO 2006, chapter 7).
}
\value{
A \code{data.table}, tibble, or \code{data.frame} containing the original data plus the calculated metrics. 

WHO Z-scores:
\itemize{
  \item \code{who_waz}: weight-for-sex/age
  \item \code{who_lhaz}: length (or height) for sex/age
  \item \code{who_wflz}: weight-for-length for sex
  \item \code{who_bmiz}: BMI-for-sex/age
  \item \code{who_headcz}: head circumference for sex/age
}

\code{bmi} is also returned when it was computed from \code{wt} and \code{lenhei}.

BIV flags, following the \pkg{anthro} package. Each 'f' (flag) is 1 if the z-score is
outside the WHO plausibility range, 0 if inside, and NA if the z-score is NA:
\itemize{
  \item \code{fwaz}: 1 if \code{who_waz} < -6 or > 5
  \item \code{flhaz}: 1 if \code{who_lhaz} < -6 or > 6
  \item \code{fwflz}: 1 if \code{who_wflz} < -5 or > 5
  \item \code{fbmiz}: 1 if \code{who_bmiz} < -5 or > 5
  \item \code{fheadcz}: 1 if \code{who_headcz} < -5 or > 5
}
Note that there doesn't appear to be any publication that justifies these cutpoints.  While values
outside these ranges are outliers, the measurement may be correct.
}
\references{
World Health Organization, Department of Nutrition for Health and Development.
WHO Child Growth Standards: Length/height-for-age, weight-for-age,
weight-for-length, weight-for-height, and body mass index-for-age. Methods and
Development. Geneva; 2006.
\url{https://www.who.int/publications/i/item/924154693X}

The reference WHO LMS values for weight-for-age are in the 'Expanded Tables' files at
\url{https://www.who.int/tools/child-growth-standards/standards/weight-for-age}. Reference values
for the other measures are in the left column. 
}
\author{David Freedman \email{DavidSFreedman@gmail.com}}
\seealso{
\code{\link{cdc_z}} for standardized values based on the CDC growth charts,
\code{\link{gradual_z}} for a weighted average of the WHO and CDC z-scores between 2 and
5 years, and \code{\link{NHanes}} for the example data.
}
\examples{
# Four children, with sex given as text. lenhei is recumbent length through
# 730 days and standing height after that. The last two children have the
# same weight and lenhei at 730 and 731 days: who_lhaz differs because the
# reference changes from length to height, and who_wflz is NA after 730 days.
d <- data.frame(sex = c("M", "F", "M", "M"),
                agedays = c(30, 365, 730, 731),
                wt = c(4.2, 9.0, 12.2, 12.2),
                lenhei = c(53.5, 73.0, 87.1, 87.1),
                headc = c(37.5, 45.5, NA, NA))
out <- who_z(d, agedays = agedays, wt = wt, lenhei = lenhei, headc = headc)
print(out, digits = 3)

# BIV flags: a z-score outside the WHO plausibility range is flagged with 1.
# A one-year-old boy weighing 25 kg has who_waz > 5, so fwaz = 1.
d <- data.frame(sex = 1, agedays = 365, wt = c(9.5, 25))
who_z(d, agedays = agedays, wt = wt)

# NHanes (a data.table): build lenhei from length (<= 730 days) and
# height, and keep the WHO age range. bmi is not passed, so it is computed
# from wt and lenhei (the NHanes bmi column uses standing height and is
# missing for infants).
library(data.table)
d <- NHanes[agedays <= 1826]       # a subset is already a copy
d[, lenhei := fifelse(agedays <= 730, length, ht)]
out <- who_z(d, agedays = agedays, wt = wt, lenhei = lenhei, headc = headc)
head(out[, .(seqn, sex, agedays, who_waz, who_lhaz, who_bmiz,
             who_wflz, who_headcz, fwaz)])
}
