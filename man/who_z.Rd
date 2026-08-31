\name{who_z}
\alias{who_z}
\title{Sex- and Age- (or Length-) Standardized Weight, Length/Height, BMI,
and Head Circumference Metrics from the WHO Growth Charts}
\description{
Calculates z-scores and biologically-implausible-value (BIV) flags for
weight-for-age, length/height-for-age, BMI-for-age, head-circumference-for-age,
and weight-for-length, based on the WHO Child Growth Standards. The original
data are returned together with the calculated metrics.

This function is included in the cdcanthro package because CDC recommends using
(1) the CDC Growth Charts for children with ages (months) >=24 and < 240, and (2)
the WHO Standards for children < 24 months of age. The use of these different charts, along
with the change from recumbent length to standing height at age 2.0 y, creates
discontinuities at age 2.0 years (730 days).
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
  \item{data}{a data.frame or data.table of measurements. Must contain a
    variable named \code{sex} or \code{SEX} (but not both).}
  \item{agedays}{age in completed days. Note that all arguments can be unquoted (e.g., 
        agedays = days) or quoted(agedays = 'days')
        If age is given as completed number of months (e.g., NHANES), add 0.5 months,
        multiply by 30.4375 to convert to agedays, and round to the nearest day.}
  \item{wt}{weight in kg.}
  \item{lenhei}{recumbent length (ages <= 730 days) or standing height
        (ages > 730 days), in cm.  Note that there is an ~0.7 difference in M (median) between
        730 and 731 days in the reference data, when the measurement changes from
        recumbent length to standing height}
  \item{bmi}{body mass index, kg/m^2. If not supplied but both \code{wt} and
    \code{lenhei} are, BMI is calculated as \code{wt / (lenhei/100)^2}.}
  \item{headc}{head circumference in cm.}
  \item{sex_code}{Numeric coding for sex if other than 1 for boys and 2 for girls. 
       \emph{Optional.}}
}
\details{
Column names can be given with or without quotation marks. Naming the arguments rather than
relying on position is recommended, e.g. \code{who_z(d, agedays = age, wt = weight)}; 
otherwise a value can silently land in the wrong slot. 
Any absent measurement is skipped with a warning, and its z-score is not returned.

\code{sex} may be coded as \code{boys/girls}, \code{male/female}, or
\code{1/2}. Character values may be upper or lower case; only the first
character is used, so 'b' / 'g' is fine. If sex is numeric and not coded as 1/2, specify this 
in the optional \code{sex_code} argument; the code for boys is the first number.  
For example, \code{0/1} coding would be specified as \code{sex_code} = 0:1 or 
\code{sex_code} = c(0,1).


Age in the WHO reference data goes from 0 to 1856 days. However, weight-for-length Z
is calculated only for children <= 730 days of age with a length between 45 and 110 cm.
}
\value{
A data.table containing the original data plus the calculated metrics. The age-based z-scores
are calculated for children with an agedays between 0 and 1826 (inclusive). Weight-for-length
z is calculated for children between 0 and 730 days.

WHO Z-scores:
\itemize{
  \item \code{who_waz}: weight-for-sex/age
  \item \code{who_lhaz}: length (or height) for sex/age
  \item \code{who_wflz}: weight-for-length for sex
  \item \code{who_bmiz}: BMI-for-sex/age
  \item \code{who_headcz}: head circumference for sex/age
}

BIV flags, following the \pkg{anthro} package. Each 'f' (flag) is 1 if the z-score is
outside the WHO plausibility range, 0 if inside, and NA if the z-score is NA:
\itemize{
  \item \code{fwaz}: 1 if \code{who_waz} < -6 or > 5
  \item \code{flhaz}: 1 if \code{who_lhaz} < -6 or > 6
  \item \code{fwflz}: 1 if \code{who_wflz} < -5 or > 5
  \item \code{fbmiz}: 1 if \code{who_bmiz} < -5 or > 5
  \item \code{fheadcz}: 1 if \code{who_headcz} < -5 or > 5
}
Note that there doesn't appear to be any publications that justify these cutpoints. While values
outside these ranges are outliers, the measurement may be correct.
}
\references{
World Health Organization, Department of Nutrition for Health and Development.
WHO Child Growth Standards: Length/height-for-age, weight-for-age,
weight-for-length, weight-for-height, and body mass index-for-age. Methods and
Development. Geneva; 2006.
\url{https://apps.who.int/iris/handle/10665/43413}

The reference LMS values are in the 'Expanded Percentile' files at
\doi{https://doi.org/10.1111/j.1651-2227.2006.tb02378.x}
}
\author{David Freedman \email{DavidSFreedman@gmail.com}}
\seealso{\code{cdc_z} for standardized values based on the CDC growth charts
}
\examples{
NHanes
d <- NHanes
d <- data.table::copy(NHanes)
d[, let(lenhei=ifelse(agedays<=730,length,ht))]
# lenhei is a composite of length and ht:
# it is length for 0 to <=730 days, but height for >730 days
d <- d[agedays <= 1826]
out <- who_z(d, agedays, wt, lenhei, bmi, headc)
head(out)

# verify that the function works for head circumference (small N)
temp <- NHanes[!is.na(headc)]
temp[, let(lenhei=ifelse(agedays<=730,length,ht))]
out <- who_z(temp, agedays, wt, lenhei, bmi, headc)
head(out)
}
