\name{gradual_z}
\alias{gradual_z}
\title{Gradual WHO-to-CDC Transition Z-Scores}
\description{
Computes a smooth, gradually transitioning z-score for weight, height/length,
and body mass index (BMI) that blends World Health Organization (WHO) and
Centers for Disease Control and Prevention (CDC) growth reference z-scores
across early childhood. The gradual z-score is the WHO z-score through 730 days
(2.0 y) of age, the CDC z-score from 1826 days (5.0 y) onward, and a weighted
average of the two in between.

Gradual z-scores avoid the abrupt jump produced by switching directly from WHO to CDC
references at age 2 years as recommended by CDC.  For example, under an abrupt
switch, a girl weighing 9.9 kg would be at the 10th percentile (WHO) the day
before her second birthday and at the 2nd percentile (CDC) the following day;
\code{gradual_z} avoids this discontinuity.
}
\usage{
gradual_z(data,
         agedays = agedays,
         whowtz  = who_waz,
         cdcwtz  = waz,
         whohtz  = who_lhaz,
         cdchtz  = haz,
         whobmiz = who_bmiz,
         cdcbmiz = bmiz)
}
\arguments{
  \item{data}{A \code{data.frame}, \code{data.table}, or tibble containing,
    at minimum, the age column and at least one complete WHO/CDC z-score
    column pair. The same class is returned. Not every z-score pair is
    required -- see Value.}
  \item{agedays}{Column with age in days. \strong{Required.} Column names can be
      given unquoted or quoted.}
  \item{whowtz}{Column with the WHO weight-for-age z-score (default: \code{who_waz},
      as returned by \code{\link{who_z}}).}
  \item{cdcwtz}{Column with the CDC weight-for-age z-score (default: \code{waz},
      as returned by \code{\link{cdc_z}}).}
  \item{whohtz}{Column with the WHO length/height-for-age z-score (default: \code{who_lhaz}).}
  \item{cdchtz}{Column with the CDC height-for-age z-score (default: \code{haz}).}
  \item{whobmiz}{Column with the WHO BMI-for-age z-score (default: \code{who_bmiz}).}
  \item{cdcbmiz}{Column with the CDC BMI-for-age z-score (default: \code{bmiz}).}
}

\details{
Naming the arguments rather than relying on position is recommended. 
However, if \code{data} is the output of \code{\link{who_z}} and
\code{\link{cdc_z}} run on the same rows, the default column names match and the
function can be called as \code{gradual_z(data)}.

For each row, age in days determines which of three regions applies:
\itemize{
  \item \strong{agedays \eqn{\le} 730} (through 2 years): the WHO z-score is
    used directly.
  \item \strong{730 < agedays < 1826} (2 to 5 years): a weighted average of
    the WHO and CDC z-scores is used, with weights that shift linearly with
    age so the blended value equals the WHO z-score at 730 days and the CDC
    z-score at 1826 days.
  \item \strong{agedays \eqn{\ge} 1826} (5 years and beyond): the CDC
    z-score is used directly.
}
This produces a transition that is continuous at both boundaries, avoiding
the discontinuity that would result from switching abruptly from WHO to CDC
references at a single cutoff age.  Note that if \code{data} already contains 
\code{grad_waz}, \code{grad_haz}, or \code{grad_bmiz}, the column is overwritten 
and a \code{warning()} is issued.

For ages between 2 and 5 years, the gradual z-score is a weighted
average of the WHO and CDC z-scores:

\deqn{Z_{gradual} = \frac{Z_{WHO} \times (5 - \mathrm{ageyears}) + Z_{CDC} \times (\mathrm{ageyears} - 2)}{3}}{
gradual Z = (WHO_Z * (5 - ageyears) + CDC_Z * (ageyears - 2)) / 3}

where \code{ageyears = agedays / 365.25}. In this window a missing WHO or CDC z-score
gives a missing gradual z-score; outside it only the z-score in use is needed.

Gradual z-scores from 2 through 5 years of age are not directly comparable to those based
on either reference alone, and researchers may need sensitivity analyses to facilitate
comparison with other studies.

Because the WHO reference changes from recumbent length to standing height at 730/731
days, and the gradual z-score equals the WHO z-score through 730 days, \code{grad_haz}
and \code{grad_bmiz} keep the small discontinuity at that age that is present in
\code{who_lhaz} and \code{who_bmiz}.

The default \code{cdcbmiz = bmiz} uses the BMI z-score returned by \code{\link{cdc_z}},
which is the extended BMI z-score above the 95th percentile. To blend the
LMS-only CDC value instead, use \code{cdcbmiz = orig_bmiz}. Likewise,
\code{who_bmiz} from \code{\link{who_z}} uses the WHO adjustment for values
beyond 3 SD.
}

\value{
An object of the same class as \code{data} (\code{data.frame},
\code{data.table}, or tibble), containing all columns of \code{data}, plus:
\item{grad_waz}{Gradual (blended) weight-for-age z-score. Added only if both
  \code{whowtz} and \code{cdcwtz} are found in \code{data}.}
\item{grad_haz}{Gradual (blended) height/length-for-age z-score. Added only if
  both \code{whohtz} and \code{cdchtz} are found in \code{data}.}
\item{grad_bmiz}{Gradual (blended) BMI-for-age z-score. Added only if both
  \code{whobmiz} and \code{cdcbmiz} are found in \code{data}.}

Each of the three gradual measures is checked independently. If neither column of a
pair is found, that measure is skipped with an informational \code{message()}. 
If only one column of a pair is found, that measure is skipped with a \code{warning()} instead. 
If no complete pair is found, a \code{warning()} is issued.

In the output, each measure's columns are grouped together in the order WHO, CDC, and 
gradual (e.g. \code{who_waz}, \code{waz}, \code{grad_waz}), for weight,
then height, then BMI; any other input columns (including \code{agedays}) come first.
}

\seealso{
The original growth charts, tables, and R/Stata code are available at
\url{https://github.com/carriedaymont/gradual-who-cdc}.
}

\references{
Daymont C, Hwang W, Paul IM, Shur N, Freedman DS (2025).
"Creation and Evaluation of New Growth Charts With a Gradual
Transition From WHO to CDC Values." \emph{Pediatrics}, \bold{156}(3),
e2025070697. \doi{10.1542/peds.2025-070697}
}

\author{David Freedman \email{DavidSFreedman@gmail.com}}

\examples{
# Four children at 2, 3, 4 and 5 years with WHO and CDC z-scores already
# computed. At 730 days the gradual z-score is the WHO value, at 1826 days
# the CDC value, and in between a weighted average of the two.
d <- data.frame(
  agedays  = c(730, 1096, 1461, 1826),
  who_waz  = c(0.9, 0.7, 0.6, 0.4),
  waz      = c(0.7, 0.5, 0.3, 0.3),
  who_lhaz = c(-0.2, 0.1, 0.2, 0.1),
  haz      = c(-0.3, 0.0, 0.1, 0.2),
  who_bmiz = c(1.0, 0.9, 0.8, 0.7),
  bmiz     = c(0.1, 0.5, 0.2, 0.2))
gradual_z(d, agedays, who_waz, waz, who_lhaz, haz, who_bmiz, bmiz)

# The column names match the defaults, so this is the same call
gradual_z(d)

# Typical use: who_z() and cdc_z() on the same data, then gradual_z().
# NHanes (a data.table) has age in completed months and in days; cdc_z()
# needs months, with 0.5 added. bmi is dropped so both functions compute
# it from wt and the length/height column.
library(data.table)
d <- NHanes[agedays <= 1826]       # a subset is already a copy
d[, bmi := NULL]
d[, let(lenhei = fifelse(agedays <= 730, length, ht),
        agemos = agemos + 0.5)]
d <- who_z(d, agedays = agedays, wt = wt, lenhei = lenhei)
d <- cdc_z(d, age = agemos, wt = wt, ht = ht)
out <- gradual_z(d)
# children in the transition window: WHO, CDC and gradual z-scores side by side
head(out[agedays > 730 & agedays < 1826,
         .(seqn, agedays, who_waz, waz, grad_waz, who_bmiz, bmiz, grad_bmiz)])
}
\keyword{manip}
