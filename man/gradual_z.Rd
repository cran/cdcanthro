\name{gradual_z}
\alias{gradual_z}
\title{Gradual WHO-to-CDC Transition Z-Scores}
\description{
Computes a smooth, gradually transitioning z-score for weight, height/length,
and body mass index (BMI) that blends World Health Organization (WHO) and
Centers for Disease Control and Prevention (CDC) growth reference z-scores
across early childhood.  These gradual z-scores use WHO z-scores up to 730 days (2.0 y) of age,
CDC z-scores are used from 1826 days (5.0 y) of age onward, and a weighted average of the CDC and WHO
z-scores between ages 2 and 5 years. 

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
  \item{agedays}{Column for age in days.
      Note that all arguments can be either unquoted or quoted.}
  \item{whowtz}{WHO weight-for-age z-score.}
  \item{cdcwtz}{CDC weight-for-age z-score.}
  \item{whohtz}{WHO length/height-for-age z-score.}
  \item{cdchtz}{CDC height-for-age z-score.}
  \item{whobmiz}{WHO BMI-for-age z-score.}
  \item{cdcbmiz}{CDC BMI-for-age z-score.}
}

\details{
Naming the arguments rather than relying on position is recommended. 
However, if the input was created using \code{cdc_z} and
\code{who_z}, the function can be called as gradual_z(data). Argument values
can be quoted or unquoted.

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

It should be noted that gradual z-scores from 2 through 5 years of age 
are not directly comparable to those based on other reference data, 
and researchers may need sensitivity analyses to facilitate comparison with other studies.

Further, because of the change in recumbent length to standing height at age
2.0 years, a discontinuity at this age will be present for grad_haz and grad_bmiz.
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
library(data.table)
dt <- data.table(
  agedays  = c(730, 1096, 1461, 1826), # 2, 3, 4, and 5 years of age
  who_waz  = c(0.9, 0.7, 0.6, 0.4),
  waz      = c(0.7, 0.5, 0.3, 0.3),
  who_lhaz = c(-0.2, 0.1, 0.2, 0.1),
  haz      = c(-0.3, 0.0, 0.1, 0.2),
  who_bmiz = c(1.0, 0.9, 0.8, 0.7),
  bmiz     = c(0.1, 0.5, 0.2, 0.2)
)
result <- gradual_z(dt, agedays, who_waz, waz, who_lhaz, haz, who_bmiz, bmiz)
result[, ageyears:= round(agedays/365.25,2)]
result

gradual_z(dt) # because column names match defaults
}
\keyword{manip}
