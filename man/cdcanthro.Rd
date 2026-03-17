\name{cdcanthro}

\alias{cdcanthro}
\title{
Generate Sex- and Age-Standardized Weight, Height, and BMI Metrics From the CDC Growth Charts
}
\description{
Generate z-scores, percentiles, and other metrics for weight, height, and BMI based on the 2000 CDC growth charts.
Has a single function, 'cdcanthro'.  Requires the package data.table to be
installed; library(cdcanthro) will also attach data.table.

The BMI metrics included z-scores and percentiles base on the growth charts,
along with various newer metrics such as extended BMIz, and percent of the 50th and 95th
percentiles. The extended BMI metrics are used for BMIs above the 95th percentile.
}
\usage{
cdcanthro(data, age, wt, ht, bmi, all = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.frame or data.table}
  \item{age}{age in months specified as accuately as possible.}
  \item{wt}{weight (kg).}
  \item{ht}{height (cm).}
  \item{bmi}{BMI, kg/m^2.}
  \item{all}{Logical. If \code{TRUE}, returns all intermediate variables used in calculations. Defaults to \code{FALSE}.}
}
\details{
The function expects the child's sex to be named 'sex'; either upper- or lower-case is fine. You cannot have both 'SEX' and 'sex' in your data. Values can be coded as 'boys/girls', 'b/g', 'male/female', 'm/f', or '1/2'. Character values can be in upper- or lower-case; only the first character is considered.

Weight is in kg, and ht is in cm.  BMI is kg/m^2.

Age in months should be given as accurately as possible because the function linearly interpolates between ages.  If completed number of months is known (e.g., NHANES), add 0.5 because NHANES value are 24, 25, 26, etc.

If age is in days, divide by 30.4375 so that a child who is 3672 days old would have an age in months of 120.641.

For additional information on age, see information on agemos at
\url{https://www.cdc.gov/growth-chart-training/hcp/computer-programs/sas.html}

If all=TRUE, all variables in Freedman et al. paper will be output. Default is FALSE.  If all=TRUE, will also output the L, M, and S values for each child and the value of sigma for the half-normal distribution. Default is FALSE

The calculation of BMI z-scores for children without obesity is Z = (((BMI / M) ^ L) -1) / (L*S) where BMI is the child’s BMI, L is Box-Cox transformation for normality for the child’s sex and age, M is median, and S is coefficient of variation. Reference data are the merged LMS files at https://www.cdc.gov/growthcharts/percentile_data_files.htm (Centers for Disease Control and Prevention (CDC), 2022). Values of sigma for children with obesity are based on formulas in the Wei et al. (2020) paper.

For children with obesity, BMI percentiles are calculated as 90 + 10*pnorm((BMI - p95) / sigma) where p95 is the sex-and age-specific 95th percentile, and sigma is the scale distribution of the half-normal distribution.



}
\value{
Returns a data.table containing the original data and various weight, height, and BMI metrics.  Can convert this to a dataframe with 'setDF(output_data)'.

Variables in output:

waz, haz, bmiz: CDC --for-age z-scores for Weight, Height, and BMI. BMIz is based on 2000 CDC growth charts (non-obese children) and extended BMIz (obese children)

mod_waz, mod_haz, mod_bmiz: modified z-scores

ext_bmip and ext_bmiz: extended BMI percentile and z-score.  See note to BMIz

pre_2022_bmiz and pre_2022_bmip:  orignal calculations of BMIz and BMI percentile

bmip95: BMI expressed as percentage of 95th percentile, 120 percent is lower threshold for severe obeseity

if 'all = TRUE', then output other BMI metrics describe in Wei et al. paper.  Default is FALSE.  These express BMI as distance or percent distance from the median.  If percent of the median is desired, 100 can be added to the values.

}
\references{
Kuczmarski RJ, Ogden CL, Guo SS, Grummer-Strawn LM, Flegal KM, Mei Z, et al. 2000 CDC Growth Charts for the United States: methods and development. Vital and Health Statistics Series 11, Data from the National Health Survey 2002;11:1–190.

Wei R, Ogden CL, Parsons VL, Freedman DS, Hales CM. A method for calculating BMI z-scores and percentiles above the 95th percentile of the CDC growth charts. Annals of Human Biology 2020;47:514–21.

Freedman DS, Woo JG, Ogden CL, Xu JH, Cole TJ. Distance and Percent Distance from Median BMI as Alternatives to BMI z-score. Br J Nutr 2019;124:1–8.


}
\author{
David Freedman
}
\note{
Do NOT put arguments in quotation marks, such as cdcanthro(data,'age','wt','ht','bmi').  Use: cdcanthro(data, age, wt, ht, bmi)

Reference data for the LMS and extended BMI data files at

\url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}

\url{https://www.cdc.gov/growthcharts/extended-bmi-data-files.htm}
}


\examples{
library(data.table)
data <- data.table(sex=1:2, agem=120.5, wtk=c(30,60), htc=c(135,144))
data[, bmi := wtk / (htc/100)^2]
out <- cdcanthro(data, age=agem, wt=wtk, ht=htc, bmi)

# OR data = cdcanthro(data, agem, wtk, htc, bmi);
round(out,2)[1:5]
# setDF(out) to convert to a dataframe

# results with 'all=TRUE'
out = cdcanthro(data, age=agem, wt=wtk, ht=htc, bmi, all=TRUE);
round(out,2)[1:5]

# another example, BMI is not in dataset
d <- data.table(sex=c(1,2,1,2,2), age=c(141,54,217,155,52),
                wt=c(57,25,72,72,17.7), ht=c(143,102,166,169,105)
)
d[,':=' (age=age+0.5)] # age was give as completed months
d
d <- cdcanthro(d,age,wt,ht) # if BMI is not given, it's based on wt and ht (cm)
round(d,2)

# 2015/16 and 2017/18 NHANES data
NHanes
NHanes[,agemos:=agemos+0.5] # because age is completed number of months
NHanes = cdcanthro(NHanes,agemos,wt,ht,bmi)
round(NHanes,2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
