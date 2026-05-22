
# Prevent R CMD check notes for data.table variables
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(
    "seq_", "sex", "_AGEMOS1", "denom", "agemos2", "lwt2", "mwt2", "swt2", 
    "lbmi2", "mbmi2", "sbmi2", "lht2", "mht2", "sht2", "agemos1", "lwt1", 
    "mwt1", "swt1", "lbmi1", "mbmi1", "sbmi1", "lht1", "mht1", "sht1", 
    "wl", "wm", "ws", "bl", "bm", "bs", "hl", "hm", "hs", "mref", 
    "..v", "bmi_l", "bmi_m", "waz", "haz", "orig_bmiz", "orig_bmip", 
    "bmiz", "p50", "bmip95", "perc_median", "mod_bmiz", "wap", "mod_waz", 
    "hap", "mod_haz", ".", "sexn", "age", "wt", "ht", "bmi", "L", "M", "S", 
    "p95", "bz", "bmip", "z1", "bmi_s", "sref", "z0", "sigma", "ebz", "ebp", 
    "agey", "cdc__ref__data"
  ))
}


# Internal helper functions

# cc(): captures unevaluated args as a character vector: cc(a, b) -> c("a", "b")
cc <- function(...) as.character(sys.call()[-1])

grepv <- function (..., ignore.case=TRUE, value=TRUE){
  base::grep(..., ignore.case=ignore.case, value=value)
}

set_cols_first <- function (DT, cols, intersection = TRUE) {
  if (intersection) {
    return(data.table::setcolorder(DT, c(intersect(cols, names(DT)),
                                         setdiff(names(DT), cols))))
  } else {
    return(data.table::setcolorder(DT, c(cols, setdiff(names(DT), cols))))
  }
}

# cz_score(): returns standard LMS z-score (z) and the "modified" z-score (mz)
# used by CDC to flag biologically implausible values. mz uses half the
# distance between the median and the +/-2 SD point as the scaling unit,
# which avoids the inflation of |z| that occurs at extreme tails when L is
# far from 1. Named list elements make the := assignment at the call site
# robust to accidental reordering during future edits.
cz_score <- function(var, l, m, s){
  ls   = l * s; invl = 1 / l
  z    = (((var / m) ^ l) - 1) / ls
  sdp2 = (m * (1 + 2 * ls) ^ invl) - m
  sdm2 = m - (m * (1 - 2 * ls) ^ invl)
  mz   = data.table::fifelse(var < m,
                             (var - m) / (0.5 * sdm2),
                             (var - m) / (0.5 * sdp2))
  list(z = z, mz = mz)
}

# -------------------------------------------------------------------------
# Pre-computed qnorm constants at package level.
# Computing them once avoids repeated calls to qnorm() on every invocation of cdcanthro().
# -------------------------------------------------------------------------
.qn50 <- qnorm(0.50)
.qn85 <- qnorm(0.85)
.qn95 <- qnorm(0.95)

# lms_pct: moved to package level (was recreated inside cdcanthro() on every
# call). qnorm() is not called inside the function.
lms_pct <- function(m, l, s, qn) m * (1 + l * s * qn) ^ (1 / l)

cdcanthro <- function(data, age, wt, ht, bmi, all = FALSE) {
  .datatable.aware <- TRUE
  
  input_class      <- class(data)[1]
  input_was_tibble <- input_class == "tbl_df"
  input_was_df     <- input_class == "data.frame"
  
  data <- as.data.table(data)
  
  # seq_ is used as a row-id to recover the original row order after the
  # final join. If the caller already has a column named 'seq_' we'd silently
  # overwrite it, so guard against that.
  if ("seq_" %in% names(data)) {
    stop("Input data must not contain a column named 'seq_' (it is used ",
         "internally to preserve row order). Please rename it before calling ",
         "cdcanthro().")
  }
  data[, seq_ := seq_len(.N)]
  dorig <- copy(data)
  
  nms <- grepv('^sex$|^SEX$',names(data))
  if (length(nms) != 1) {
    stop ("A child's sex MUST be named 'sex' or 'SEX'.
             But your data can't contain both 'sex' and 'SEX'.")
  }
  if (nms != 'sex') names(data)[names(data) == nms] <- 'sex'
  
  
  # Use substitute to capture column names
  age_var <- deparse(substitute(age))
  wt_var  <- if (!missing(wt))  deparse(substitute(wt))  else ""
  ht_var  <- if (!missing(ht))  deparse(substitute(ht))  else ""
  bmi_var <- if (!missing(bmi)) deparse(substitute(bmi)) else ""
  
  if (!age_var %in% names(data)) stop("Age variable not found in data")
  has_wt  <- !missing(wt)  && wt_var  %in% names(data)
  has_ht  <- !missing(ht)  && ht_var  %in% names(data)
  has_bmi <- !missing(bmi) && bmi_var %in% names(data)
  
  if (!has_wt && !has_ht && !has_bmi)
    stop("At least one of weight, height, or BMI must be present in data.")
  
  data[, age := data[[age_var]]]
  data[, wt  := if (has_wt) data[[wt_var]] else NA_real_]
  data[, ht  := if (has_ht) data[[ht_var]] else NA_real_]
  
  # If all ages are whole numbers, add 0.5 months to each age.
  # If any fractional ages are present the data are assumed to
  # reflect 'exact' measurements and no adjustment is made.
  # na.rm = TRUE: ages can legitimately be NA in real datasets; without
  # it, all() returns NA and the if() would error.
  if (isTRUE(all(data$age == floor(data$age), na.rm = TRUE))) {
    message("All ages are integers so 0.5 months has been added to each age for the calculations. 
     This is consistent with the CDC/NHANES convention of treating a reported age of, 
     say, 36 months as representing the interval [36, 37), so 36.5 is used as the midpoint.")
    data$age <- data$age + 0.5
  }
  
  if (has_bmi) {
    data$bmi <- data[[bmi_var]]
  } else if (has_wt && has_ht) {
    data[, bmi := wt / (ht / 100)^2]
  } else {
    data[, bmi := NA_real_]
  }
  have_bmi <- has_bmi || (has_wt && has_ht)
  
  n_na_wt  <- if (has_wt)  sum(is.na(data$wt))  else 0L
  n_na_ht  <- if (has_ht)  sum(is.na(data$ht))  else 0L
  n_na_bmi <- sum(is.na(data$bmi))
  
  if (!has_wt)
    warning("Weight variable not found: WAZ etc will be NA.")
  if (!has_ht)
    warning("Height variable not found: HAZ etc will be NA.")
  if (!have_bmi)
    warning("BMI was not present and can't be calculated: BMI metrics will be NA.")
  if (!has_wt && !has_ht && !have_bmi)
    warning("No anthropometric variables found: no metrics will be calculated.")
  if (n_na_wt  > 0) warning(n_na_wt,  " row(s) with missing weight: WAZ will be NA for those rows.")
  if (n_na_ht  > 0) warning(n_na_ht,  " row(s) with missing height: HAZ will be NA for those rows.")
  if (n_na_bmi > 0) warning(n_na_bmi, " row(s) with missing BMI: BMI calculations will be NA for those rows.")
  
  data[,sexn:=toupper(substr(sex,1,1))]
  data[,sexn:=fcase(
    sexn %in% c(1,'B', 'M'), 1L,
    sexn %in% c(2,'G', 'F'), 2L
  )]
  if (all(is.na(data$sexn))) 
    stop("No valid sex values found. Expected M/F, 1/2, B/G, or lower-case.")
  
  # Restrict to the documented valid age range. Rows outside this range
  # are dropped from 'data' but kept in 'dorig', so they reappear in the
  # final output with NA z-scores after the join.
  data <- data[age >= 24 & age < 240, .(seq_, sexn, age, wt, ht, bmi)]
  
  # Defensive copy. cdc__ref__data is a package dataset loaded from /data and
  # is therefore shared across calls in the same R session. Want to be certain no write leaks back to
  # the package data. copy() is cheap on a reference table this small.
  cdc_ref <- copy(cdc__ref__data[`_AGEMOS1` > 23 & denom == 'age'])
  
  setnames(cdc_ref, gsub('^_', '', tolower(names(cdc_ref))))
  setnames(cdc_ref, 'sex', 'sexn')
  
  # values at 240.0 months: https://www.cdc.gov/growthcharts/percentile_data_files.htm
  d20 <- cdc_ref[agemos2==240,
                 .(sexn,agemos2,lwt2,mwt2,swt2,lbmi2,mbmi2,sbmi2,lht2,mht2,sht2)]
  names(d20) <- gsub('2','',names(d20));
  
  cdc_ref <- cdc_ref[,.(sexn,agemos1,lwt1,mwt1,swt1,lbmi1,mbmi1,sbmi1,lht1,mht1,sht1)]
  names(cdc_ref) <- gsub('1','',names(cdc_ref));
  
  cdc_ref=rbindlist(list(cdc_ref,d20))
  cdc_ref[sexn==1, ':=' (mref=23.02029, sref=0.13454)] # checked on 7/9/22
  cdc_ref[sexn==2, ':=' (mref=21.71700, sref=0.15297)]
  # Values are from Table 3, https://doi.org/10.1080/03014460.2020.1808065
  
  v=cc(sexn,age,wl,wm,ws,bl,bm,bs,hl,hm,hs,mref,sref);
  setnames(cdc_ref,v)
  
  # interpolate reference data to match each age_month in input data
  uages <- unique(data$age)
  
  # If dlen == 0, all input ages are exact matches and no interpolation is needed.
  dlen  <- length(setdiff(uages, cdc_ref$age))
  
  db <- cdc_ref[sexn == 1]
  dg <- cdc_ref[sexn == 2]
  
  # Interpolation functions: for a given reference column vector (col),
  # return linearly interpolated values at each unique input age (uages).
  # Note: lapply() below applies these to every column, including sexn and
  # age themselves. That's harmless -- interpolating a constant (sexn) gives
  # back the constant, and interpolating age against age gives back uages --
  # and keeps the code concise.
  fboys  <- function(col) approx(db$age, col, xout = uages)$y
  fgirls <- function(col) approx(dg$age, col, xout = uages)$y
  
  if (dlen > 0) {
    # At least one input age is not an exact reference age, so interpolate.
    db <- setDT(lapply(db, fboys))
    dg <- setDT(lapply(dg, fgirls))
    cdc_ref <- rbindlist(list(db, dg))
  }
  
  setkey(data,    sexn, age)
  setkey(cdc_ref, sexn, age)
  # Restrict cdc_ref to only those (sexn, age) combinations that actually
  # appear in the input data, then right-join data onto it. Result row count
  # equals nrow(data); rows whose sexn is NA  will be dropped from dt, 
  # but they survive in dorig and reappear with NA z-scores after the dorig[dt] join.
  cdc_ref <- cdc_ref[unique(data[, .(sexn, age)])]
  dt <- cdc_ref[data]
  
  if (has_wt) {
    dt[, c('waz', 'mod_waz') := cz_score(wt,  wl, wm, ws)]
  } else {
    dt[, c('waz', 'mod_waz') := NA_real_]
  }
  
  if (has_ht) {
    dt[, c('haz', 'mod_haz') := cz_score(ht,  hl, hm, hs)]
  } else {
    dt[, c('haz', 'mod_haz') := NA_real_]
  }
  
  if (have_bmi) {
    dt[, c('bz', 'mod_bmiz') := cz_score(bmi, bl, bm, bs)]
  } else {
    dt[, c('bz', 'mod_bmiz') := NA_real_]
  }

  setnames(dt,cc(bl,bm,bs),cc(bmi_l,bmi_m,bmi_s))
  dt[,c('wl','wm','ws','hl','hm','hs'):=NULL]
  
  dt[,':=' (
    bmip=100*pnorm(bz),
    p50 = lms_pct(bmi_m, bmi_l, bmi_s, .qn50),
    p85 = lms_pct(bmi_m, bmi_l, bmi_s, .qn85),
    p95 = lms_pct(bmi_m, bmi_l, bmi_s, .qn95),
    wap=100*pnorm(waz),  hap=100*pnorm(haz),
    
    # other BMI metrics -- PMID 31439056
    z1=((bmi/bmi_m) - 1) / bmi_s,  # LMS formula when L=1: ((BMI/M)-1)/S
    z0 = log(bmi/bmi_m) / bmi_s # LMS transformation with L=0, note these end in '0'
  )
  ][,':=' (
    # second pass: derived distance-from-median metrics
    dist_median         = z1 * bmi_m * bmi_s, # un-adjusted distance from median with L=1
    adj_dist_median     = z1 * sref * mref, # adjusted (to age 20.0 y) dist from median
    perc_median         = z1 * 100 * bmi_s, # un-adjusted % from median
    adj_perc_median     = z1 * 100*sref, # adjusted % from median
    log_perc_median     = z0 * 100 * bmi_s, # un-adjusted % from median with L=0 (log scale)
    adj_log_perc_median = z0 * 100* sref,  # adjusted % from median w L=0 (log scale)
    bmip95              =100*(bmi/p95)
  )]
  
  ## now create Extended z-score for BMI >=95th P
  dt[,':=' (ebz=bz, ebp=bmip, agey=age/12)]
  dt[, sigma:=data.table::fifelse(sexn==1, 0.3728 + 0.5196*agey - 0.0091*agey^2,
                                  0.8334 + 0.3712*agey - 0.0011*agey^2)]
  # Table 3, DOI:10.1080/03014460.2020.1808065
  
  dt[bmip >= 95, ebp := 90 + 10 * pnorm((bmi - p95) / round(sigma, 8))]
  # Cap extended z at 8.21: the largest achievable possible ebz
  # is ~8.20945. The < 1 guard protects against the floating-point edge case where ebp/100 rounds to
  # exactly 1, which would give qnorm(1) = Inf.
  dt[bmip >= 95, ebz := fifelse(ebp / 100 < 1, qnorm(ebp / 100), 8.21)]
  # sigma rounded to 8 to agree with NCHS
  # https://www.cdc.gov/growthcharts/extended-bmi-data-files.htm
  
  x <- cc(agey,mref,sref,sexn);
  dt[,(x):=NULL]
  
  setnames(dt,
           cc(bz,        bmip,      ebp,  ebz),
           cc(orig_bmiz, orig_bmip, bmip, bmiz)
  )
  
  v <- cc(seq_, bmiz, bmip, p50, p95, bmip95, orig_bmip, orig_bmiz, perc_median,
           mod_bmiz, waz, wap, mod_waz, haz, hap, mod_haz)
  if (!has_bmi & have_bmi) v <- c('bmi', v) 
  
  if (isTRUE(all)) {
    v=c(v, 'bmi_l', 'bmi_m', 'bmi_s',  'sigma', 'adj_dist_median', 'dist_median',
        'adj_perc_median', 'log_perc_median', 'adj_log_perc_median')
  }
  
  dt <- dt[,..v]
  
  setkey(dt,seq_); setkey(dorig,seq_)
  dtot <- dorig[dt]
  set_cols_first(dtot,names(dorig))
  dtot[,seq_:=NULL]
  
  if (input_was_df) return(as.data.frame(dtot))
  if (input_was_tibble) {
    if (requireNamespace("tibble", quietly = TRUE)) {
      return(tibble::as_tibble(dtot))
    } else {
      warning("tibble package not installed; returning a data.frame instead.")
      return(as.data.frame(dtot))
    }
  }
  return(dtot[])
}