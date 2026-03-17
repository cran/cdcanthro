#' @import data.table
#' @importFrom stats pnorm qnorm approx
NULL

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

# Prevent R CMD check notes for data.table variables
# if(getRversion() >= "2.15.1")  {
#   utils::globalVariables(c(".", "sexn", "age", "wt", "ht", "bmi", "L", "M", "S", 
#                            "p95", "bz", "bmip", "z1", "bmi_s", "sref", "z0", 
#                            "sigma", "ebz", "ebp", "agey", "cdc__ref__data"))
# }

# Internal helper functions
cc <- function (...) as.character(sys.call()[-1])

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

cz_score <- function(var, l, m, s){ 
      ls=l*s; invl=1/l
      z = (((var/m) ^ l) -1) / (ls)
      sdp2 = (m * (1 + 2*ls) ^ (invl)) - m
      sdm2 = m - (m * (1 - 2*ls) ^ (invl))
      mz = data.table::fifelse(var < m, (var - m)/(0.5*sdm2), (var - m)/(sdp2*0.5) )
      list(z, mz)
}

#' @export
cdcanthro <- function(data, age, wt, ht, bmi, all = FALSE) {
 
   dt <- data.table::as.data.table(data)
   data <- data.table::copy(dt) 
   .datatable.aware <- TRUE
   
   data[, seq_ := seq_len(.N)]
   dorig <- copy(data)

   nms <- grepv('^sex$',names(data))
   if (length(nms) != 1) {
      stop ("A child's sex MUST be named 'sex'; this is case insensitive.
             Also, you cannot have both 'sex' and 'SEX' as variables in your data.")
   }
   if (nms!='sex') {names(data)[which(names(data)==nms)] <- 'sex'}

   # Use substitute to capture column names
   data$age <- data[[deparse(substitute(age))]]
   data$wt <- data[[deparse(substitute(wt))]]
   data$ht <- data[[deparse(substitute(ht))]]

   # data$bmi <- data[[deparse(substitute(bmi))]]
   # data[is.na(bmi) & !is.na(wt+ht),bmi:=wt/(ht/100)^2] # kg and cm

   # change next line to replace 'bmi' if it's missing: 9/27/24
   b <- grepv('bmi',names(data))
   if (length(b)==1){
      data$bmi <- data[[deparse(substitute(bmi))]]
   } else {
      data[,bmi:=wt/(ht/100)^2] # wt is in kg, ht in cm
   }

   if (('age' %in% names(data)) == FALSE){
      stop('There must be an variable for age in months in the data')
   }

   data[,sexn:=toupper(substr(sex,1,1))]
   data[,sexn:=fcase(
      sexn %in% c(1,'B','M'), 1L,
      sexn %in% c(2,'G','F'), 2L
   )]

   data <- data[between(age,24,239.9999), #  & !(is.na(wt) & is.na(ht)),
                    .(seq_, sexn,age,wt,ht,bmi)];

   # cdc__ref__data <- fread(p0(.anal,'Growth_Charts/Data/cdc_ref_data.csv'));
   cdc_ref <- cdc__ref__data[`_AGEMOS1`>23 & denom=='age'] # if in /data

   # NHanes <- get0("NHanes", envir = asNamespace("cdcanthro")) #sysdata.rda
   # cdc_ref <- get0("cdc__ref__data", envir = asNamespace("cdcanthro")) #sysdata.rda
   # https://stackoverflow.com/questions/32964741/accessing-sysdata-rda-within-package-functions
   # as.data.table(cdc_ref)

   setnames(cdc_ref, tolower(names(cdc_ref)))
   setnames(cdc_ref, gsub('^_', '', names(cdc_ref)))
   setnames(cdc_ref,'sex','sexn')

   # values at 240.0 months: https://www.cdc.gov/growthcharts/percentile_data_files.htm
   d20 <- cdc_ref[agemos2==240,
               .(sexn,agemos2,lwt2,mwt2,swt2,lbmi2,mbmi2,sbmi2,lht2,mht2,sht2)]
   names(d20) <- gsub('2','',names(d20));

   cdc_ref <- cdc_ref[,.(sexn,agemos1,lwt1,mwt1,swt1,lbmi1,mbmi1,sbmi1,lht1,mht1,sht1)]
   names(cdc_ref) <- gsub('1','',names(cdc_ref));

   cdc_ref=rbindlist(list(cdc_ref,d20))
   cdc_ref[sexn==1, ':=' (mref=23.02029, sref=0.13454)] # checked on 7/9/22
   cdc_ref[sexn==2, ':=' (mref=21.71700, sref=0.15297)]

   # v=c('sexn','age','wl','wm','ws','bl','bm','bs','hl','hm','hs','mref','sref');
   v=cc(sexn,age,wl,wm,ws,bl,bm,bs,hl,hm,hs,mref,sref);
   setnames(cdc_ref,v)

   # interpolate reference data to match each age_month in input data
   uages <- unique(data$age)
   dlen <- length(setdiff(data$age,cdc_ref$age))
   db <- cdc_ref[sexn==1]
     fboys <- function(v,...)approx(db$age,v,xout=uages)$y
   dg <- cdc_ref[sexn==2]
     fgirls <- function(v,...)approx(dg$age,v,xout=uages)$y

   if (dlen > 0) {
   if (length(uages) > 1) {
         db <- as.data.table(sapply(db[,..v],fboys))
         dg <- as.data.table(sapply(dg[,..v],fgirls))
   } else {
       if (length(uages)==1) {  # dataset has only 1 age
         db <- as.data.table(t(sapply(db[,..v],fboys)))
         dg <- as.data.table(t(sapply(dg[,..v],fgirls)))
       }
   }
   }
   cdc_ref <- rbindlist(list(db,dg))

   # cdc_ref <- reframe(cdc_ref,
   #    across(c(age,wl,wm,ws,bl,bm,bs,hl,hm,hs,mref,sref),
   #    \(v) approx(age, v, xout = uages)$y), .by = sexn) |>
   #    setDT()

   du <- unique(data[,.(sexn,age)])
   cdc_ref <- cdc_ref[du, on=c('sexn','age')]

   setkey(data,sexn,age); setkey(cdc_ref,sexn,age)
   dt <- cdc_ref[data];

   dt[,c('waz', 'mod_waz'):= cz_score(wt, wl, wm, ws)]
   dt[,c('haz', 'mod_haz'):= cz_score(ht, hl, hm, hs)]
   dt[,c('bz', 'mod_bmiz'):= cz_score(bmi, bl, bm, bs)]

   # as.data.table(dt);
   setnames(dt,cc(bl,bm,bs),cc(bmi_l,bmi_m,bmi_s))
   dt[,c('wl','wm','ws','hl','hm','hs'):=NULL]

   dt[,':=' (
      bmip=100*pnorm(bz),
      p50= bmi_m * (1 + bmi_l*bmi_s*qnorm(0.50))^(1 / bmi_l),
      p85= bmi_m * (1 + bmi_l*bmi_s*qnorm(0.85))^(1 / bmi_l),
      p95= bmi_m * (1 + bmi_l*bmi_s*qnorm(0.95))^(1 / bmi_l),
      p97= bmi_m * (1 + bmi_l*bmi_s*qnorm(0.97))^(1 / bmi_l),
      wap=100*pnorm(waz),  hap=100*pnorm(haz),

     # other BMI metrics -- PMID 31439056
      z1=((bmi/bmi_m) - 1) / bmi_s,  # LMS formula when L=1: ((BMI/M)-1)/S
      z0 = log(bmi/bmi_m) / bmi_s # LMS transformation with L=0, note these end in '0'
     )
     ][,':=' (
      dist_median = z1 * bmi_m * bmi_s, # un-adjusted distance from median with L=1
      adj_dist_median = z1 * sref * mref, # adjusted (to age 20.0 y) dist from median
      perc_median = z1 * 100 * bmi_s, # un-adjusted % from median
      adj_perc_median = z1 * 100*sref, # adjusted % from median
      log_perc_median = z0 * 100 * bmi_s, # un-adjusted % from median with L=0 (log scale)
      adj_log_perc_median = z0 * 100* sref,  # adjusted % from median w L=0 (log scale)
      bmip95=100*(bmi/p95)
   )]

   ## now create Extended z-score for BMI >=95th P
    dt[,':=' (ebz=bz, ebp=bmip, agey=age/12)]
    dt[, sigma:=data.table::fifelse(sexn==1, 0.3728 + 0.5196*agey - 0.0091*agey^2,
                               0.8334 + 0.3712*agey - 0.0011*agey^2)]
    dt[bmip>=95, ebp:=90 + 10*pnorm((bmi - p95) / round(sigma,8))]
    # sigma rounded to 8 to agree with NCHS, Craig Hales
    dt[bmip>=95 & ebp/100 < 1, ebz:=qnorm(ebp/100)]
    dt[ebp/100==1, ebz:=8.21] # highest possible value is 8.20945
    # The SAS program also uses 8.21 for this

   x <- cc(agey,mref,sref,sexn);
   dt[,(x):=NULL]

   setnames(dt,
      cc(bz,        bmip,      ebp,  ebz),
      cc(orig_bmiz, orig_bmip, bmip, bmiz)
   )

   v=cc(seq_, bmiz, bmip, p50, p95, bmip95, orig_bmip, orig_bmiz, perc_median,
       mod_bmiz, waz, wap, mod_waz, haz, hap, mod_haz)

   if(all == TRUE){
      v=c(v, 'bmi_l', 'bmi_m', 'bmi_s',  'sigma', 'adj_dist_median', 'dist_median',
          'adj_perc_median', 'log_perc_median', 'adj_log_perc_median')
   }

   dt <- dt[,..v]

   setkey(dt,seq_); setkey(dorig,seq_)
   dtot <- dorig[dt]
   set_cols_first(dtot,names(dorig))
   dtot[,seq_:=NULL]
   return(dtot[])
}

.datatable.aware <- TRUE

