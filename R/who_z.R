# whoanthro: WHO growth-standard z-scores for young children (0 to 5 y, 1826 days)
#
# Computes WHO Child Growth Standard z-scores, plus biologically-implausible-
# value (BIV) flags, for weight-for-age, length/height-for-age, BMI-for-age,
# head-circumference-for-age, and weight-for-length.
#
# lenhei is recumbent length up to age 2.0 y (<= 730 d) and standing height for
# > 2 y (>= 731 d). The length- and height-based intervals do not overlap, so
# there is a deliberate jump in M between 730 and 731 days.
# Weight-for-length is 'for length' and does not go over 110 cm.
#
# sex can be coded 1/2, BOYS/GIRLS, b/g, m/f (case-insensitive).
# wt in kg, length/height (lenhei) in cm, age in DAYS.
#
# 'BIV' flags (match the CRAN anthro package): f* = 1 if the z-score is outside the
# WHO plausibility range, 0 if inside, NA if the z-score is NA.
#   fwaz    : -6 <= waz    <= 5
#   flhaz   : -6 <= lhaz   <= 6
#   fwflz   : -5 <= wflz   <= 5
#   fbmiz   : -5 <= bmiz   <= 5
#   fheadcz : -5 <= headcz <= 5
#
# ROW ALIGNMENT
# -------------
# No rows are ever dropped and nothing is re-sorted, so results stay positionally
# aligned with the input from start to finish. The reference joins use
# nomatch = NA, which returns exactly one row per input row, in input order:
# children outside the reference range simply get NA LMS values and therefore NA
# z-scores. Because of this there is no row-id column, no copy of the input, and
# no join at the end -- the computed columns are written straight onto the
# working copy with set().
#
# INTERNAL WORKING COLUMNS
# -------------------------
# All standardized-name columns (agedays, wt, lenhei, headc, bmi, sex_n, len_2)
# used for the reference joins live in a private `work` table built fresh from
# the caller's data -- `data` itself is never written to until the very end,
# when only brand-new result columns (who_waz, fwaz, ...) are added. This
# means there is no "track what we added, then delete it" step, and no way
# for an internal column name to collide with (and silently duplicate) one of
# the caller's own columns.


# ---- helpers ----------------------------------------------------------------

calc_who_z <- function(var, l, m, s, correct = FALSE) {
  z <- ((var / m)^l - 1) / (l * s)

  if (correct) {
    idx_hi <- which(z > 3)
    idx_lo <- which(z < -3)

    if (length(idx_hi) > 0) {
      inv_l <- 1 / l[idx_hi]
      sdp2 <- m[idx_hi] * (1 + 2 * l[idx_hi] * s[idx_hi])^inv_l
      sdp3 <- m[idx_hi] * (1 + 3 * l[idx_hi] * s[idx_hi])^inv_l
      z[idx_hi] <- 3 + (var[idx_hi] - sdp3) / (sdp3 - sdp2)
    }

    if (length(idx_lo) > 0) {
      inv_l <- 1 / l[idx_lo]
      sdm2 <- m[idx_lo] * (1 - 2 * l[idx_lo] * s[idx_lo])^inv_l
      sdm3 <- m[idx_lo] * (1 - 3 * l[idx_lo] * s[idx_lo])^inv_l
      z[idx_lo] <- -3 - (var[idx_lo] - sdm3) / (sdm3 - sdm2)
    }
  }
  return(z)
}


who_z <- function(data,
               agedays = agedays,
               wt      = wt,
               lenhei  = lenhei,
               bmi     = bmi,
               headc   = headc,
               sex_code = 1:2
               )
{
  # Catch a missing or malformed 'data' argument early
  if (missing(data))
    stop("Argument 'data' is required.")
  if (!is.data.frame(data))
    stop("'data' must be a data.frame, data.table, or tibble, not '",
         class(data)[1], "'.")

   # sex_code = c(male_code, female_code) tells normalize_sex() how a numeric
   # (or numeric-looking) sex column is coded, for datasets that don't use
   # the package default of 1 = male, 2 = female (e.g. sex_code = 0:1 for a
   # 0/1-coded column). validate_sex_code() lives in utils-anthro.R and is
   # checked again inside normalize_sex() itself -- called here too so a
   # malformed sex_code fails immediately, before any other work, matching
   # cdc_z()'s handling of the same argument.
   validate_sex_code(sex_code)

  # Work on a data.table copy so we never modify the caller's object by
  # reference -- see as_data_table_safe() in utils-anthro.R
   type <- as_data_table_safe(data)
   data <- type$data

   sex_var <- resolve_sex_column(data)

   # ---- resolve the bare column-name arguments to strings, once, up front ----
   # Each resolve_optional_var() call captures the NAME of a column (the
   # caller's substitute()d expression), checks whether that column actually
   # exists, and warns immediately if a supplied argument looks like a typo.
   agedays_var <- resolve_col_name(substitute(agedays))
   wt_arg      <- resolve_optional_var(data, substitute(wt),     missing(wt),     "wt")
   lenhei_arg  <- resolve_optional_var(data, substitute(lenhei), missing(lenhei), "lenhei")
   headc_arg   <- resolve_optional_var(data, substitute(headc),  missing(headc),  "headc")
   bmi_arg     <- resolve_optional_var(data, substitute(bmi),    missing(bmi),    "bmi")

   # The same column named for two measurements is almost always a positional
   # slip, e.g. who_z(d, agedays, lenhei) puts the length column into 'wt'.
   used <- c(wt = wt_arg$var, lenhei = lenhei_arg$var,
             headc = headc_arg$var, bmi = bmi_arg$var)
   dup  <- used[duplicated(used) | duplicated(used, fromLast = TRUE)]
   if (length(dup))
     stop("Column '", dup[[1]], "' is used for more than one of: ",
          paste(names(dup), collapse = ", "),
          ". Name the arguments, e.g. who_z(data, agedays = ..., lenhei = ...).")

   has_wt     <- wt_arg$has
   has_lenhei <- lenhei_arg$has
   has_headc  <- headc_arg$has
   has_bmi    <- bmi_arg$has
   have_bmi   <- has_bmi || (has_wt && has_lenhei)

   # Collect every "variable not supplied" issue into one vector instead of
   # calling warning() separately for each -- a call with wt, lenhei, and bmi
   # all missing used to raise three separate warnings; now it raises one.

   if (!agedays_var %in% names(data))
      stop("Age variable '", agedays_var, "' not found in data.")
   av <- data[[agedays_var]]
   if (!is.numeric(av))
      stop("Age variable '", agedays_var, "' must be numeric (age in days).")

   if (any(abs(av - round(av)) > 1e-8, na.rm = TRUE)) {
     warning("Non-integer agedays found; rounded to whole days.")
     av <- round(av)
   }

   if (!any(has_wt, has_lenhei, has_headc, has_bmi))
     stop("No measurement columns found. Supply at least one of wt, lenhei, headc, bmi.")

   # check for non-numeric variables
   bad <- character(0)
   if (has_wt && !is_num_or_all_na(data[[wt_arg$var]]))
     bad <- c(bad, sprintf("weight variable '%s' (kg)", wt_arg$var))
   if (has_lenhei && !is_num_or_all_na(data[[lenhei_arg$var]]))
     bad <- c(bad, sprintf("length/height variable '%s' (cm)", lenhei_arg$var))
   if (has_headc && !is_num_or_all_na(data[[headc_arg$var]]))
     bad <- c(bad, sprintf("head circumference variable '%s' (cm)", headc_arg$var))
   if (has_bmi && !is_num_or_all_na(data[[bmi_arg$var]]))
     bad <- c(bad, sprintf("BMI variable '%s'", bmi_arg$var))

   if (length(bad) > 0)
     stop("The following must be numeric: ", paste(bad, collapse = "; "), ".")

   # ---- lean working table -----------------------------------------------
   # Built fresh under standardized names, from the caller's resolved columns
   # directly -- `data` itself is not touched. Every row is kept, in the
   # original order, so the results below line up with `data` position for
   # position.
   work <- data.table(agedays = av,
                      sex_n = normalize_sex(data[[sex_var]], sex_code = sex_code))

   if (nrow(work) > 0L && all(is.na(work$sex_n)))
      stop("No valid sex values found. Expected M/F, ",
           sex_code[1], "/", sex_code[2], ", B/G, or lower-case.")

   if (has_wt)     work[, wt     := num_col(data[[wt_arg$var]])]
   if (has_lenhei) work[, lenhei := num_col(data[[lenhei_arg$var]])]
   if (has_headc)  work[, headc  := num_col(data[[headc_arg$var]])]

   # code 0 and negative values to NA - see utils
   set_nonpositive_na(work, c("wt", "lenhei", "headc"))

   if (has_bmi) {
     work[, bmi := num_col(data[[bmi_arg$var]])]
   } else if (has_wt && has_lenhei) {
      work[, bmi := (wt * 10000) / (lenhei * lenhei)]
   }

   # length is used for weight-for-length, which applies only to ages <= 730 d;
   # older children get NA here and so find no match in the WFL reference.
   if (has_lenhei) {
     work[, len_2 := fifelse(agedays <= 730L, as.double(lenhei), NA_real_)]
   }

   set_nonpositive_na(work, c("bmi"))

   # ---- age-based indicators (waz, lhaz, bmiz, headcz) -----------------------
   # Only 'forage' rows can match on agedays, and we carry just the LMS columns
   # we actually use -- anything else would be duplicated across every data row.
   lmscols <- c('wei_l', 'wei_m', 'wei_s', 'len_l', 'len_m', 'len_s',
                'bmi_l', 'bmi_m', 'bmi_s', 'headc_l', 'headc_m', 'headc_s')
   lmscols <- intersect(lmscols, names(cdcanthro::who_ref_data))

   # if (!is.data.table(cdcanthro::who_ref_data)) {
   #   who_ref_data <- as.data.table(who_ref_data)
   # }
   dref1 <- cdcanthro::who_ref_data[denom=='forage' & agedays <= 1826,
                                c('sexn', 'agedays', lmscols), with = FALSE]
   # dref1's reference column is 'sexn' (from who_ref_data); work's is 'sex_n'.
   # nomatch = NA (the default) keeps every row of `work`, in `work`'s order:
   # ages over 1826 days, and unrecognized sex, get NA LMS values and thus NA
   # z-scores. This is what makes the positional assignment below valid, and it
   # assumes who_ref_data has at most one row per (sexn, agedays).
   dt1 <- dref1[work, on = c(sexn = 'sex_n', 'agedays')]

   # Calculate z-scores
   if (has_wt){
      dt1[, waz := calc_who_z(wt, wei_l, wei_m, wei_s, correct = TRUE)]
   }

   # lhaz length/height for age z-score  (correct = FALSE)
   if (has_lenhei){
      dt1[, lhaz := calc_who_z(lenhei, len_l, len_m, len_s, correct = FALSE)]
   }

   # bmiz  (weight-based -> correct = TRUE)
   if (have_bmi){
      dt1[, bmiz := calc_who_z(bmi, bmi_l, bmi_m, bmi_s, correct = TRUE)]
   }

   # head circumference z-score  (correct = FALSE)
   if (has_headc){
      dt1[, headcz := calc_who_z(headc, headc_l, headc_m, headc_s, correct = FALSE)]
   }

   # ---- weight-for-length (with reference interpolation) ---------------------
   # who_ref_data has length to 2 digits, so probably can delete the approx
   # and just round(len,2) before merging

   if (has_wt && has_lenhei) {
      dref2 <- cdcanthro::who_ref_data[denom=='forlen' & len >= 45 & len <= 110,
                            .(sexn,len,wfl_l,wfl_m,wfl_s)]

      # get ulens in 'work'
      ulens <- unique(work$len_2[!is.na(work$len_2)])
      needs_interp <- any(!ulens %in% dref2$len)

      # --- REFERENCE CHART INTERPOLATION ---
      # If the input contains lengths not mapped directly inside the standard WHO
      # tables, we execute linear interpolation via stats::approx across
      # LMS parameters to generate custom growth metrics matching the user's data

      if (needs_interp) {
        setkey(dref2, sexn, len)

      dref2 <- dref2[, {
        list(
          len   = ulens,
          wfl_l = stats::approx(len, wfl_l, xout = ulens, rule = 1)$y,
          wfl_m = stats::approx(len, wfl_m, xout = ulens, rule = 1)$y,
          wfl_s = stats::approx(len, wfl_s, xout = ulens, rule = 1)$y
        )
      }, by = sexn]
      }

      # i.wt reaches into work (the i-table) for the weight column —
      # dref2 has no wt column, but the prefix documents
      # where the value comes from.
      # dref2's reference columns are 'sexn'/'len'; work's are 'sex_n'/'len_2'.
     set(dt1, j = 'wflz',
         value = dref2[work, on = c(sexn = 'sex_n', len = 'len_2'),
                       calc_who_z(i.wt, wfl_l, wfl_m, wfl_s, correct = TRUE)])
     # dt1[agedays > 730, wflz := NA_real_]   # WFL applies only to length (<= 2 y)
  }


   # ---- write the results onto `data` ----------------------------------------
   # dt1 has exactly nrow(data) rows in the same order, so each column is copied
   # across by position with set() -- no row id, no join, no second copy of the
   # input. BIV flags come out NA wherever the z-score is NA.
   zs <- intersect(c('waz', 'lhaz', 'wflz', 'bmiz', 'headcz'), names(dt1))

   # BIV plausibility range for each z-score: a value outside [lo, hi] is flagged.
   # Ranges match the CRAN anthro package; note waz is asymmetric.
   biv <- list(waz    = c(lo = -6, hi = 5),
               lhaz   = c(lo = -6, hi = 6),
               wflz   = c(lo = -5, hi = 5),
               bmiz   = c(lo = -5, hi = 5),
               headcz = c(lo = -5, hi = 5))
   fnm <- c(waz = 'fwaz', lhaz = 'flhaz', wflz = 'fwflz',
            bmiz = 'fbmiz', headcz = 'fheadcz')

   # Every column this call is about to add: the who_-prefixed z-scores, their
   # (unprefixed) BIV flags, and -- when bmi was derived rather than supplied
   # -- 'bmi' itself. Collected up front so overlap with the caller's own
   # columns can be checked once, the same way cdc_z() does it.
   out_cols <- c(paste0('who_', zs), unname(fnm[zs]))
   if (!has_bmi && have_bmi) out_cols <- c(out_cols, 'bmi')

   # Rename any of data's own columns that would collide with a computed
   # output column. New names are made unique against every other input
   # column and every output column, so an existing 'fwaz0' can't collide.
   overlap <- intersect(names(data), out_cols)
   if (length(overlap) > 0) {
     new_nm <- utils::tail(make.unique(c(setdiff(names(data), overlap), out_cols,
                                  paste0(overlap, "0"))),
                    length(overlap))
     setnames(data, overlap, new_nm)
     warning(paste(overlap, collapse = ", "),
             " in original data renamed to ",
             paste(new_nm, collapse = ", "),
             " to avoid conflict with computed values.")
   }

   stopifnot(nrow(dt1) == nrow(data))
   for (z in zs) {
      v <- dt1[[z]]
      # z-scores are prefixed 'who_' so they never collide with cdcanthro()'s
      set(data, j = paste0('who_', z), value = v)
      set(data, j = fnm[[z]],
          value = as.integer(!v %between% c(biv[[z]]['lo'], biv[[z]]['hi'])))
   }

   if (!has_bmi && have_bmi) {
      set(data, j = "bmi", value = dt1$bmi)
   }

   # return same type of data that was input
   restore_input_type(data, type$input_was_df, type$input_was_tibble)
}
