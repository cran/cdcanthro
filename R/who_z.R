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
# the caller's own columns. See utils-anthro.R's snapshot_names() comment for
# why a track-and-delete approach is fragile with data.table in the first
# place -- an earlier version of this function relied on one, and because
# names(data) returns a *live* reference to a data.table's names vector
# rather than a copy, the "was this column already here" snapshot silently
# kept pace with every column added afterwards, so the cleanup step never
# actually removed anything.


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

# calc_who_z <- function(var, l, m, s, correct = FALSE) {
#   dt <- data.table(var = var, l = l, m = m, s = s)
#   dt[, z := ((var / m)^l - 1) / (l * s)]
#
#   if (correct) {
#     # Upper tail correction (> 3)
#     dt[z > 3, let(sdp2 = m * (1 + 2 * l * s)^(1 / l),
#                   sdp3 = m * (1 + 3 * l * s)^(1 / l))
#       ][z > 3, z := 3 + (var - sdp3) / (sdp3 - sdp2)]
#     # Lower tail correction (< -3)
#     dt[z < (-3), let(sdm2 = m * (1 - 2 * l * s)^(1 / l),
#                      sdm3 = m * (1 - 3 * l * s)^(1 / l))
#      ][z < (-3), z := -3 - (var - sdm3) / (sdm3 - sdm2)]
#   }
#   dt$z
# }


who_z <- function(data,
               agedays = agedays,
               wt      = wt,
               lenhei  = lenhei,
               bmi     = bmi,
               headc   = headc,
               sex_code = 1:2
               )
{

   # sex_code = c(male_code, female_code) tells normalize_sex() how a numeric
   # (or numeric-looking) sex column is coded, for datasets that don't use
   # the package default of 1 = male, 2 = female (e.g. sex_code = 0:1 for a
   # 0/1-coded column). validate_sex_code() lives in utils-anthro.R and is
   # checked again inside normalize_sex() itself -- called here too so a
   # malformed sex_code fails immediately, before any other work, matching
   # cdc_z()'s handling of the same argument.
   validate_sex_code(sex_code)

  # Work on a data.table copy so we never modify the caller's object by
  # reference (unless copy = FALSE was requested and data is already a
  # data.table -- see as_data_table_safe() in utils-anthro.R for the full
  # copy = TRUE/FALSE tradeoffs); type$data is that (copied or original)
  # table, type$input_was_df / $input_was_tibble record what to hand back
  # at the end.
   type <- as_data_table_safe(data)
   data <- type$data

   resolve_sex_column(data)

   # ---- resolve the bare column-name arguments to strings, once, up front ----
   # Each resolve_optional_var() call captures the NAME of a column (the
   # caller's substitute()d expression), checks whether that column actually
   # exists, and warns immediately if a supplied argument looks like a typo.
   # headc and bmi are optional -- when omitted, missing() is TRUE and the
   # resolved $var is NULL, which flows through to has_* = FALSE below.
   agedays_var <- resolve_col_name(substitute(agedays))
   wt_arg      <- resolve_optional_var(data, substitute(wt),     missing(wt),     "wt")
   lenhei_arg  <- resolve_optional_var(data, substitute(lenhei), missing(lenhei), "lenhei")
   headc_arg   <- resolve_optional_var(data, substitute(headc),  missing(headc),  "headc")
   bmi_arg     <- resolve_optional_var(data, substitute(bmi),    missing(bmi),    "bmi")

   has_wt     <- wt_arg$has
   has_lenhei <- lenhei_arg$has
   has_headc  <- headc_arg$has
   has_bmi    <- bmi_arg$has
   have_bmi   <- has_bmi || (has_wt && has_lenhei)

   # Collect every "variable not supplied" issue into one vector instead of
   # calling warning() separately for each -- a call with wt, lenhei, and bmi
   # all missing used to raise three separate warnings; now it raises one.
   var_msgs <- character(0)

   if (!has_wt)
      var_msgs <- c(var_msgs,
         "Weight (wt) not called: waz, wflz, and bmiz will not be calculated.")
   if (!has_lenhei)
      var_msgs <- c(var_msgs, paste0(
         "Length/height (lenhei) not called. If length and height are present, ",
         "combine them into one variable: lenhei = ifelse(agedays <= 730, length, height)."
      ))
   if (!has_headc)
      var_msgs <- c(var_msgs,
         "Head circumference (headc) not called: headcz will not be calculated.")
   if (!has_bmi && has_wt && has_lenhei)
      var_msgs <- c(var_msgs,
         "BMI not supplied: it will be calculated as wt / (lenhei/100)^2.")
   if (!has_bmi && !(has_wt && has_lenhei))
      var_msgs <- c(var_msgs,
         "BMI not found and cannot be calculated: bmiz will not be calculated.")

   if (length(var_msgs) > 0)
      warning(
         sprintf("%d variable issue(s) in who_z():\n%s",
                 length(var_msgs),
                 paste0("- ", var_msgs, collapse = "\n")),
         call. = FALSE
      )

   if (!agedays_var %in% names(data))
      stop("Age variable '", agedays_var, "' not found in data.")
   av <- data[[agedays_var]]
   if (!is.numeric(av))
      stop("Age variable '", agedays_var, "' must be numeric (age in days).")

   n_frac <- sum(abs(av - round(av)) > 1e-8, na.rm = TRUE)
   if (n_frac > 0) {
      warning(format(n_frac, big.mark = ","),
              " row(s) had non-integer agedays; rounded to whole days.")
      av <- round(av)
   }

   # ---- lean working table -----------------------------------------------
   # Built fresh under standardized names, from the caller's resolved columns
   # directly -- `data` itself is not touched. Every row is kept, in the
   # original order, so the results below line up with `data` position for
   # position.
   work <- data.table(agedays = av, sex_n = normalize_sex(data$sex, sex_code = sex_code))

   if (base::all(is.na(work$sex_n)))
      stop("No valid sex values found. Expected M/F, ",
           sex_code[1], "/", sex_code[2], ", B/G, or lower-case.")

   if (has_wt)     work[, wt     := data[[wt_arg$var]]]
   if (has_lenhei) work[, lenhei := data[[lenhei_arg$var]]]
   if (has_headc)  work[, headc  := data[[headc_arg$var]]]

   if (has_bmi) {
      work[, bmi := data[[bmi_arg$var]]]
   } else if (has_wt && has_lenhei) {
      work[, bmi := wt / (lenhei / 100)^2]
   }

   # length is used for weight-for-length, which applies only to ages <= 730 d;
   # older children get NA here and so find no match in the WFL reference.
   if (has_lenhei) {
     work[, len_2 := fifelse(agedays <= 730, lenhei, NA_real_)]
   }

   # ---- age-based indicators (waz, lhaz, bmiz, headcz) -----------------------
   # Only 'forage' rows can match on agedays, and we carry just the LMS columns
   # we actually use -- anything else would be duplicated across every data row.
   lmscols <- c('wei_l', 'wei_m', 'wei_s', 'len_l', 'len_m', 'len_s',
                'bmi_l', 'bmi_m', 'bmi_s', 'headc_l', 'headc_m', 'headc_s')
   lmscols <- intersect(lmscols, names(who_ref_data))

   # if (!is.data.table(cdcanthro::who_ref_data)) {
   #   who_ref_data <- as.data.table(who_ref_data)
   # }
   dref1 <- cdcanthro::who_ref_data[denom=='forage' & agedays <= 1826,
                                    c('sexn', 'agedays', lmscols), with = FALSE]
   # dref1's reference column is 'sexn' (from who_ref_data); work's is 'sex_n'.
   # nomatch = NA (the default) keeps every row of `work`, in `work`'s order:
   # ages over 1826 days, and unrecognised sex, get NA LMS values and thus NA
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
   # WFL needs BOTH weight and length; skipping the block also avoids building
   # dref2 and running a join that could not produce anything. wflz is created
   # INSIDE the block, so no all-NA wflz/fwflz columns appear when WFL can't
   # be computed.
   if (has_wt && has_lenhei) {
      dref2 <- cdcanthro::who_ref_data[denom=='forlen' & len>=45 & len<=110,
                            .(sexn,len,wfl_l,wfl_m,wfl_s)]

      # get ulens in 'work'
      ulens <- unique(work$len_2[!is.na(work$len_2)])
      # interpolate reference data to match each length in the input data
      dlen  <- length(setdiff(ulens, dref2$len))

      # --- REFERENCE CHART INTERPOLATION ---
      # If the input contains lengths not mapped directly inside the standard WHO
      # tables, we execute linear interpolation via stats::approx across
      # LMS parameters to generate custom growth metrics matching the user's data
      interp_vars <- c('wfl_l', 'wfl_m', 'wfl_s')

      if (dlen > 0) {
         dref2 <- dref2[, c(
            list(len = ulens),
            lapply(.SD, function(col) approx(len, col, xout = ulens)$y)
         ), by = sexn, .SDcols = interp_vars]
      }

     # original way of doing calculating wflz - required a new (dt2) data.table
     # dt2 <- dref2[work, on = c(sexn = 'sex_n', len = 'len_2')]
     #    set(dt1, j = 'wflz',
     #        value = calc_who_z(dt2$wt, dt2$wfl_l, dt2$wfl_m, dt2$wfl_s, correct = TRUE))
     #    dt1[agedays > 730, wflz := NA_real_]   # WFL applies only to length (<= 2 y)

      # i.wt reaches into work (the i-table) for the weight column —
      # dref2 has no wt column so it's unambiguous, but the prefix documents
      # where the value comes from.
      # dref2's reference columns are 'sexn'/'len'; work's are 'sex_n'/'len_2'.
     set(dt1, j = 'wflz',
         value = dref2[work, on = c(sexn = 'sex_n', len = 'len_2'),
                       calc_who_z(i.wt, wfl_l, wfl_m, wfl_s, correct = TRUE)])
     dt1[agedays > 730, wflz := NA_real_]   # WFL applies only to length (<= 2 y)
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

   for (z in zs) {
      v <- dt1[[z]]
      # z-scores are prefixed 'who_' so they never collide with cdcanthro()'s
      set(data, j = paste0('who_', z), value = v)
      set(data, j = fnm[[z]],
          value = as.integer(v < biv[[z]]['lo'] | v > biv[[z]]['hi']))
   }

   # If BMI was computed rather than supplied, surface it in the output too,
   # the same way cdcanthro() surfaces a computed BMI. (work$bmi/dt1$bmi is
   # the full-range computed value -- not restricted to agedays <= 1826 --
   # since BMI itself doesn't depend on the WHO age window, only bmiz does.)
   stopifnot(nrow(dt1) == nrow(data))
   if (!has_bmi && have_bmi) {
      set(data, j = "bmi", value = dt1$bmi)
   }

   message(
      "Z-scores for weight, lenhei (length or height), BMI, and headc are ",
      "calculated for children <= 1826 days.\n",
      "Z-scores for WFL (weight-for-length) are calculated for children ",
      "<= 730 days who have a length between 45 and 110 cm (inclusive)."
   )

   # return same type of data that was input
   restore_input_type(data, type$input_was_df, type$input_was_tibble)
   }
