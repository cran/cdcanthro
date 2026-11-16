# cdc_z(): CDC 2000 growth-chart z-scores and percentiles for weight, height,
# and BMI (24.0 to < 240 months), including the extended BMI metrics
# (Wei et al. 2020) for BMIs at or above the 95th percentile.
#
# The globalVariables() declarations for the column names used in
# data.table calls by cdc_z(), who_z() and gradual_z() live together in
# R/globals.R. Input-type preservation and argument resolution live in
# R/utils-anthro.R, where who_z() uses the same helpers.
#
# Pipeline: validate arguments -> build a thin `work` table (age, sex,
# measurements) in the input's row order -> align ages (+0.5 for completed
# months) -> interpolate the reference L, M, S to the ages present -> look
# up each row's reference values by index -> compute z-scores, percentiles
# and the extended BMI metrics on `work` -> bind the requested columns to
# the input and return it in the input's class.

# z at the 95th percentile; p95 = M * (1 + L*S*.qn95)^(1/L)
.qn95 <- qnorm(0.95)
# cdcanthro() is the old name of cdc_z(); it warns once per session.
.cdcanthro_state <- new.env(parent = emptyenv())
.cdcanthro_state$warned <- FALSE

cdcanthro <- function(data, age = agemos, wt = wt, ht = ht, bmi = bmi,
                      sex_code = 1:2, all_vars = FALSE) {
  if (!.cdcanthro_state$warned) {
    warning(
      "`cdcanthro()` is deprecated; please use `cdc_z()`.",
      call. = FALSE
    )
    .cdcanthro_state$warned <- TRUE
  }
  mc <- match.call()

  # Quote the fully qualified function name to protect the traceback
  # and guarantee namespace resolution.
  mc[[1]] <- quote(cdcanthro::cdc_z)
  eval(mc, envir = parent.frame())
}

# -------------------------------------------------------------------------
# cdc_z()
# -------------------------------------------------------------------------

cdc_z <- function(data,
                      age = agemos,
                      wt = wt,
                      ht = ht,
                      bmi = bmi,
                      sex_code = 1:2,
                      all_vars = FALSE
                      )
{

  # --- INPUT VALIDATION ---
  # Catch a missing or malformed 'data' argument early with a clear message,
  # rather than failing deep inside as.data.table() or a downstream
  # data.table call with a confusing error.
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
  # malformed sex_code fails immediately.
  validate_sex_code(sex_code)

  # Work on a data.table copy so we never modify the caller's object by
  # reference; see as_data_table_safe() in utils-anthro.R
  type <- as_data_table_safe(data)
  data <- type$data

  # Locate and standardize the sex column (must be some case variant of
  # 'sex'; both 'sex' and 'SEX' present at once is ambiguous).
  sex_var <- resolve_sex_column(data)

  # --- ARGUMENT PARSING ---
  # age is required. wt, ht and bmi are optional: if the named column is not
  # in data, resolve_optional_var() returns $var = NULL and $has = FALSE, and
  # the metrics that need it are left out of the output. A supplied-but-
  # nonexistent name (a likely typo) is warned about inside
  # resolve_optional_var().
  age_var <- resolve_col_name(substitute(age))
  if (!age_var %in% names(data))
    stop("Age variable '", age_var, "' not found in data.")
  if (!is.numeric(data[[age_var]]))
    stop("Age variable '", age_var, "' must be numeric (age in months).")

  wt_arg  <- resolve_optional_var(data, substitute(wt),  missing(wt),  "wt")
  ht_arg  <- resolve_optional_var(data, substitute(ht),  missing(ht),  "ht")
  bmi_arg <- resolve_optional_var(data, substitute(bmi), missing(bmi), "bmi")

  # The same column named for two measurements is almost always a positional
  # slip, e.g. cdc_z(d, age, ht) puts the height column into 'wt'.
  used <- c(wt = wt_arg$var, ht = ht_arg$var, bmi = bmi_arg$var)
  dup  <- used[duplicated(used) | duplicated(used, fromLast = TRUE)]
  if (length(dup))
    stop("Column '", dup[[1]], "' is used for more than one of: ",
         paste(names(dup), collapse = ", "),
         ". Name the arguments, e.g. cdc_z(data, age = ..., ht = ...).")

  has_wt  <- wt_arg$has
  has_ht  <- ht_arg$has
  has_bmi <- bmi_arg$has
  have_bmi <- has_bmi || (has_wt && has_ht)

  if (!has_wt && !has_ht && !has_bmi)
    stop("At least one of weight, height, or BMI must be present in data.")

  bad <- character(0)
  if (has_wt && !is_num_or_all_na(data[[wt_arg$var]]))
    bad <- c(bad, sprintf("weight variable '%s' (kg)", wt_arg$var))
  if (has_ht && !is_num_or_all_na(data[[ht_arg$var]]))
    bad <- c(bad, sprintf("height variable '%s' (cm)", ht_arg$var))
  if (has_bmi && !is_num_or_all_na(data[[bmi_arg$var]]))
    bad <- c(bad, sprintf("BMI variable '%s'", bmi_arg$var))
  if (length(bad) > 0)
    stop("The following must be numeric: ", paste(bad, collapse = "; "), ".")

  # --- WORK TABLE EXTRACTION ---
  # Build a thin working table directly from `data`, so the caller's
  # columns are never written to and the reference lookup has only the
  # columns it needs. `work` is built by direct column extraction, so it
  # stays in exactly `data`'s row order; the positional lookup below
  # (idx / rc()) relies on this.
  sex_n <- normalize_sex(data[[sex_var]], sex_code = sex_code)

  work <- data.table(
    age  = data[[age_var]],
    sex_n = sex_n
  )

  if (nrow(work) > 0L && all(is.na(work$sex_n)))
    stop("No valid sex values found. Expected M/F, ",
         sex_code[1], "/", sex_code[2], ", B/G, or lower-case.")

  if (has_wt) work[, wt := num_col(data[[wt_arg$var]])]
  if (has_ht) work[, ht := num_col(data[[ht_arg$var]])]

  # Zero and negative measurements become NA; see set_nonpositive_na() in
  # utils-anthro.R.
  set_nonpositive_na(work, c("wt", "ht"))

  if (has_bmi) {
    work[, bmi := num_col(data[[bmi_arg$var]])]
  } else if (has_wt && has_ht) {
    work[, bmi := (wt * 10000) / (ht * ht)]
  }

  set_nonpositive_na(work, c("bmi"))

  # --- AGE ALIGNMENT ---
  # If no age has a fractional part, the ages are taken to be completed
  # months (as NHANES reports them) and 0.5 is added so that 36 means the
  # midpoint of [36, 37). One non-integer age anywhere disables this and
  # the ages are used as given. The caller's data are not modified.
  uages <- unique(work$age)
  uages <- uages[!is.na(uages)]

  if (length(uages) > 0L && !any(abs(uages - round(uages)) > 1e-8)) {
    warning("All ages are integers so 0.5 months has been added before the ",
            "calculations to align with CDC intervatls. See Details. ",
            "The input data was not modified.")
    set(work, j = "age", value = work$age + 0.5)
    uages <- uages + 0.5  # Update unique ages to match
  }

  # --- REFERENCE CHART INTERPOLATION ---
  # The reference grid is 23.5, 24.5, ..., 239.5, 240 months. Ages not on
  # the grid get L, M, S (and the other reference columns) by linear
  # interpolation with stats::approx(), within sex, at the unique ages
  # present in the data.
  #
  # The 23.5 row is kept because the table has no 24.0 row: ages 24.0 to
  # < 24.5 are interpolated between 23.5 and 24.5. Ages under 24 and ages
  # >= 240 are excluded from uages, find no reference row, and get NA.
  ref_data <- cdcanthro::cdc_ref_data  # ages 23.5, 24.5, ..., 239.5, 240
  uages <- uages[uages >= 24 & uages < 240]

  # Interpolate only if some age is off the grid; otherwise subset.
  need_interp <- !all(uages %in% ref_data$age)
  interp_vars <- setdiff(names(ref_data), c('sexn', 'age', 'mref', 'sref'))

  # age is passed in j as list(age = uages) rather than through .SD, so
  # lapply() does not interpolate age against itself. mref and sref (the
  # BMI M and S at 240 months) are constant within sex, so one value each
  # is carried through rather than interpolated.
  if (need_interp) {
    ref_data <- ref_data[,
         c(
           list(
             age  = uages,
             mref = mref[1L],
             sref = sref[1L]
           ),
           lapply(.SD, function(col) approx(age, col, xout = uages)$y)
         ),
         by = sexn, .SDcols = interp_vars
    ]
  } else {
    ref_data <- ref_data[age %in% uages]
  }

  # Derived reference quantities (1/(L*S), the +/-2 SD values for the
  # modified z-scores, p95 and sigma) are computed on the reference table --
  # one row per sex and unique age -- rather than on the n data rows.
  if (has_wt) {
    ref_data[, let(
      inv_wl_ws = 1 / (wl * ws),
      wt_sdm2 = wm - (wm * (1 - 2 * wl * ws)^(1 / wl)),
      wt_sdp2 = (wm * (1 + 2 * wl * ws)^(1 / wl)) - wm
    )]
  }
  if (has_ht) {
    ref_data[, let(
      inv_hl_hs = 1 / (hl * hs),
      ht_sdm2 = hm - (hm * (1 - 2 * hl * hs)^(1 / hl)),
      ht_sdp2 = (hm * (1 + 2 * hl * hs)^(1 / hl)) - hm
    )]
  }
  if (have_bmi) {
    ref_data[, let(
      inv_bl_bs = 1 / (bl * bs),
      bmi_sdm2 = bm - (bm * (1 - 2 * bl * bs)^(1 / bl)),
      bmi_sdp2 = (bm * (1 + 2 * bl * bs)^(1 / bl)) - bm,
      p95      = bm * (1 + bl * bs * .qn95) ^ (1 / bl),
      sigma    = fifelse(sexn == 1L,
              round(0.3728 + 0.5196 * (age / 12) - 0.0091 * (age / 12)^2, 8),
              round(0.8334 + 0.3712 * (age / 12) - 0.0011 * (age / 12)^2, 8))
              # rounded to 8 decimals to match the sigma values in the
              # CDC SAS program
    )]
  }

  # Positional lookup, not a join: idx[i] is the row of ref_data matching
  # work[i] on (sex, age), or NA (age outside 24 to < 240, unrecognized sex,
  # missing age). rc() pulls one reference column at a time through idx, so
  # no n-row copy of the reference table is made. length(idx) == nrow(work)
  # holds only if ref_data has one row per (sexn, age); the stopifnot()
  # guards that. who_z() uses the same pattern.
  idx <- ref_data[work, on = c(sexn = "sex_n", "age"), which = TRUE]
  stopifnot(length(idx) == nrow(work))
  rc  <- function(col) ref_data[[col]][idx]

  if (has_wt) {
    # temporary reference columns, so the |L| < 0.01 line below can subset them
    work[, let(wl = rc("wl"), wm = rc("wm"), ws = rc("ws"))]
    work[, waz     := ((wt / wm)^wl - 1) * rc("inv_wl_ws")]
    work[, mod_waz := (wt - wm) / (0.5 * fifelse(wt < wm, rc("wt_sdm2"), rc("wt_sdp2")))]
    # z-scores switch to the log (L = 0) form when |L| < 0.01, matching the
    # CDC SAS program. In cdc_ref_data only hl crosses zero (range -0.39 to
    # 2.24), so only height can hit the rule, including at interpolated
    # ages; wl is always <= -0.197, so the line below never fires:
    # work[abs(wl) < 0.01, waz := log(wt / wm) / ws]
    work[, wap := 100 * pnorm(waz)]
    work[, c("wl", "wm", "ws") := NULL]
  }

  if (has_ht) {
    # temporary reference columns, so the |L| < 0.01 line below can subset them
    work[, let(hl = rc("hl"), hm = rc("hm"), hs = rc("hs"))]
    work[, haz     := ((ht / hm)^hl - 1) * rc("inv_hl_hs")]
    work[, mod_haz := (ht - hm) / (0.5 * fifelse(ht < hm, rc("ht_sdm2"), rc("ht_sdp2")))]
    work[abs(hl) < 0.01, haz := log(ht / hm) / hs]
    work[, hap := 100 * pnorm(haz)]
    work[, c("hl", "hm", "hs") := NULL]
  }

  if (have_bmi) {
    # Temporary reference columns. p95 and sigma are also used by the
    # extended-BMI block below, which subsets rows (bmip >= 95).
    work[, let(bm = rc("bm"), p95 = rc("p95"), sigma = rc("sigma"))]

    # --- ALWAYS COMPUTED ---
    work[, bmi_ratio := bmi / bm]
    work[, let(
      perc_median = (bmi_ratio - 1) * 100,
      bmip95      = 100 * (bmi / p95),
      mod_bmiz    = (bmi - bm) / (0.5 * fifelse(bmi < bm, rc("bmi_sdm2"), rc("bmi_sdp2"))),
      bmiz        = (bmi_ratio^rc("bl") - 1) * rc("inv_bl_bs")
    )]
    # No log-form switch for BMI: bl in cdc_ref_data is always <= -0.95,
    # far from the 0.01 threshold used for weight and height.
    work[, let(bmip = 100 * pnorm(bmiz))]
    work[, let(ebp = bmip, ebz = bmiz)]

    # --- all_vars EXTRAS ONLY ---
    if (isTRUE(all_vars)) {
      work[, bs := rc("bs")]
      sref <- rc("sref")
      work[, let(
        dist_median = bmi - bm,
        z1          = (bmi_ratio - 1) / bs,
        z0          = log(bmi_ratio) / bs
      )]
      work[, let(
        adj_dist_median     = z1 * sref * rc("mref"),
        adj_perc_median     = z1 * 100 * sref,
        log_perc_median     = z0 * 100 * bs,
        adj_log_perc_median = z0 * 100 * sref
      )]
      rm(sref)
      work[, let(bmi_l = rc("bl"), bmi_m = rc("bm"), bmi_s = rc("bs"))]
    }

    drop_cols <- intersect(c('bmi_ratio', 'z1', 'z0'), names(work))
    for (col in drop_cols) {
      set(work, j = col, value = NULL)
    }

    # Extended BMI percentile and z-score for BMI >= 95th percentile
    # (Wei et al. 2020): above the 95th percentile the LMS z-scores compress
    # into a narrow range, so BMI is instead modelled as half-normal above
    # p95 with scale sigma, and the percentile is 90 + 10 * Phi(.).
    work[bmip >= 95, ebp := 90 + 10 * pnorm((bmi - p95) / sigma)]
    # ebp can round to exactly 100, where qnorm(1) is Inf. qnorm() of the
    # largest double below 1 is 8.2095, so 8.21 is the ceiling of what ebz
    # could ever be and is used as the cap.
    work[bmip >= 95 & ebp < 100, ebz := qnorm(ebp / 100)]
    work[bmip >= 95 & ebp >= 100, ebz := 8.21]

    # Preserve the original LMS-based values under 'orig_*' names
    setnames(work,
             c('bmiz',      'bmip',      'ebp',  'ebz'),
             c('orig_bmiz', 'orig_bmip', 'bmip', 'bmiz'))
    if (!isTRUE(all_vars)) work[, sigma := NULL]
  }

  v <- c('bmiz', 'bmip', 'p95', 'bmip95', 'orig_bmip', 'orig_bmiz',
        'perc_median', 'mod_bmiz', 'waz', 'wap', 'mod_waz', 'haz', 'hap',
        'mod_haz')

  if (!has_bmi && have_bmi) v <- c('bmi', v)

  if (isTRUE(all_vars) && have_bmi) {
    v <- c(v, 'bmi_l', 'bmi_m', 'bmi_s',  'sigma', 'adj_dist_median', 'dist_median',
        'adj_perc_median', 'log_perc_median', 'adj_log_perc_median')
  }

  # Keep only cols that were actually computed
  v <- intersect(v, names(work))

  # Rename any of data's own columns that would collide with a computed
  # output column. New names are made unique against every other input
  # column and every output column, so an existing 'bmiz0' can't collide.
  overlap <- intersect(names(data), v)
  if (length(overlap) > 0) {
    new_nm <-  utils::tail(make.unique(c(setdiff(names(data), overlap), v,
                                 paste0(overlap, "0"))),
                   length(overlap))
    setnames(data, overlap, new_nm)
    warning(paste(overlap, collapse = ", "),
            " in original data renamed to ",
            paste(new_nm, collapse = ", "),
            " to avoid conflict with computed values.")
  }

  stopifnot(nrow(work) == nrow(data))
  out <- setDT(c(as.list(data), as.list(work)[v]))

  # return same type of data that was input (out, not data)
  restore_input_type(out, type$input_was_df, type$input_was_tibble)
}
