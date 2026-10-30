# who_z() now live together in R/zzz.R.
#
# Input-type preservation has moved to R/utils-anthro.R, where who_z()
# uses the same helpers -- see that file for the shared implementations.

.qn95 <- qnorm(0.95)

# warns once per session about changing from cdcanthro (deprecated) to cdc_z
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
# Main Anthropometric Function
# -------------------------------------------------------------------------
#
# ROW ALIGNMENT
# -------------
# No row-id column is used. Results stay positionally aligned with the
# input because `work` (below) is built directly from `data`, in `data`'s
# row order, and the reference join uses nomatch = NA, which returns
# exactly one row per row of `work` -- children outside the reference
# range simply get NA LMS values and therefore NA z-scores. This is the
# same invariant who_z() relies on for its own reference joins (see the
# ROW ALIGNMENT note in who_z.R): as long as the interpolated
# reference table has at most one row per (sexn, age), the join can
# neither drop nor reorder rows, so the computed columns can be written
# straight onto `data` with set() at the end -- no row-id column, no
# separate output table to re-sort, and no join needed at the end.
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
  # age is required. wt, ht, and bmi are optional -- if omitted by the
  # caller, resolve_optional_var()'s $var is NULL, which propagates to
  # has_wt / has_ht / has_bmi = FALSE below. A supplied-but-non existent
  # column name (a likely typo) is warned about immediately by
  # resolve_optional_var(), and remembered via $typo so the generic
  # "not found" warning below doesn't also fire for the same problem.
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
  # Build a thin working table directly from `data`, to protect the
  # original data and give the reference join a small table to match
  # against. `work` is built by direct column extraction, so it stays in
  # exactly `data`'s row order -- this is what the ROW ALIGNMENT note above
  # relies on.
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

  # code 0 and negative values to NA - see utils
  set_nonpositive_na(work, c("wt", "ht"))

  if (has_bmi) {
    work[, bmi := num_col(data[[bmi_arg$var]])]
  } else if (has_wt && has_ht) {
    work[, bmi := (wt * 10000) / (ht * ht)]
  } else {
    work[, bmi := NA_real_]
  }

  set_nonpositive_na(work, c("bmi"))

  # --- AGE ALIGNMENT ---
  # If input ages contain zero fractional elements, the data are assumed to represent
  # rounded integer age-months. In this case, a midpoint adjustment
  # of +0.5 months is added to age. If even one decimal age is found,
  # the data are left intact.
  uages <- unique(work$age)
  uages <- uages[!is.na(uages)]

  if (length(uages) > 0L && !any(abs(uages - round(uages)) > 1e-8)) {
    warning("All ages are integers so 0.5 months has been added before the ",
            "calculations. See Details. The input data was not modified.")
    set(work, j = "age", value = work$age + 0.5)
    uages <- uages + 0.5  # Update unique ages to match
  }

  # --- REFERENCE CHART INTERPOLATION ---
  # If the input contains specific ages not contained in reference data,
  # linearly interpolate via stats::approx for all LMS parameters

  # Keep the 23.5 row in ref_data: the table has no 24.0 row,
  # so ages 24.0 to < 24.5 are interpolated between 23.5 and 24.5.
  # Ages under 24 get NA as do ages >= 240.
  ref_data <- cdcanthro::cdc_ref_data  # ages 23.5, 24.5, ..., 239.5, 240
  uages <- uages[uages >= 24 & uages < 240]

  # does uages contain any values not in reference data?
  need_interp <- !all(uages %in% ref_data$age)
  interp_vars <- setdiff(names(ref_data), c('sexn', 'age', 'mref', 'sref'))

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

  # By using list(age = uages) directly inside the j slot alongside lapply(),
  # you skip the overhead of allocating intermediate tables
  # If age were included in .SD, lapply would call approx() on it too --
  # interpolating age against itself at the target ages, which returns uages back.

  # Compute p95, etc parameters on the tiny reference table
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
              # rounded to agree with Hales / NCHS
    )]
  }


  # merge ref_data with work in data's row order. See the ROW ALIGNMENT note
  # above cdc_z().  Note that collapse::join is NOT faster
  # ref_data's reference column is 'sexn' (from cdc_ref_data); work's is 'sex_n'.
  dt <- ref_data[work, on = c(sexn = 'sex_n', 'age')]

  # Modified Z-score uses pre-computed values for SD2 etc
  if (has_wt) {
    dt[, let(
      waz     = ((wt / wm)^wl - 1) * inv_wl_ws,
      mod_waz = (wt - wm) / (0.5 * fifelse(wt < wm, wt_sdm2, wt_sdp2))
    )]
    dt[abs(wl) < 0.01, waz := log(wt / wm) / ws]
    # z-scores switch to the logarithmic (L=0)
    # form when |L| < 0.01, matching the CDC SAS implementation.

    dt[, wap := 100 * pnorm(waz)]
    dt[, c('wl', 'wm', 'ws', 'wt_sdm2', 'wt_sdp2', 'inv_wl_ws') := NULL]
  }

  if (has_ht) {
    dt[, let(
      haz     = ((ht / hm)^hl - 1) * inv_hl_hs,
      mod_haz = (ht - hm) / (0.5 * fifelse(ht < hm, ht_sdm2, ht_sdp2))
    )]
    dt[abs(hl) < 0.01, haz := log(ht / hm) / hs]
    dt[, hap := 100 * pnorm(haz)]
    dt[, c('hl', 'hm', 'hs', 'ht_sdm2', 'ht_sdp2', 'inv_hl_hs') := NULL]
  }

  if (have_bmi) {
    # --- ALWAYS COMPUTED ---
    dt[, bmi_ratio := bmi / bm]
    dt[, let(
      perc_median = (bmi_ratio - 1) * 100,
      bmip95      = 100 * (bmi / p95),
      mod_bmiz    = (bmi - bm) / (0.5 * fifelse(bmi < bm, bmi_sdm2, bmi_sdp2)),
      bmiz        = (bmi_ratio^bl - 1) * inv_bl_bs
    )]
    # dt[abs(bl) < 0.01, bmiz := log(bmi / bm) / bs]
    # bmi_l never comes close to 0
    dt[, let(bmip = 100 * pnorm(bmiz))]
    dt[, let(ebp = bmip, ebz = bmiz)]

    # --- all_vars EXTRAS ONLY ---
    if (isTRUE(all_vars)) {
      dt[, let(
        dist_median = bmi - bm,
        z1          = (bmi_ratio - 1) / bs,
        z0          = log(bmi_ratio) / bs
      )]
      dt[, let(
        adj_dist_median     = z1 * sref * mref,
        adj_perc_median     = z1 * 100 * sref,
        log_perc_median     = z0 * 100 * bs,
        adj_log_perc_median = z0 * 100 * sref
      )]
      setnames(dt, c('bl', 'bm', 'bs'), c('bmi_l', 'bmi_m', 'bmi_s'))
    }
    drop_cols <- intersect(c('bmi_sdm2', 'bmi_sdp2', 'inv_bl_bs', 'bmi_ratio',
                             'bm', 'mref', 'sref', 'sexn', 'bl', 'bs'),
                           names(dt))
    dt[, (drop_cols) := NULL]
  }

  # Create Extended z-score for BMI >=95th P.  For children tracking above
  # the 97th percentile, traditional LMS z-scores flatten out artificially.
  if (have_bmi) {
    dt[bmip >= 95, ebp := 90 + 10 * pnorm((bmi - p95) / sigma)]
    # Cap extended z at 8.21: the largest possible ebz is ~8.2095
    # ebp can be so close to 100 that ebz calculation will return NA
    dt[bmip >= 95 & ebp < 100, ebz := qnorm(ebp / 100)]
    dt[bmip >= 95 & ebp >= 100, ebz := 8.21]


  # Preserve the original LMS-based values under 'orig_*' names
  setnames(dt,
             c('bmiz',        'bmip',    'ebp',  'ebz'),
             c('orig_bmiz', 'orig_bmip', 'bmip', 'bmiz'))
  }

  v <- c('bmiz', 'bmip', 'p95', 'bmip95', 'orig_bmip', 'orig_bmiz',
        'perc_median', 'mod_bmiz', 'waz', 'wap', 'mod_waz', 'haz', 'hap',
        'mod_haz')

  if (!has_bmi && have_bmi) v <- c('bmi', v)

  if (isTRUE(all_vars) && have_bmi) {
    v <- c(v, 'bmi_l', 'bmi_m', 'bmi_s',  'sigma', 'adj_dist_median', 'dist_median',
        'adj_perc_median', 'log_perc_median', 'adj_log_perc_median')
  }

  # Keep only columns that were actually computed
  v <- intersect(v, names(dt))

  # --- WRITE the RESULTS ONTO `data` ---
  # `dt` has exactly nrow(data) rows, in data's row order (see the ROW
  # ALIGNMENT note above cdc_z()), so each column in `v` can be copied
  # across by position with set() -- no row-id column, no second copy of
  # the input to re-sort, and no join at the end.

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

  stopifnot(nrow(dt) == nrow(data))
  for (col in v) set(data, j = col, value = dt[[col]])

  # Reorder columns to ensure the original ones appear first
  # desired_order <- c(orig_cols, setdiff(names(data), orig_cols))
  # setcolorder(data, desired_order, skip_absent=TRUE)

  # return same type of data that was input
  restore_input_type(data, type$input_was_df, type$input_was_tibble)
}
