if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(
    # Row ordering
    'seq_', '..v', '.',
    # Demographic and body size data
    'sexn', 'age', 'wt', 'ht', 'bmi',
    # LMS columns from reference data
    'wl', 'wm', 'ws', 'bl', 'bm', 'bs', 'hl', 'hm', 'hs',
    # Pre-computed SD limits for modified z-score
    'wt_sdm2', 'wt_sdp2', 'ht_sdm2', 'ht_sdp2', 'bmi_sdm2', 'bmi_sdp2',
    # Reference constants and pre-computed values
    'mref', 'sref', 'p95', 'sigma',
    # Weight and height  z-scores and percentiles
    'waz', 'mod_waz', 'wap','haz', 'mod_haz', 'hap',
    # BMI z-scores and percentiles
    'bz', 'mod_bmiz', 'bmip', 'bmiz', 'orig_bmiz', 'orig_bmip', 'ebz', 'ebp',
    # BMI derived metrics
    'p50', 'z1', 'z0', 'perc_median', 'bmip95',
    'dist_median', 'adj_dist_median', 'adj_perc_median',
    'log_perc_median', 'adj_log_perc_median',
    # Renamed LMS columns (used in cc() for setnames)
    'bmi_l', 'bmi_m', 'bmi_s',
    # Package dataset from sysdata.rda
    'cdc_ref_data',
    # temporary variables
    'inv_bl_bs', 'inv_wl_ws', 'inv_hl_hs', 'bmi_ratio'
  ))
}

# Internal helper functions

# cc(): captures unevaluated args as a character vector: cc(a, b) -> c("a", "b")
cc <- function(...) as.character(sys.call()[-1])

grepv <- function (..., ignore.case=TRUE, value=TRUE){
  base::grep(..., ignore.case=ignore.case, value=value)
}

.qn95 <- qnorm(0.95)

# lms_pct: moved to package level (was recreated inside cdcanthro() on every
lms_pct <- function(m, l, s, qn) m * (1 + l * s * qn) ^ (1 / l)


# -------------------------------------------------------------------------
# Main Anthropometric Function
# -------------------------------------------------------------------------
cdcanthro <- function(data, age, wt, ht, bmi, all_vars = FALSE) {
  .datatable.aware <- TRUE
  
  # Detect and preserve the input class so the function can return the same
  # type it received (data.frame, data.table, or tibble).
  input_class      <- class(data)[1]
  input_was_tibble <- input_class == "tbl_df"
  input_was_df     <- input_class == "data.frame"
  
  # Convert to data.table for all subsequent operations. For data.frame and
  # tibble inputs this creates a copy, leaving the caller's object untouched.
  # For data.table inputs, as.data.table() returns the object itself, so all
  # subsequent := operations modify it in place -- which is intentional since
  # we copy only the columns we need into lean_data below.
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  } else {
    data <- data.table::copy(data) # Explicit copy to prevent polluting user's global scope
  }
  
  # Save the original column names before adding any internal columns.
  # Used at the end to restore output column ordering via setcolorder().
  orig_cols <- names(data)
  
  # seq_ is used as a row-id to recover the original row order after the
  # final cbind. If the caller already has 'seq_' drop it with a warning.
  if ("seq_" %in% names(data)) {
    warning("Input column 'seq_' has been dropped; it is used internally for row ordering.")
    data[, seq_ := NULL]
  }
  data[, seq_ := seq_len(.N)]
  
  # Locate the sex column. It must be named 'sex' or 'SEX' (case-insensitive
  # match on the full name), but both cannot exist simultaneously since that
  # would make the sex coding ambiguous. Rename to lowercase 'sex'.
  nms <- grepv('^sex$|^SEX$', names(data))
  if (length(nms) != 1) {
    stop("A child's sex MUST be named 'sex' or 'SEX'.
          But your data can't contain both 'sex' and 'SEX'.")
  }
  if (nms != 'sex') setnames(data, nms, 'sex')
  
  # --- ARGUMENT PARSING ---
  # Capture the caller's argument expressions as column name strings.
  # deparse(substitute(x)) converts a bare name (e.g. agemos) to the
  # string "agemos" for column lookup. 
  
  # age is required. wt, ht, and bmi are optional -- if omitted by the
  # caller, missing() returns TRUE and the corresponding *_var is set to
  # NULL, which propagates to has_wt / has_ht / has_bmi = FALSE below.
  age_var <- deparse(substitute(age))
  if (!age_var %in% names(data)) stop("Age variable not found in data")
  
  wt_var  <- if (!missing(wt))  deparse(substitute(wt))  else NULL
  ht_var  <- if (!missing(ht))  deparse(substitute(ht))  else NULL
  bmi_var <- if (!missing(bmi)) deparse(substitute(bmi)) else NULL

  # Verify the column is actually present and issue warnings if absent
  has_wt  <- !is.null(wt_var)  && wt_var  %in% names(data)
  has_ht  <- !is.null(ht_var)  && ht_var  %in% names(data)
  has_bmi <- !is.null(bmi_var) && bmi_var %in% names(data)
  have_bmi <- has_bmi || (has_wt && has_ht)
  
  if (!has_wt) 
    warning("Weight variable not found: WAZ etc not calculated.")
  if (!has_ht) 
    warning("Height variable not found: HAZ etc not calculated.")
  if (!have_bmi) 
    warning("BMI not found and can't be calculated: no BMI metrics will be calculated.")
  if (!has_wt && !has_ht && !has_bmi)
    stop("At least one of weight, height, or BMI must be present in data.")
  
  # --- LEAN EXTRACTION ---
  # Build a lean/thin table to protect original data. 
  # Fast path: numeric sex coding (1/2) skips the 27M-element character round-trip.
  sx <- data$sex
  if (is.numeric(sx)) {
    sexn <- fcase(sx == 1, 1L, sx == 2, 2L)
  } else {
    # Compute unique values first (tiny vector)
    usex <- unique(sx)
    first_char <- substr(as.character(usex), 1, 1)
    # Vectorized map directly on the tiny `usex` vector
    usexn <- fifelse(first_char %chin% c("1", "B", "M", "b", "m"), 1L,
             fifelse(first_char %chin% c("2", "G", "F", "g", "f"), 2L, NA_integer_))
    # Map back instantly using fast binary matching
    sexn <- usexn[chmatch(sx, usex)]
  }
  
  lean_data <- data.table(
    seq_ = data$seq_,
    age  = data[[age_var]],
    sexn = sexn
  )
  
  if (base::all(is.na(lean_data$sexn))) 
    stop("No valid sex values found. Expected M/F, 1/2, B/G, or lower-case.")
  
  if (has_wt) lean_data[, wt := data[[wt_var]]] 
  if (has_ht) lean_data[, ht := data[[ht_var]]] 
  
  if (has_bmi) {
    lean_data[, bmi := data[[bmi_var]]]
  } else if (has_wt && has_ht) {
    lean_data[, bmi := wt / (ht / 100)^2]
  } else {
    lean_data[, bmi := NA_real_]
  }
  
  # --- AGE ALIGNMENT ---
  # If input ages contain zero fractional elements, the data are assumed to represent
  # rounded integer age-months. In this case, a midpoint adjustment 
  # of +0.5 months is added to age. If even one decimal age is found, 
  # the data are treated as exact/continuous timeline markers and left intact.
  # Drops out the second it hits a continuous decimal value
  uages <- unique(lean_data$age)
  if (!any(uages %% 1 != 0, na.rm = TRUE)) {
    warning("All ages are integers so 0.5 months has been added.")
    set(lean_data, j = "age", value = lean_data$age + 0.5)
    uages <- uages + 0.5 # Update unique ages to match
  }
  
  # interpolate reference data to match each age_month in input data
  dlen  <- length(setdiff(uages, cdc_ref_data$age))
  
  # --- REFERENCE CHART INTERPOLATION ---
  # If the input contains specific ages not mapped directly inside the standard CDC 
  # monthly tables, we execute linear interpolation via stats::approx across 
  # all LMS parameters to generate custom growth metrics matching the user's profiles.
  interp_vars <- setdiff(names(cdc_ref_data), cc(sexn, age, mref, sref))
  
  if (dlen > 0) {
    ref_data <- cdc_ref_data[, c(
        list(age = uages),
        list(mref = mref[1L], sref = sref[1L]), 
        # because mref and sref are constants within sex and have a length of ~217
      lapply(.SD, function(col) approx(age, col, xout = uages)$y)),
      by = sexn, .SDcols = interp_vars]
  } else {
    ref_data <- data.table::copy(cdc_ref_data)
  }
  # By using list(age = uages) directly inside the j slot alongside lapply(), 
  # you skip the overhead of allocating intermediate tables
  # If age were included in .SD, lapply would call approx() on it too —
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
      p95      = lms_pct(bm, bl, bs, .qn95),
      sigma    = fifelse(sexn == 1L, 
                    round(0.3728 + 0.5196 * (age / 12) - 0.0091 * (age / 12)^2, 8), 
                    round(0.8334 + 0.3712 * (age / 12) - 0.0011 * (age / 12)^2, 8))
                    # rounded to agree with Hales / NCHS
    )] 
  }
  
  # Get number of valid ages in data
  n_inrange <- lean_data[data.table::between(age, 24.0, 239.9999), .N]
  n_excluded <- nrow(data) - n_inrange
  if (n_excluded > 0) {
    warning(prettyNum(n_excluded, big.mark = ","), " ",
            "Input row(s) had ages in months that were < 24 or >= 240 months. ",
            "The CDC Growth Charts apply to ages 24 months up to (but not ",
            "including) 240 months. Rows with ages outside ",
            "this range will have NAs for the calculated variables.")
  }
  rm(n_excluded, n_inrange)

  # mere ref_data with lean_data, keep only lean_data rows
  dt <- ref_data[lean_data, on = .(sexn, age)]
  # collapse::join is NOT faster
  
  # Modified Z-score uses pre-computed values for SD2
  if (has_wt) {
    dt[, let(
      waz     = ((wt / wm)^wl - 1) * inv_wl_ws,
      mod_waz = (wt - wm) / (0.5 * fifelse(wt < wm, wt_sdm2, wt_sdp2))
    )]
    dt[abs(wl) < 0.01, waz := log(wt / wm) / ws]
    # z-scores use the standard LMS formula, switching to the logarithmic (L=0) 
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
        p50         = bm,              # copy: bm must survive as bmi_m
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
      setnames(dt, cc(bl, bm, bs), cc(bmi_l, bmi_m, bmi_s))
    } 
    drop_cols <- intersect(c('bmi_sdm2', 'bmi_sdp2', 'inv_bl_bs', 'bmi_ratio', 
                             'bm', 'mref', 'sref', 'sexn'),  
                           names(dt))
    dt[, (drop_cols) := NULL]
  }
  
  # Create Extended z-score for BMI >=95th P
  # For children tracking above the 97th percentile, traditional LMS z-scores 
  # flatten out artificially. 
  if (have_bmi) {
    dt[bmip >= 95, ebp := 90 + 10 * pnorm((bmi - p95) / sigma)]
    # Cap extended z at 8.21: the largest possible ebz is ~8.2095
    dt[bmip >= 95, ebz := fifelse(ebp / 100 < 1, qnorm(ebp / 100), 8.21)]

  # Rename: This preserves the original LMS-based values under 'orig_*' names 
  setnames(dt,
             cc(bmiz,        bmip,      ebp,  ebz),
             cc(orig_bmiz, orig_bmip, bmip, bmiz))
  }

  v <- cc(seq_, bmiz, bmip, p50, p95, bmip95, orig_bmip, orig_bmiz, perc_median,
          mod_bmiz, waz, wap, mod_waz, haz, hap, mod_haz)
  
  if (!has_bmi & have_bmi) v <- c('bmi', v) 
  
  if (isTRUE(all_vars) & have_bmi) {
    v=c(v, 'bmi_l', 'bmi_m', 'bmi_s',  'sigma', 'adj_dist_median', 'dist_median',
        'adj_perc_median', 'log_perc_median', 'adj_log_perc_median')
  }
  
  # Keep only columns that exist in dt
   v <- intersect(v, names(dt))
   dt_out <- dt[, ..v, with=FALSE] # excluded intermediate variables
   
   # Rename input columns that would collide with computed output columns
   # by adding '0' at the end. Without this, cbind would create duplicate names 
   # and the original values would mask the computed ones.
   overlap <- intersect(names(data), names(dt_out))
   overlap <- setdiff(overlap, "seq_")
   if (length(overlap) > 0) {
     setnames(data, overlap, paste0(overlap, "0"))
     warning(paste(overlap, collapse = ", "),
             " in original data renamed to ", 
             paste(paste0(overlap, "0"), collapse = ", "),
             " to avoid conflict with computed values.")
   }
    
  # data and dt_out must both be in original row order
  if (is.unsorted(data$seq_)) setorder(data, seq_)
  if (is.unsorted(dt_out$seq_)) setorder(dt_out, seq_)
   # Remove seq_ as it's no longer needed
   data[, seq_ := NULL]; dt_out[, seq_ := NULL]
   
   # Add dt_out's columns directly into data
   for (col in names(dt_out)) {
     set(data, j = col, value = dt_out[[col]])
   }
    # Now 'data' contains everything, so rename it to match your final logic
   dtot <- data; 
   rm(dt_out, data) 
   
  # The following applies ONLY if lean_data was limited to 24 to 239.999 months
  # subset to surviving rows [24 months, < 240 months), in matching order
  # Because seq_ was created as 1:N on the original data, 
  # each value in dt_out$seq_ serves as an explicit pointer back to the 
  # exact row position in data where that record originated.
  # dtot <- data[dt_out$seq_]
  # rm(data,dt_out)
  # data[, seq_ := NULL]; dt_out[, seq_ := NULL]
  # dtot <- cbind(data, dt_out)
   
  # Reorder columns to ensure the original ones appear first
  desired_order <- c(orig_cols, setdiff(names(dtot), orig_cols))
  setcolorder(dtot, desired_order, skip_absent=TRUE)
  
  # return same type of data that was input
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
