#
# gradual_z function for gradual transition WHO-CDC z-score smoothing
#
# Blends WHO and CDC growth-chart z-scores over the 2-to-5-year transition
# window recommended by CDC: WHO reference values are used through
# 730 days of age, CDC reference values are used from 1826 days onward, and
# a linearly age-weighted average of the two is used in between.
#
# Each of the three measures (weight, height/length, BMI) is handled
# independently: a gradual z-score is only computed for a measure if BOTH
# its WHO and CDC columns are present in 'data'. Measures with neither
# column present are skipped with an informational message(); measures with
# only one of the two columns present trigger a warning() and are skipped
# (an incomplete pair can't be blended).
#
# Input variables:
#   data     = a data.frame, tibble, or data.table. The same class is
#              returned. Must contain the age column, plus at least one
#              complete WHO/CDC column pair.
#   agedays  = column with age in days (quoted or unquoted name). Always required.
#   whowtz, cdcwtz   = columns with WHO and CDC weight-for-age z-scores
#   whohtz, cdchtz   = columns with WHO and CDC height/length-for-age z-scores
#   whobmiz, cdcbmiz = columns with WHO and CDC BMI-for-age z-scores
#
# Output variables:
#   grad_waz, grad_haz, grad_bmiz = gradual z-scores, one per measure for
#     which both WHO and CDC columns were found (WHO <= 730 days, weighted
#     average 731-1825 days, CDC >= 1826 days). Rows with missing agedays
#     return NA for the gradual z-scores.
#
# SHARED HELPERS
# --------------
# This file depends on R/utils-anthro.R for shared plumbing:
#  - as_data_table_safe(), restore_input_type(), snapshot_names() for the
#    same input-type-detection and column-name-snapshot handling that
#    whoanthro() and cdcanthro() use.
#  - resolve_col_name() to turn each column-name argument's substitute()d
#    expression into a string, accepting either a bare symbol (whowtz =
#    who_waz) or a quoted string (whowtz = "who_waz") -- the same
#    quote-tolerant resolution whoanthro() and cdcanthro() use for their
#    own column-name arguments. See utils-anthro.R for how it tells the
#    two forms apart without ever evaluating a bare symbol.
#
# One helper from utils-anthro.R is deliberately NOT reused here, because
# gradual_z()'s column-name arguments behave differently from whoanthro()'s
# and cdcanthro()'s by design:
#
#  - resolve_optional_var() treats an argument as "not supplied" based on
#    missing(the_arg) -- correct for whoanthro()/cdcanthro(), where a
#    column-name default like `wt = wt` is a placeholder that only takes
#    effect if the caller explicitly writes `wt = wt`. gradual_z()'s
#    defaults (whowtz = who_waz, cdcwtz = waz, ...) are, by contrast,
#    meant to be REAL, LIVE fallback column names -- they are exactly
#    whoanthro()'s and cdcanthro()'s own default output column names, so
#    that `gradual_z(data)` with no column arguments at all just works
#    against their output. Gating that on missing() would break the
#    "just works after running whoanthro()+cdcanthro()" convenience:
#    calling gradual_z(data) without naming whowtz would resolve to
#    has_wt = FALSE even when data actually has a who_waz column, since
#    missing(whowtz) is TRUE whenever the caller doesn't write it out.
#    gradual_z() instead always resolves the argument to a name (default
#    or explicit, quoted or not) via resolve_col_name(substitute(.)) and
#    then simply checks whether that name exists in data -- which is what
#    the who_present/cdc_present checks below do. This also means the
#    defaults are unaffected by accepting quotes: an unsupplied argument's
#    substitute()d expression is always its bare-symbol default, never a
#    string, so it always takes resolve_col_name()'s deparse() path.


gradual_z <- function(data,
                      agedays  = agedays,
                      whowtz   = who_waz,
                      cdcwtz   = waz,
                      whohtz   = who_lhaz,
                      cdchtz   = haz,
                      whobmiz  = who_bmiz,
                      cdcbmiz  = bmiz)
{
  # 'data' must actually be supplied, and must be a type this function
  # knows how to handle -- otherwise the error would come from deep inside
  # data.table::as.data.table() instead of pointing at the real problem.
  if (missing(data) || is.null(data)) {
    stop("'data' is required.")
  }
  if (!inherits(data, c("data.frame", "data.table", "tbl_df"))) {
    stop("'data' must be a data.frame, data.table, or tibble.")
  }

  # Work on a data.table copy so we never modify the caller's object by
  # reference; type$data is that copy, type$input_was_df / $input_was_tibble
  # record what to hand back at the end.
  type <- as_data_table_safe(data)
  data <- type$data

  # Save the original column names before adding any internal columns, so
  # the output can restore this column order at the end. snapshot_names()
  # (not plain names()) is required: names(data) for a data.table is a
  # *live* reference to its names vector, and data.table's by-reference :=
  # calls below would otherwise silently make orig_cols "catch up" to
  # include every column added afterwards too.
  orig_cols <- snapshot_names(data)

  # Capture column-name arguments as strings. Each accepts either a bare
  # symbol (agedays = agedays) or a quoted string (agedays = "agedays") --
  # see the SHARED HELPERS note above and resolve_col_name() in
  # utils-anthro.R.
  col_agedays <- resolve_col_name(substitute(agedays))
  col_whowtz  <- resolve_col_name(substitute(whowtz))
  col_cdcwtz  <- resolve_col_name(substitute(cdcwtz))
  col_whohtz  <- resolve_col_name(substitute(whohtz))
  col_cdchtz  <- resolve_col_name(substitute(cdchtz))
  col_whobmiz <- resolve_col_name(substitute(whobmiz))
  col_cdcbmiz <- resolve_col_name(substitute(cdcbmiz))

  # agedays is the one column that's always required.
  if (!(col_agedays %in% names(data))) {
    stop(sprintf("Column '%s' (agedays) not found in 'data'.", col_agedays))
  }
  if (!is.numeric(data[[col_agedays]])) {
    stop(sprintf("Column '%s' (agedays) must be numeric.", col_agedays))
  }

  # Helper for the gradual z-score calculation.
  # who_wt and cdc_wt are linear in age (years) and sum to 3 over the
  # 2-5 year transition window, so blended is a proper convex combination.
  blend_z <- function(who, cdc, days) {
    who_wt  <- 5 - (days / 365.25)
    cdc_wt  <- (days / 365.25) - 2
    blended <- (who * who_wt / 3) + (cdc * cdc_wt / 3)
    data.table::fcase(
      days > 730 & days < 1826, blended,
      days <= 730,              who,
      days >= 1826,             cdc
    )
  }

  agedays_vec <- data[[col_agedays]]
  if (any(agedays_vec < 0, na.rm = TRUE)) {
    warning("Some 'agedays' values are negative; check your data.")
  }

  who_cols <- c(waz = col_whowtz,  haz = col_whohtz,  bmiz = col_whobmiz)
  cdc_cols <- c(waz = col_cdcwtz,  haz = col_cdchtz,  bmiz = col_cdcbmiz)
  out_cols <- c(waz = "grad_waz",  haz = "grad_haz",  bmiz = "grad_bmiz")

  # For each measure, look up its WHO/CDC/output column names from the
  # parallel vectors above (keyed by measure: waz, haz, bmiz). A gradual
  # z-score is only computed for a measure if BOTH its WHO and CDC columns
  # are present in 'data' -- an incomplete pair can't be blended, and a
  # measure that's entirely absent just wasn't supplied.
  computed <- character(0)
  for (m in names(who_cols)) {
    who_col     <- who_cols[[m]]
    cdc_col     <- cdc_cols[[m]]
    who_present <- who_col %in% names(data)
    cdc_present <- cdc_col %in% names(data)

    if (who_present && cdc_present) {
      # The same column supplied for both halves of a pair (typo, or a
      # duplicated argument) would otherwise silently "blend" a column
      # with itself and produce plausible-looking but wrong output.
      if (identical(who_col, cdc_col)) {
        stop(sprintf(
          "'%s' was supplied as both the WHO and CDC column for measure '%s' -- they must be different columns.",
          who_col, m
        ))
      }
      if (!is.numeric(data[[who_col]])) {
        stop(sprintf("Column '%s' (WHO %s) must be numeric.", who_col, m))
      }
      if (!is.numeric(data[[cdc_col]])) {
        stop(sprintf("Column '%s' (CDC %s) must be numeric.", cdc_col, m))
      }

      out_col <- out_cols[[m]]
      if (out_col %in% names(data)) {
        warning(sprintf("Overwriting existing column '%s'.", out_col))
      }
      data[, (out_col) := blend_z(data[[who_col]], data[[cdc_col]], agedays_vec)]
      computed <- c(computed, out_col)
    } else if (who_present || cdc_present) {
      # Only one half of the pair is present -- can't blend, so skip and
      # tell the caller why (this usually means a typo in a column name).
      warning(sprintf(
        "Skipping %s: found '%s' but not '%s' -- both WHO and CDC columns are required to compute a gradual z-score.",
        m, if (who_present) who_col else cdc_col, if (who_present) cdc_col else who_col
      ))
    } else {
      # Neither column is present -- that measure just wasn't supplied.
      # Not a warning (nothing is wrong), but worth a note for anyone
      # scanning logs in a pipeline.
      message(sprintf("Skipping %s: neither '%s' nor '%s' found in 'data'.", m, who_col, cdc_col))
    }
  }

  if (length(computed) == 0) {
    warning("No complete WHO/CDC z-score pair was found; no gradual z-scores were computed.")
  }

  # Reorder columns: any non-measurement original columns (e.g. agedays,
  # id variables) first, followed by each measure's
  # three columns grouped together in WHO, CDC, and gradual order -- weight,
  # then height, then BMI. intersect() preserves that fixed order while
  # silently dropping any column that wasn't present/computed, so this
  # works no matter which measures ended up available.
  weight_order <- intersect(c(col_whowtz,  col_cdcwtz,  "grad_waz"),  names(data))
  height_order <- intersect(c(col_whohtz,  col_cdchtz,  "grad_haz"),  names(data))
  bmi_order    <- intersect(c(col_whobmiz, col_cdcbmiz, "grad_bmiz"), names(data))

  # Exclude both the WHO/CDC input columns AND the possible output columns
  # (grad_*) from leading_cols. The output columns are placed
  # explicitly below, and if the input data already happened to contain a
  # column with one of those names (the overwrite case warned about
  # above), leaving it in leading_cols as well would list it twice and
  # make desired_order longer than the actual number of columns, which
  # setcolorder() below would error on.
  z_score_cols <- intersect(c(col_whowtz, col_cdcwtz, col_whohtz, col_cdchtz,
                              col_whobmiz, col_cdcbmiz), names(data))
  grad_cols    <- intersect(unname(out_cols), names(data))
  leading_cols <- setdiff(orig_cols, c(z_score_cols, grad_cols))
  desired_order <- c(leading_cols, weight_order, height_order, bmi_order)
  # Safety net: include anything unexpected that isn't already placed,
  # so no column is ever silently dropped from the output.
  desired_order <- c(desired_order, setdiff(names(data), desired_order))
  data.table::setcolorder(data, desired_order)

  # return same type of data that was input
  restore_input_type(data, type$input_was_df, type$input_was_tibble)
}
