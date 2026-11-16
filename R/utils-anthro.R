# =============================================================================
# Internal helpers shared by who_z() and cdc_z()
# =============================================================================
#
# Both functions take a data.frame/data.table/tibble of measurements, resolve
# a handful of bare-column-name arguments against it, normalize a sex column
# coded in various ways, and hand back a result in the same type they were
# given. That plumbing is identical in spirit between the WHO and CDC
# functions, so it lives here once instead of being maintained twice.
#
# None of these helpers change the algorithmic complexity of the code that
# calls them

# ---- column-name argument resolution ---------------------------------------

# Turn a captured argument expression into a column-name string. Accepts
# both a bare symbol (wt = weight) and a quoted string (wt = "weight").
# String literals are self-quoting in R, so substitute() already returns
# them as character values -- checking is.character() on the *unevaluated*
# expression distinguishes the two forms without ever evaluating anything,
# so a bare symbol that happens to collide with a variable name in the
# caller's environment is never mistakenly evaluated.
#
# IMPORTANT: this function does not call substitute() itself -- it can't.
# substitute() only sees how an argument was written one frame up from
# where it runs, so who_z()/cdc_z() must call substitute() on their
# own formal arguments and pass the resulting language object in here.
resolve_col_name <- function(expr) {
   if (is.character(expr)) expr else deparse(expr)
}

# Resolve one optional measurement argument end to end. `expr` must already
# be the result of substitute(the_arg) taken in the caller's own frame, and
# `was_missing` the result of missing(the_arg) taken there too.
#
# `expr` is resolved into a candidate column name via resolve_col_name()
# UNCONDITIONALLY -- including when was_missing is TRUE. That matters: a
# formal like `wt = wt` has a default expression that is itself just the
# symbol `wt`, and substitute(wt) returns that symbol whether or not the
# caller wrote `wt = ...` (missing() only reports whether they wrote it, not
# whether a default exists) -- so the default's own column name is checked
# against `data` here exactly the same way an explicitly supplied name
# would be. Previously this function short-circuited to var = NULL whenever
# was_missing was TRUE, which meant a column literally named 'wt' was never
# even looked up unless the caller redundantly wrote `wt = wt` themselves;
# age_var (resolved directly with resolve_col_name(substitute(age)) in
# cdc_z()/who_z(), with no missing() check at all) never had this problem,
# which is what exposed the inconsistency.
#
# If the argument WAS supplied (was_missing = FALSE) but names a column that
# doesn't exist, that's almost always a typo rather than a genuine "I don't
# have this measurement" -- so a warning is issued immediately, and the
# caller can use $typo to avoid also emitting a generic "not supplied"
# warning for the same problem. When was_missing = TRUE and the default's
# column doesn't exist either, that's NOT a typo -- the caller never named
# anything -- so no typo warning fires; has = FALSE lets the generic
# "missing" warnings in cdc_z()/who_z() cover it instead.
#
# Returns list(var = <name or NULL>, has = <logical>, typo = <logical>).
resolve_optional_var <- function(data, expr, was_missing, arg_label) {
   var  <- resolve_col_name(expr)
   has  <- var %in% names(data)
   typo <- !was_missing && !has
   if (typo) {
      warning("Argument '", arg_label, " = ", var, "' was supplied, but '",
              var, "' is not a column in data (check for a typo).")
   }
   list(var = if (has) var else NULL, has = has, typo = typo)
}

# ---- sex column -------------------------------------------------------------

# Locate and standardize the caller's sex column. It must be named some case
# variant of 'sex' (sex, SEX, Sex, ...); both 'sex' and 'SEX' present at once
# is ambiguous.
#
# Stops with a message specific to the actual problem -- "not found" vs.
# "more than one candidate" -- rather than one message covering both, so the
# caller knows exactly what to fix.
resolve_sex_column <- function(data) {
  nms <- grep('^sex$', names(data), ignore.case = TRUE, value = TRUE)
  if (length(nms) == 0) {
    stop("No sex column found. A child's sex column MUST be named some ",
         "case variant of 'sex' (e.g. 'sex', 'SEX', 'Sex').")
  }
  if (length(nms) > 1) {
    stop("Multiple candidate sex columns found: ", paste(nms, collapse = ", "),
         ". Only one column named some case variant of 'sex' may be ",
         "present in data.")
  }
  nms   # return the name; no renaming
}

# Validate a sex_code argument: c(male_code, female_code), the two raw
# values that mean "boy" / "girl" in the caller's sex column (default 1/2;
# e.g. c(0, 1) for a 0/1-coded column). Defined once here so any caller that
# accepts a custom sex coding -- currently normalize_sex() itself, and
# cdc_z()/who_z() for a fast, early check before doing any other work --
# shares one validation and one error message instead of drifting apart.
validate_sex_code <- function(sex_code) {
   if (!is.numeric(sex_code) || length(sex_code) != 2 || anyNA(sex_code) ||
       sex_code[1] == sex_code[2]) {
      stop("'sex_code' must be a length-2 numeric vector of two distinct, ",
           "non-NA values: c(male_code, female_code). Got: ",
           paste(sex_code, collapse = ", "))
   }
   invisible(sex_code)
}

# Sex codes:
# Recognized character codes (compared after trimws() and tolower()).
# Whole values are matched, not just the first letter, so that codes such
# as 'missing', 'mujer' or 'not stated' become NA instead of being read as
# a sex from their first letter.
.male_words   <- c("m", "male", "males", "b", "boy", "boys")
.female_words <- c("f", "female", "females", "g", "girl", "girls")

# Map a vector of sex codes to 1L (boys) / 2L (girls) / NA_integer_.
#
# Numeric sx: sex_code[1] -> 1L, sex_code[2] -> 2L.
# Character/factor sx: the two sex_code values written as text (e.g. "1",
# "2"), or the words in .male_words / .female_words, case-insensitive.
#
# Non-missing values that match neither code become NA, and ONE warning
# reports how many rows that affected and which values were seen, so a
# coding problem cannot silently drop children from an analysis.
#
# Character input maps only the *unique* values and expands back with
# chmatch(), so the string work runs on a handful of distinct codes.
normalize_sex <- function(sx, sex_code = 1:2) {
  validate_sex_code(sex_code)
  if (is.numeric(sx)) {
    out <- fcase(sx == sex_code[1], 1L, sx == sex_code[2], 2L,
                 default = NA_integer_)
  } else {
    sx    <- as.character(sx)
    usex  <- unique(sx)
    key   <- tolower(trimws(usex))
    male_codes   <- c(format(sex_code[1], scientific = FALSE), .male_words)
    female_codes <- c(format(sex_code[2], scientific = FALSE), .female_words)
    usexn <- fifelse(key %chin% male_codes, 1L,
                     fifelse(key %chin% female_codes, 2L, NA_integer_))
    out <- usexn[chmatch(sx, usex)]
  }
  bad <- !is.na(sx) & is.na(out)
  if (any(bad) && !all(is.na(out))) {
    vals <- unique(sx[bad])
    warning(sum(bad), " row(s) have an unrecognized sex code (",
            paste0("'", utils::head(vals, 5), "'", collapse = ", "),
            if (length(vals) > 5) ", ..." else "",
            "); their results are NA. See 'Details' or 'sex_code' in the help page.",
            call. = FALSE)
  }
  out
}

as_data_table_safe <- function(data) {
  input_was_tibble <- inherits(data, "tbl_df")
  input_was_df     <- inherits(data, "data.frame") &&
    !inherits(data, "data.table") && !input_was_tibble

  data <- data.table::setDT(as.list(data))

  list(
    data = data,
    input_was_df = input_was_df,
    input_was_tibble = input_was_tibble
  )
}

# set 0 and negative values of the various measures to NA
set_nonpositive_na <- function(data, cols) {
  cols <- intersect(cols, names(data))

  for (j in cols) {
    x <- data[[j]]
    i <- which(x <= 0)

    if (length(i))
      data.table::set(data, i = i, j = j, value = rep(NA_real_, length(i)) )
  }
  invisible(data)
}

# A measurement column is usable if it is numeric, or if it is entirely NA.
# An all-NA column such as data.frame(ht = NA) is logical, not numeric, but
# simply means "no values"; num_col() turns it into NA_real_.
is_num_or_all_na <- function(x) is.numeric(x) || (is.logical(x) && all(is.na(x)))
num_col <- function(x) if (is.numeric(x)) x else as.numeric(x)

# Inverse of as_data_table_safe(): restore the caller's original type.
restore_input_type <- function(data, input_was_df, input_was_tibble) {
  if (input_was_df) return(setDF(data))
  if (input_was_tibble) {
    if (requireNamespace("tibble", quietly = TRUE)) {
      return(tibble::as_tibble(data))
    }
    warning("tibble package not installed; returning a data.frame instead.")
    return(as.data.frame(data))
  }
  data[]
}

