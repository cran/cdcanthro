# =============================================================================
# Internal helpers shared by whoanthro() and cdcanthro()
# =============================================================================
#
# Both functions take a data.frame/data.table/tibble of measurements, resolve
# a handful of bare-column-name arguments against it, normalize a sex column
# coded in various ways, and hand back a result in the same type they were
# given. That plumbing is identical in spirit between the WHO and CDC
# functions, so it lives here once instead of being maintained twice.
#
# None of these helpers change the algorithmic complexity of the code that
# calls them -- each still operates on the same vectors/columns the inline
# code used to -- so factoring them out here has no effect on speed or
# memory use. They are not exported; they exist only for whoanthro() and
# cdcanthro() to share.


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
# where it runs, so whoanthro()/cdcanthro() must call substitute() on their
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
# is ambiguous. Renames the matched column to lowercase 'sex' in place
# (data.table by-reference) and returns `data` invisibly.
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
   if (nms != 'sex') data.table::setnames(data, old = nms, new = 'sex')
   invisible(data)
}

# Validate a sex_code argument: c(male_code, female_code), the two raw
# values that mean "boy" / "girl" in the caller's sex column (default 1/2;
# e.g. c(0, 1) for a 0/1-coded column). Defined once here so any caller that
# accepts a custom sex coding -- currently normalize_sex() itself, and
# cdc_z()/whoanthro() for a fast, early check before doing any other work --
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

# Map a vector of sex codes to 1L (boys) / 2L (girls) / NA_integer_.
#
# sex_code = c(male_code, female_code) says what "boy"/"girl" look like in a
# *numeric* sx (default 1/2; pass e.g. 0:1 for a 0/1-coded column). The same
# two values are also honored as the leading digit of a *character* code, so
# a caller using a non-default sex_code doesn't have two different codings
# to remember for numeric vs. character sex columns. Letter codes B/M for
# boys and G/F for girls are always recognized regardless of sex_code, since
# they're unambiguous on their own.
#
# Numeric input takes a fast path via fcase(). Character input maps only the
# *unique* values in `sx` and expands back with chmatch(), so
# substr()/toupper() run on a handful of distinct codes instead of on every
# row.
normalize_sex <- function(sx, sex_code = 1:2) {
   validate_sex_code(sex_code)
   if (is.numeric(sx)) {
      return(fcase(sx == sex_code[1], 1L, sx == sex_code[2], 2L, default = NA_integer_))
   }
   sx    <- as.character(sx)
   usex  <- unique(sx)
   fc    <- toupper(substr(usex, 1L, 1L))
   male_codes   <- c(as.character(sex_code[1]), "B", "M")
   female_codes <- c(as.character(sex_code[2]), "G", "F")
   usexn <- fifelse(fc %chin% male_codes, 1L,
                     fifelse(fc %chin% female_codes, 2L, NA_integer_))
   usexn[chmatch(sx, usex)]
}

# ---- input type preservation -------------------------------------------------

# Snapshot the input's class and return a data.table working copy, built
# entirely from data.table's PUBLIC API (no data.table:::shallow(), which an
# earlier version of this function used -- it avoids duplicating column
# data, but it's unexported, triggers an R CMD check NOTE ["Unexported
# object imported by a ':::' call"], and carries no compatibility promise:
# github.com/Rdatatable/data.table/issues/2323, open since 2017, is
# literally a request to make it public).
#
# copy = TRUE (default) always hands back an independent data.table, so the
# caller's own `data` is never touched, mutated, or left in a half-updated
# state if who_z()/cdc_z() errors out partway through -- see the COPY
# ARGUMENT note in who_z.R/cdc_z.R. It's built the cheap way for EITHER
# input type: base R's as.list() returns a new, independent list holding
# the SAME column vectors (no data duplicated -- verified: same column
# addresses before/after, and orders of magnitude faster than a real
# duplicate at scale), and data.table::setDT() converts THAT list to a
# data.table by reference. `data` itself -- whether it came in as a
# data.frame, tibble, or already a data.table -- is never passed to
# setDT() and so is never touched, in class or in content. (The one thing
# this loses that data.table::copy() would keep: a key or secondary index
# set on an incoming data.table. who_z()/cdc_z() never rely on incoming
# data being keyed, so that's not a correctness issue here -- just worth
# knowing if you ever pass in a data.table you deliberately keyed first.)
#
# copy = FALSE skips that and mutates the caller's own `data` by reference
# instead -- the cheapest possible option, and data.table's own normal
# idiom (this is exactly what := and set() do throughout the rest of
# data.table). This ONLY works when `data` is ALREADY a data.table. Tested,
# not assumed: for a data.frame/tibble input, data.table::setDT() has its
# own, separate protection against converting an object in place when it
# might be referenced elsewhere (which, being a function argument, it
# always is), so it takes a full, independent duplicate regardless of what
# `copy` asks for -- there's no public-API way around that, and calling
# setDT() directly on `data` to try would only add a confusing side effect
# (data`'s own class silently becoming "data.table" even though its
# columns never actually changed). So for a data.frame/tibble input this
# function always takes the same safe, independent as.list()-based path
# above regardless of `copy`, and copy = FALSE emits a message() saying so.
# For a data.table input, by contrast, plain set()/:=/setnames() on the
# EXISTING object always mutate it by reference with no such protection --
# so skipping the copy step entirely is enough, and reliably works: new
# columns who_z()/cdc_z() add really do land on the object the caller
# passed in, and if who_z()/cdc_z() errors out partway through, whatever
# had already been done to it by that point is left in place, since
# set()/:= are not transactional. Callers who want guaranteed by-reference
# mutation for a data.frame/tibble should convert once themselves first
# with data.table::setDT(your_data) and pass the resulting data.table in.
#
# Returns list(data = <data.table>, input_was_df = <logical>,
#   input_was_tibble = <logical>).
as_data_table_safe <- function(data, copy = TRUE) {
   input_was_tibble <- inherits(data, "tbl_df")
   input_was_df     <- inherits(data, "data.frame") &&
      !inherits(data, "data.table") && !input_was_tibble

   if (is.data.table(data) && !copy) {
      # Use `data` as-is; every set()/setnames()/setcolorder() downstream
      # then mutates the caller's own table by reference.
   } else {
      if (!is.data.table(data) && !copy)
         message(
            "copy = FALSE has no effect for a data.frame/tibble input: ",
            "data.table::setDT() always protects a data.frame from being ",
            "converted in place when it might be referenced elsewhere (as ",
            "a function argument always is). For guaranteed by-reference ",
            "mutation, convert once yourself first with ",
            "data.table::setDT(your_data) and pass the resulting ",
            "data.table in."
         )
      data <- data.table::setDT(as.list(data))
   }

   list(data = data, input_was_df = input_was_df, input_was_tibble = input_was_tibble)
}

# Inverse of as_data_table_safe(): restore the caller's original type.
restore_input_type <- function(data, input_was_df, input_was_tibble) {
   if (input_was_df) return(as.data.frame(data))
   if (input_was_tibble) {
      if (requireNamespace("tibble", quietly = TRUE)) {
         return(tibble::as_tibble(data))
      }
      warning("tibble package not installed; returning a data.frame instead.")
      return(as.data.frame(data))
   }
   data[]
}

# ---- defensive column-name snapshot ------------------------------------------

# Take a genuinely independent snapshot of a data.table's column names.
#
# names(DT) for a data.table returns its *live* names vector, not a copy.
# data.table's by-reference operations (set(), setnames(), :=) mutate that
# same vector in place for speed -- so a plain `nm <- names(DT)` taken early
# in a function can silently "catch up" to reflect columns added much later,
# which makes a subsequent setdiff(names(DT), nm) wrongly return nothing.
# (This is exactly what made whoanthro()'s old added-column cleanup a
# no-op -- see whoanthro.R's history / commit message for the repro.)
# Wrapping in data.table::copy() forces a real, independent copy.
snapshot_names <- function(data) {
   data.table::copy(names(data))
}
