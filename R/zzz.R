.datatable.aware <- TRUE

# ---------------------------------------------------------------------------
# Column names used inside data.table's non-standard evaluation.
#
# data.table evaluates column names inside DT[...], so R CMD check sees them as
# undefined global variables. Declaring them here silences the "no visible
# binding for global variable" NOTEs. Both cdcanthro() and whoanthro() are
# covered; names shared by the two functions are listed once, under "shared".
# ---------------------------------------------------------------------------
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(

    ## ---- shared by both functions -------------------------------------
    # data.table syntax
    '.', '..v', '..keep', 'i.wflz', 'i.wt',
    # row ordering, sex, agemos
    'seq__', 'sexn', 'sex', 'agemos', 'sex_n', 'sex_code',
    # measurements
    'wt', 'bmi',
    # BMI z-scores / LMS columns common to both
    'bmiz', 'bmi_l', 'bmi_m', 'bmi_s',
    # weight-for-age z-score (both functions produce 'waz')
    'waz',

    ## ---- cdc_z() --------------------------------------------------
    # demographic and body size data
    'age', 'ht',
    # LMS columns from the CDC reference data
    'wl', 'wm', 'ws', 'bl', 'bm', 'bs', 'hl', 'hm', 'hs',
    # pre-computed SD limits for the modified z-score
    'wt_sdm2', 'wt_sdp2', 'ht_sdm2', 'ht_sdp2', 'bmi_sdm2', 'bmi_sdp2',
    # reference constants and pre-computed values
    'mref', 'sref', 'p95', 'sigma',
    # weight and height z-scores and percentiles
    'mod_waz', 'wap', 'haz', 'mod_haz', 'hap',
    # BMI z-scores and percentiles
    'bz', 'mod_bmiz', 'bmip', 'orig_bmiz', 'orig_bmip', 'ebz', 'ebp',
    # BMI derived metrics
    'p50', 'z1', 'z0', 'perc_median', 'bmip95',
    'dist_median', 'adj_dist_median', 'adj_perc_median',
    'log_perc_median', 'adj_log_perc_median',
    # temporary variables
    'inv_bl_bs', 'inv_wl_ws', 'inv_hl_hs', 'bmi_ratio',
    # package dataset
    'cdc_ref_data',

    ## ---- who_z() --------------------------------------------------
    # input columns
    'agedays', 'lenhei', 'headc', 'len_hei', 'head_circ',
    # reference table keys and internal columns
    'denom', 'len', 'len_2',
    # LMS columns from the WHO reference data
    'wei_l', 'wei_m', 'wei_s',
    'len_l', 'len_m', 'len_s',
    'headc_l', 'headc_m', 'headc_s',
    'wfl_l', 'wfl_m', 'wfl_s',
    # computed z-scores
    'who_waz', 'who_lhaz', 'who_bmiz', 'who_wflz', 'who_headcz',
    # biologically implausible value (BIV) flags
    'fwaz', 'flhaz', 'fwflz', 'fbmiz', 'fheadcz',
    # package dataset
    'who_ref_data',

    ## ------ gradual_z ---------------------------------------------------
    'grad_waz', 'grad_haz', 'grad_bmiz', 'ageyears', 'lhaz', 'headcz', 'wflz',

    ## other stuff -----------
    'z', 'sdp3', 'sdp2', 'sdm3', 'sdm2'

  ))
}
