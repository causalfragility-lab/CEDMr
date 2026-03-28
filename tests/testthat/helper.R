# tests/testthat/helper.R
# Shared fixtures available to all test files.
# testthat automatically sources files named helper*.R

#' Small cross-sectional test dataset with regime classification
make_test_df <- function(n = 200, seed = 42) {
  set.seed(seed)
  df <- data.frame(
    math   = rnorm(n, 500, 100),
    ses    = rnorm(n),
    bmi    = rnorm(n, 25,  5),
    opp    = rnorm(n),
    school = sample(1:20, n, replace = TRUE)
  )
  classify_regime(df, ses_var = "ses", opportunity_var = "opp")
}

#' Small longitudinal test dataset
make_test_long_df <- function(n_ids = 80, n_waves = 4, seed = 42) {
  set.seed(seed)
  data.frame(
    id     = rep(seq_len(n_ids), each = n_waves),
    wave   = rep(seq_len(n_waves), times = n_ids),
    math   = rnorm(n_ids * n_waves, 500, 100),
    bmi    = rnorm(n_ids * n_waves,  25,   5),
    ses    = rep(rnorm(n_ids), each = n_waves),
    school = rep(sample(1:10, n_ids, replace = TRUE), each = n_waves)
  )
}
