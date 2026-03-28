# Shared longitudinal test data
make_long_df <- function(n_ids = 100, n_waves = 5, seed = 42) {
  set.seed(seed)
  data.frame(
    id     = rep(seq_len(n_ids), each = n_waves),
    wave   = rep(seq_len(n_waves), times = n_ids),
    math   = rnorm(n_ids * n_waves, 500, 100),
    bmi    = rnorm(n_ids * n_waves, 25, 5),
    ses    = rep(rnorm(n_ids), each = n_waves),
    school = rep(sample(1:10, n_ids, replace = TRUE), each = n_waves)
  )
}

# ── cedm_rewb ────────────────────────────────────────────────

test_that("cedm_rewb returns correct class and structure", {
  df     <- make_long_df()
  result <- cedm_rewb(df, outcome_var = "math", health_var = "bmi",
                      id_var = "id", time_var = "wave", ses_var = "ses")

  expect_s3_class(result, "cedm_rewb")
  expect_true(!is.null(result$model))
  expect_true(!is.null(result$between_effect))
  expect_true(!is.null(result$within_effect))
  expect_true(!is.null(result$interpretation))
})

test_that("cedm_rewb creates between and within columns", {
  df     <- make_long_df()
  result <- cedm_rewb(df, outcome_var = "math", health_var = "bmi",
                      id_var = "id", time_var = "wave", ses_var = "ses")

  expect_true("bmi_between" %in% names(result$data_decomposed))
  expect_true("bmi_within"  %in% names(result$data_decomposed))
})

test_that("cedm_rewb within component sums to ~0 within person", {
  df     <- make_long_df()
  result <- cedm_rewb(df, outcome_var = "math", health_var = "bmi",
                      id_var = "id", time_var = "wave", ses_var = "ses")

  within_means <- tapply(result$data_decomposed$bmi_within,
                         result$data_decomposed$id, mean)
  expect_true(all(abs(within_means) < 1e-10))
})

test_that("cedm_rewb errors on missing variable", {
  df <- make_long_df()
  expect_error(
    cedm_rewb(df, outcome_var = "math", health_var = "bmi",
              id_var = "id", time_var = "MISSING", ses_var = "ses"),
    "Missing"
  )
})

test_that("cedm_rewb print method runs without error", {
  df     <- make_long_df()
  result <- cedm_rewb(df, outcome_var = "math", health_var = "bmi",
                      id_var = "id", time_var = "wave", ses_var = "ses")
  expect_output(print(result))
})

# ── cedm_trajectory ──────────────────────────────────────────

test_that("cedm_trajectory returns correct class and k clusters", {
  df     <- make_long_df()
  result <- cedm_trajectory(df, health_var = "bmi", id_var = "id",
                             time_var = "wave", k = 3, plot = FALSE)

  expect_s3_class(result, "cedm_trajectory")
  expect_equal(result$k, 3)
  expect_equal(nrow(result$cluster_summary), 3)
})

test_that("cedm_trajectory cluster assignment covers all ids", {
  df     <- make_long_df(n_ids = 100, n_waves = 5)
  result <- cedm_trajectory(df, health_var = "bmi", id_var = "id",
                             time_var = "wave", k = 3, plot = FALSE)

  expect_equal(nrow(result$cluster_assignment), 100)
})

test_that("cedm_trajectory hierarchical method works", {
  df     <- make_long_df()
  result <- cedm_trajectory(df, health_var = "bmi", id_var = "id",
                             time_var = "wave", k = 3,
                             method = "hierarchical", plot = FALSE)

  expect_s3_class(result, "cedm_trajectory")
  expect_equal(result$method, "hierarchical")
})

test_that("cedm_trajectory generates ggplot when plot = TRUE", {
  df     <- make_long_df()
  result <- cedm_trajectory(df, health_var = "bmi", id_var = "id",
                             time_var = "wave", k = 3, plot = TRUE)

  expect_s3_class(result$plot, "ggplot")
})

test_that("cedm_trajectory print method runs without error", {
  df     <- make_long_df()
  result <- cedm_trajectory(df, health_var = "bmi", id_var = "id",
                             time_var = "wave", k = 3, plot = FALSE)
  expect_output(print(result))
})
