# Shared test data
make_df <- function(n = 200, seed = 42) {
  set.seed(seed)
  df <- data.frame(
    math   = rnorm(n, 500, 100),
    ses    = rnorm(n),
    bmi    = rnorm(n, 25, 5),
    school = sample(1:20, n, replace = TRUE)
  )
  classify_regime(df, ses_var = "ses", ses_tertiles = TRUE)
}

test_that("cedm_production base model returns correct class", {
  df     <- make_df()
  result <- cedm_production(df, outcome_var = "math",
                            ses_var = "ses", health_var = "bmi",
                            model = "base")

  expect_s3_class(result, "cedm_production")
  expect_true(!is.null(result$model))
  expect_true(!is.null(result$coefficients))
  expect_true(!is.null(result$formula))
})

test_that("cedm_production base model contains SES x health interaction", {
  df     <- make_df()
  result <- cedm_production(df, outcome_var = "math",
                            ses_var = "ses", health_var = "bmi",
                            model = "base", center = TRUE)

  coef_terms <- result$coefficients$term
  expect_true(any(grepl(":", coef_terms)))
})

test_that("cedm_production regime model contains three-way interaction", {
  df     <- make_df()
  result <- cedm_production(df, outcome_var = "math",
                            ses_var = "ses", health_var = "bmi",
                            regime_var = "cedm_regime",
                            model = "regime", center = TRUE)

  coef_terms <- result$coefficients$term
  # Should have at least one term with two colons (three-way interaction)
  expect_true(any(sapply(coef_terms, function(t)
    length(gregexpr(":", t)[[1]]) >= 2)))
})

test_that("cedm_production additive model has no interaction", {
  df     <- make_df()
  result <- cedm_production(df, outcome_var = "math",
                            ses_var = "ses", health_var = "bmi",
                            model = "additive", center = TRUE)

  coef_terms <- result$coefficients$term
  expect_false(any(grepl(":", coef_terms)))
})

test_that("cedm_production errors on missing outcome variable", {
  df <- make_df()
  expect_error(
    cedm_production(df, outcome_var = "nonexistent",
                    ses_var = "ses", health_var = "bmi"),
    regexp = "."   # any error
  )
})

test_that("cedm_production print method runs without error", {
  df     <- make_df()
  result <- cedm_production(df, outcome_var = "math",
                            ses_var = "ses", health_var = "bmi",
                            model = "base")
  expect_output(print(result))
})
