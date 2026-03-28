make_df <- function(n = 200, seed = 42) {
  set.seed(seed)
  df <- data.frame(
    math = rnorm(n, 500, 100),
    ses  = rnorm(n),
    bmi  = rnorm(n, 25, 5),
    opp  = rnorm(n)
  )
  classify_regime(df, ses_var = "ses", opportunity_var = "opp")
}

# ── plot_regimes ─────────────────────────────────────────────

test_that("plot_regimes returns a ggplot object", {
  df <- make_df()
  p  <- plot_regimes(df, ses_var = "ses", health_var = "bmi")

  expect_s3_class(p, "ggplot")
})

test_that("plot_regimes works with outcome_var", {
  df <- make_df()
  p  <- plot_regimes(df, ses_var = "ses", health_var = "bmi",
                     outcome_var = "math")

  expect_s3_class(p, "ggplot")
})

test_that("plot_regimes errors on missing variable", {
  df <- make_df()
  expect_error(
    plot_regimes(df, ses_var = "MISSING", health_var = "bmi"),
    "Missing"
  )
})

# ── plot_cedm_interaction ────────────────────────────────────

test_that("plot_cedm_interaction returns a ggplot object", {
  df   <- make_df()
  prod <- cedm_production(df, outcome_var = "math",
                          ses_var = "ses", health_var = "bmi",
                          regime_var = "cedm_regime",
                          model = "regime", center = TRUE)
  p <- plot_cedm_interaction(
    cedm_prod_result = prod,
    data             = df,
    ses_var          = "ses",
    health_var       = "bmi",
    regime_var       = "cedm_regime",
    outcome_var      = "math"
  )

  expect_s3_class(p, "ggplot")
})

# ── cedm_spline_moderation ───────────────────────────────────

test_that("cedm_spline_moderation returns correct class", {
  df     <- make_df()
  result <- cedm_spline_moderation(df, outcome_var = "math",
                                   ses_var = "ses", health_var = "bmi",
                                   plot = FALSE)

  expect_s3_class(result, "cedm_spline")
})

test_that("cedm_spline_moderation fitted model is not NULL", {
  df     <- make_df()
  result <- cedm_spline_moderation(df, outcome_var = "math",
                                   ses_var = "ses", health_var = "bmi",
                                   plot = FALSE)

  expect_true(!is.null(result$model))
})

test_that("cedm_spline_moderation generates plot when plot = TRUE", {
  df     <- make_df()
  result <- cedm_spline_moderation(df, outcome_var = "math",
                                   ses_var = "ses", health_var = "bmi",
                                   plot = TRUE)

  expect_s3_class(result$plot, "ggplot")
})

test_that("cedm_spline_moderation print method runs without error", {
  df     <- make_df()
  result <- cedm_spline_moderation(df, outcome_var = "math",
                                   ses_var = "ses", health_var = "bmi",
                                   plot = FALSE)
  expect_output(print(result))
})
