test_that("cedm_sensitivity returns correct class and columns", {
  set.seed(42)
  df   <- data.frame(math = rnorm(300, 500, 100),
                     ses  = rnorm(300),
                     bmi  = rnorm(300, 25, 5))
  fit  <- lm(math ~ ses * bmi, data = df)
  sens <- cedm_sensitivity(fit, term = "ses")

  expect_s3_class(sens, "cedm_sensitivity")
  expect_true(all(c("term", "estimate", "t_value", "p_value",
                    "itcv", "rir_pct", "rir_n",
                    "stability", "cedm_interpretation") %in% names(sens)))
})

test_that("cedm_sensitivity ITCV is non-negative", {
  set.seed(42)
  df   <- data.frame(math = rnorm(300, 500, 100),
                     ses  = rnorm(300),
                     bmi  = rnorm(300, 25, 5))
  fit  <- lm(math ~ ses * bmi, data = df)
  sens <- cedm_sensitivity(fit)

  expect_true(all(sens$itcv >= 0))
})

test_that("cedm_sensitivity RIR is in [0, 100]", {
  set.seed(42)
  df   <- data.frame(math = rnorm(300, 500, 100),
                     ses  = rnorm(300),
                     bmi  = rnorm(300, 25, 5))
  fit  <- lm(math ~ ses * bmi, data = df)
  sens <- cedm_sensitivity(fit)

  expect_true(all(sens$rir_pct >= 0))
  expect_true(all(sens$rir_pct <= 100))
})

test_that("cedm_sensitivity errors on unknown term", {
  set.seed(42)
  df  <- data.frame(math = rnorm(200), ses = rnorm(200))
  fit <- lm(math ~ ses, data = df)

  expect_error(
    cedm_sensitivity(fit, term = "nonexistent_term"),
    "not found"
  )
})

test_that("cedm_sensitivity print method does not recurse infinitely", {
  set.seed(42)
  df   <- data.frame(math = rnorm(300, 500, 100),
                     ses  = rnorm(300),
                     bmi  = rnorm(300, 25, 5))
  fit  <- lm(math ~ ses * bmi, data = df)
  sens <- cedm_sensitivity(fit, term = "ses")

  # If recursion bug exists this would throw a stack overflow error
  expect_output(print(sens))
})

test_that("cedm_sensitivity works with cedm_production output", {
  set.seed(42)
  df <- data.frame(math = rnorm(300, 500, 100),
                   ses  = rnorm(300),
                   bmi  = rnorm(300, 25, 5))
  df   <- classify_regime(df, ses_var = "ses", ses_tertiles = TRUE)
  prod <- cedm_production(df, outcome_var = "math",
                          ses_var = "ses", health_var = "bmi",
                          model = "base")
  sens <- cedm_sensitivity(prod)

  expect_s3_class(sens, "cedm_sensitivity")
  expect_gt(nrow(sens), 0)
})
