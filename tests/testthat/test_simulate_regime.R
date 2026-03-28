# ── cedm_simulate ────────────────────────────────────────────

test_that("cedm_simulate returns correct structure", {
  sim <- cedm_simulate(n = 300, seed = 42)

  expect_s3_class(sim, "data.frame")
  expect_true(all(c("id", "SES", "health", "achievement", "regime") %in%
                    names(sim)))
  expect_equal(nrow(sim), 300)
  expect_equal(nlevels(sim$regime), 3)
  expect_true(all(levels(sim$regime) %in%
                    c("neutral", "amplifying", "compensatory")))
})

test_that("cedm_simulate longitudinal adds wave column", {
  sim <- cedm_simulate(n = 300, n_waves = 3, seed = 42)

  expect_true("wave" %in% names(sim))
  expect_equal(nrow(sim), 300 * 3)
  expect_equal(sort(unique(sim$wave)), 1:3)
})

test_that("cedm_simulate is reproducible with same seed", {
  sim1 <- cedm_simulate(n = 300, seed = 99)
  sim2 <- cedm_simulate(n = 300, seed = 99)

  expect_equal(sim1$SES,         sim2$SES)
  expect_equal(sim1$achievement, sim2$achievement)
})

test_that("cedm_simulate DGP encodes correct interaction signs", {
  sim <- cedm_simulate(
    n                  = 3000,
    beta3_amplifying   = 0.60,
    beta3_compensatory = -0.30,
    beta3_neutral      = 0.00,
    seed               = 42
  )

  amp  <- sim[sim$regime == "amplifying",  ]
  comp <- sim[sim$regime == "compensatory", ]

  # SES x health correlation with achievement should be
  # stronger (more positive) in amplifying than compensatory regime
  cor_amp  <- cor(amp$SES  * amp$health,  amp$achievement)
  cor_comp <- cor(comp$SES * comp$health, comp$achievement)

  expect_gt(cor_amp, cor_comp)
})

# ── classify_regime ───────────────────────────────────────────

test_that("classify_regime creates cedm_regime column", {
  df <- data.frame(ses = rnorm(300), opp = rnorm(300))
  df <- classify_regime(df, ses_var = "ses", opportunity_var = "opp")

  expect_true("cedm_regime" %in% names(df))
  expect_true(all(df$cedm_regime %in%
                    c("amplifying", "neutral", "compensatory")))
})

test_that("classify_regime works with ses_tertiles = TRUE", {
  df <- data.frame(ses = rnorm(300))
  df <- classify_regime(df, ses_var = "ses", ses_tertiles = TRUE)

  expect_true("cedm_regime" %in% names(df))
  expect_equal(length(unique(df$cedm_regime)), 3)
})

test_that("classify_regime errors on missing variable", {
  df <- data.frame(ses = rnorm(100))

  # Actual error: "opportunity_var 'opp' not found in data"
  expect_error(
    classify_regime(df, ses_var = "ses", opportunity_var = "opp"),
    "not found"
  )
})
