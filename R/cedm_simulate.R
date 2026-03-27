#' Simulate Data Under CEDM Ecological Capability Regimes
#'
#' Generates synthetic longitudinal data from the CEDM toy data-generating
#' process, replicating Appendix A of Hait (2025). This simulation encodes
#' three ecological capability regimes and confirms the CEDM's core prediction:
#' weak mediation (small indirect effect) alongside strong and sign-varying
#' SES x health moderation across regimes.
#'
#' @param n Integer: total sample size. Default 3000 (1000 per regime).
#' @param alpha1 Numeric: SES -> health effect (small = weak mediation). Default 0.15.
#' @param beta1 Numeric: main SES -> achievement effect. Default 0.50.
#' @param beta2 Numeric: main health -> achievement effect. Default 0.10.
#' @param beta3_neutral Numeric: SES x health interaction in neutral regime.
#'   Default 0.00.
#' @param beta3_amplifying Numeric: SES x health interaction in amplifying regime.
#'   Default 0.60 (positive = health amplifies low-SES disadvantage when SES is
#'   negatively coded, or use negative values depending on your parameterization).
#' @param beta3_compensatory Numeric: SES x health interaction in compensatory
#'   regime. Default -0.30.
#' @param alpha_reg_amplifying Numeric: regime-specific intercept shift for health
#'   in amplifying regime. Default 0.30.
#' @param alpha_reg_compensatory Numeric: regime-specific intercept shift for
#'   health in compensatory regime. Default -0.30.
#' @param n_waves Integer: number of longitudinal waves to generate. Default 1
#'   (cross-sectional). Set > 1 for longitudinal data.
#' @param seed Integer: random seed. Default 123.
#'
#' @return A data.frame with columns: \code{id}, \code{SES}, \code{health},
#'   \code{achievement}, \code{regime}, and (if \code{n_waves > 1}) \code{wave}.
#'
#' @details
#' The data-generating process is:
#' \deqn{Health_i = \alpha_0 + \alpha_1 SES_i + \alpha_{regime} + \varepsilon_{M}}
#' \deqn{Achievement_i = \beta_0 + \beta_1 SES_i + \beta_2 Health_i +
#'   \beta_{3,regime} SES_i \times Health_i + \varepsilon_Y}
#' where regime-specific parameters encode amplifying, neutral, and compensatory
#' ecological contexts.
#'
#' @references
#' Hait, S. (2025). Socioeconomic Status, Health, and Academic Achievement:
#' A Capability-Ecological Developmental Model. Appendix A.
#'
#' @examples
#' sim_data <- cedm_simulate(n = 3000, seed = 42)
#' table(sim_data$regime)
#' head(sim_data)
#'
#' # Run full CEDM analysis on simulated data
#' sim_data <- classify_regime(sim_data, ses_var = "SES",
#'                             opportunity_var = NULL, ses_tertiles = TRUE)
#' prod <- cedm_production(sim_data, outcome_var = "achievement",
#'                         ses_var = "SES", health_var = "health",
#'                         regime_var = "cedm_regime", model = "regime")
#' print(prod)
#'
#' @export
cedm_simulate <- function(n                      = 3000,
                           alpha1                 = 0.15,
                           beta1                  = 0.50,
                           beta2                  = 0.10,
                           beta3_neutral          = 0.00,
                           beta3_amplifying       = 0.60,
                           beta3_compensatory     = -0.30,
                           alpha_reg_amplifying   = 0.30,
                           alpha_reg_compensatory = -0.30,
                           n_waves                = 1,
                           seed                   = 123) {

  set.seed(seed)

  n_per_regime <- floor(n / 3)
  n_total      <- n_per_regime * 3

  regime <- factor(rep(c("neutral", "amplifying", "compensatory"),
                       each = n_per_regime),
                   levels = c("neutral", "amplifying", "compensatory"))

  SES <- stats::rnorm(n_total, 0, 1)

  alpha_reg <- c(
    neutral      = 0.00,
    amplifying   = alpha_reg_amplifying,
    compensatory = alpha_reg_compensatory
  )

  health_raw <- alpha1 * SES + alpha_reg[as.character(regime)] + stats::rnorm(n_total, 0, 1)
  health     <- as.numeric(scale(health_raw))

  beta3_reg <- c(
    neutral      = beta3_neutral,
    amplifying   = beta3_amplifying,
    compensatory = beta3_compensatory
  )

  achievement_raw <- beta1 * SES + beta2 * health +
    beta3_reg[as.character(regime)] * SES * health +
    stats::rnorm(n_total, 0, 1)
  achievement <- as.numeric(scale(achievement_raw))

  out <- data.frame(
    id          = seq_len(n_total),
    SES         = SES,
    health      = health,
    achievement = achievement,
    regime      = regime
  )

  if (n_waves > 1) {
    out_list <- lapply(1:n_waves, function(w) {
      d <- out
      # Add within-person time variation
      d$achievement <- d$achievement +
        stats::rnorm(n_total, 0, 0.3) + w * 0.1  # slight growth
      d$health      <- d$health + stats::rnorm(n_total, 0, 0.1)
      d$wave        <- w
      d
    })
    out <- do.call(rbind, out_list)
    out$id <- rep(seq_len(n_total), times = n_waves)
    out    <- out[order(out$id, out$wave), ]
  }

  message(sprintf(
    "CEDM simulation: n = %d (%d per regime) | alpha1 = %.2f | beta3_amplifying = %.2f",
    n_total, n_per_regime, alpha1, beta3_amplifying
  ))

  return(out)
}
