#' CEDM Causal Mediation Analysis
#'
#' Tests whether a health indicator (e.g., BMI) mediates the relationship between
#' SES and academic achievement using the counterfactual framework of Imai et al.
#' (2010). Implements the doubly robust design used in Hait (2026) with
#' nonparametric bootstrap confidence intervals and built-in sensitivity analysis.
#' Consistent with CEDM Proposition 1: mediation effects are expected to be
#' small and unstable; moderation (see \code{cedm_production}) is expected
#' to dominate.
#'
#' @param data A data.frame.
#' @param outcome_var Character string: dependent variable (academic achievement).
#' @param ses_var Character string: treatment/exposure variable (SES).
#' @param health_var Character string: mediator variable (e.g., BMI).
#' @param covariates Character vector of covariate names for both models.
#' @param n_boot Integer: number of bootstrap replications. Default 1000.
#' @param conf_level Numeric: confidence level. Default 0.95.
#' @param sensitivity Logical: if TRUE, runs sensitivity analysis via
#'   \code{medsens()} to assess robustness to unmeasured confounding. Default TRUE.
#' @param seed Integer: random seed for reproducibility. Default 123.
#'
#' @return A list of class \code{"cedm_mediation"} with elements:
#'   \itemize{
#'     \item \code{mediation_result}: output from \code{mediation::mediate()}.
#'     \item \code{summary}: summary of mediation results.
#'     \item \code{acme}: Average Causal Mediation Effect.
#'     \item \code{ade}: Average Direct Effect.
#'     \item \code{total_effect}: Total causal effect of SES.
#'     \item \code{prop_mediated}: Proportion of total effect mediated through health.
#'     \item \code{sensitivity}: sensitivity analysis results (if requested).
#'     \item \code{interpretation}: automated CEDM-based interpretation string.
#'   }
#'
#' @details
#' The mediation model follows the two-equation structure used in the ECLS-K
#' analysis (Hait, 2026):
#' \deqn{BMI = \alpha + \gamma \times SES + \delta^\top \times Covariates + \varepsilon_1}
#' \deqn{Math = \beta + \tau' \times SES + \zeta \times BMI +
#'   \theta^\top \times Covariates + \varepsilon_2}
#' ACME = \eqn{\gamma \times \zeta}; ADE = \eqn{\tau'}.
#'
#' Per CEDM Proposition 1, a small ACME relative to the total SES effect
#' (proportion mediated < 5\%) is the expected and theoretically meaningful
#' result, indicating that health does not function as a causal conduit but
#' as a conditional conversion moderator.
#'
#' @references
#' Imai, K., Keele, L., & Tingley, D. (2010). A general approach to causal
#' mediation analysis. \emph{Psychological Methods}, 15(4), 309-334.
#'
#' Hait, S. (2026). Socioeconomic Status, Health, and Academic Achievement:
#' A Capability-Ecological Developmental Model.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' df <- data.frame(
#'   math = rnorm(500, 500, 100),
#'   ses  = rnorm(500),
#'   bmi  = rnorm(500, 25, 5),
#'   sex  = sample(0:1, 500, replace = TRUE)
#' )
#' result <- cedm_mediation(df, outcome_var = "math", ses_var = "ses",
#'                          health_var = "bmi", covariates = "sex",
#'                          n_boot = 200)
#' print(result)
#' }
#'
#' @export
cedm_mediation <- function(data,
                           outcome_var,
                           ses_var,
                           health_var,
                           covariates  = NULL,
                           n_boot      = 1000,
                           conf_level  = 0.95,
                           sensitivity = TRUE,
                           seed        = 123) {

  if (!requireNamespace("mediation", quietly = TRUE)) {
    stop("Package 'mediation' is required. Please install it.")
  }

  set.seed(seed)

  # Validate variables
  req_vars <- c(outcome_var, ses_var, health_var, covariates)
  missing  <- req_vars[!req_vars %in% names(data)]
  if (length(missing) > 0) {
    stop(paste("Missing variables:", paste(missing, collapse = ", ")))
  }

  cov_str <- if (!is.null(covariates)) {
    paste("+", paste(covariates, collapse = " + "))
  } else {
    ""
  }

  # Build formulas as strings then parse — avoids mediation package
  # scoping bug where lm() stores the unevaluated symbol 'mediator_formula'
  # in its call and mediate() cannot find it in the parent frame.
  med_fml <- stats::as.formula(
    paste(health_var, "~", ses_var, cov_str),
    env = parent.frame()
  )
  out_fml <- stats::as.formula(
    paste(outcome_var, "~", ses_var, "+", health_var, cov_str),
    env = parent.frame()
  )

  # Fit models using do.call so the formula is embedded in the call object
  mediator_model <- do.call(
    stats::lm,
    list(formula = med_fml, data = quote(data)),
    envir = environment()
  )
  outcome_model <- do.call(
    stats::lm,
    list(formula = out_fml, data = quote(data)),
    envir = environment()
  )

  # Explicitly update the call so mediation::mediate() can find data
  mediator_model$call$data <- as.name("data")
  outcome_model$call$data  <- as.name("data")

  med_result <- mediation::mediate(
    model.m    = mediator_model,
    model.y    = outcome_model,
    treat      = ses_var,
    mediator   = health_var,
    boot       = TRUE,
    sims       = n_boot,
    conf.level = conf_level
  )

  med_summary   <- summary(med_result)
  acme          <- med_result$d0
  ade           <- med_result$z0
  total         <- med_result$tau.coef
  prop_mediated <- med_result$n0

  # Optional sensitivity analysis
  sens_result <- NULL
  if (sensitivity) {
    tryCatch({
      sens_result <- mediation::medsens(med_result, rho.by = 0.05)
    }, error = function(e) {
      message("Sensitivity analysis failed: ", e$message)
    })
  }

  # Automated interpretation
  pct_mediated <- round(prop_mediated * 100, 4)
  interpretation <- paste0(
    "CEDM Mediation Interpretation:\n",
    "  ACME = ", round(acme, 3),
    " (", round(pct_mediated, 4), "% of total SES effect mediated through ",
    health_var, ").\n",
    if (pct_mediated < 5) {
      paste0(
        "  CONSISTENT with CEDM Proposition 1: ",
        health_var, " does NOT function as a meaningful causal conduit.\n",
        "  The health indicator is better conceptualized as a CONDITIONAL ",
        "CONVERSION MODERATOR (see cedm_production())."
      )
    } else {
      paste0(
        "  ", health_var, " may carry some SES effects to achievement.\n",
        "  Consider whether ecological confounding explains this indirect pathway."
      )
    }
  )

  out <- list(
    mediation_result = med_result,
    summary          = med_summary,
    acme             = acme,
    ade              = ade,
    total_effect     = total,
    prop_mediated    = prop_mediated,
    sensitivity      = sens_result,
    interpretation   = interpretation,
    outcome_var      = outcome_var,
    ses_var          = ses_var,
    health_var       = health_var
  )
  class(out) <- "cedm_mediation"
  return(out)
}

#' @export
print.cedm_mediation <- function(x, ...) {
  cat("=== CEDM Causal Mediation Analysis ===\n")
  cat("SES:", x$ses_var, "-> Health:", x$health_var,
      "-> Outcome:", x$outcome_var, "\n\n")
  cat(sprintf("  ACME (indirect via %s) : %.4f\n", x$health_var, x$acme))
  cat(sprintf("  ADE  (direct SES)      : %.4f\n", x$ade))
  cat(sprintf("  Total SES effect       : %.4f\n", x$total_effect))
  cat(sprintf("  Proportion mediated    : %.6f (%.4f%%)\n",
              x$prop_mediated, x$prop_mediated * 100))
  cat("\n", x$interpretation, "\n")
  invisible(x)
}
