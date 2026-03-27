#' Fit the CEDM Capability-Conversion Production Function
#'
#' Estimates the CEDM production function relating SES, health (e.g., BMI), and
#' ecological capability regime to academic achievement. Supports the base
#' two-way (SES x Health) model and the full three-way (SES x Health x Regime)
#' model described in the theory paper. Uses multilevel (mixed-effects) or
#' single-level OLS depending on the presence of cluster variables.
#'
#' @param data A data.frame.
#' @param outcome_var Character string: dependent variable (e.g., math score).
#' @param ses_var Character string: SES predictor.
#' @param health_var Character string: health indicator (e.g., BMI).
#' @param regime_var Character string: ecological regime variable (typically
#'   \code{"cedm_regime"} produced by \code{classify_regime()}). If NULL, the
#'   two-way SES x health model is estimated.
#' @param covariates Character vector of additional covariate names.
#' @param cluster_var Character string: grouping/cluster variable (e.g., school ID)
#'   for random intercepts. If NULL, a standard OLS model is fitted.
#' @param model One of \code{"base"} (SES + health + SES:health),
#'   \code{"regime"} (full three-way with regime), or \code{"additive"}
#'   (no interaction, main effects only).
#' @param center Logical. If TRUE (default), SES and health are mean-centered
#'   before fitting.
#' @param weights Character string naming a survey weight variable, or NULL.
#'
#' @return A list of class \code{"cedm_production"} with elements:
#'   \itemize{
#'     \item \code{model}: the fitted model object (\code{lm} or \code{lmerMod}).
#'     \item \code{summary}: model summary.
#'     \item \code{formula}: the formula used.
#'     \item \code{centered}: logical, whether centering was applied.
#'     \item \code{coefficients}: tidy coefficient table.
#'   }
#'
#' @details
#' \strong{Base model (Proposition 1 + 4):}
#' \deqn{A_i = \beta_0 + \beta_1 SES_i + \beta_2 H_i + \beta_3 (SES_i \times H_i) + \varepsilon}
#' A significant, negative \eqn{\beta_3} indicates that health constraints
#' steepen the SES-achievement gradient for low-SES children (amplifying effect).
#'
#' \strong{Full regime model (Proposition 2):}
#' \deqn{A_i = \beta_0 + \beta_1 SES_i + \beta_2 H_i + \beta_3 (SES_i \times H_i)
#'   + \beta_4 (SES_i \times H_i \times C_j) + \varepsilon}
#' The three-way interaction \eqn{\beta_4} is the empirical index of ecological
#' capability regime type.
#'
#' @references
#' Hait, S. (2025). Socioeconomic Status, Health, and Academic Achievement:
#' A Capability-Ecological Developmental Model.
#'
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   math  = rnorm(300, 500, 100),
#'   ses   = rnorm(300),
#'   bmi   = rnorm(300, 25, 5),
#'   school = sample(1:30, 300, replace = TRUE)
#' )
#' df <- classify_regime(df, ses_var = "ses")
#' result <- cedm_production(df, outcome_var = "math", ses_var = "ses",
#'                           health_var = "bmi", regime_var = "cedm_regime",
#'                           cluster_var = "school")
#' print(result)
#'
#' @export
cedm_production <- function(data,
                            outcome_var,
                            ses_var,
                            health_var,
                            regime_var    = NULL,
                            covariates    = NULL,
                            cluster_var   = NULL,
                            model         = c("base", "regime", "additive"),
                            center        = TRUE,
                            weights       = NULL) {

  model <- match.arg(model)

  # Validate variables
  required_vars <- c(outcome_var, ses_var, health_var)
  if (!is.null(regime_var))  required_vars <- c(required_vars, regime_var)
  if (!is.null(cluster_var)) required_vars <- c(required_vars, cluster_var)
  if (!is.null(covariates))  required_vars <- c(required_vars, covariates)
  if (!is.null(weights))     required_vars <- c(required_vars, weights)

  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
  }

  # Center SES and health
  if (center) {
    data[[paste0(ses_var, "_c")]]    <- scale(data[[ses_var]], scale = FALSE)
    data[[paste0(health_var, "_c")]] <- scale(data[[health_var]], scale = FALSE)
    ses_term    <- paste0(ses_var, "_c")
    health_term <- paste0(health_var, "_c")
  } else {
    ses_term    <- ses_var
    health_term <- health_var
  }

  # Build formula
  covs_str <- if (!is.null(covariates)) paste("+", paste(covariates, collapse = " + ")) else ""

  if (model == "additive") {
    fixed_str <- paste(outcome_var, "~", ses_term, "+", health_term, covs_str)
  } else if (model == "base") {
    fixed_str <- paste(outcome_var, "~", ses_term, "*", health_term, covs_str)
  } else if (model == "regime") {
    if (is.null(regime_var)) {
      stop("regime_var must be provided for model = 'regime'.")
    }
    fixed_str <- paste(outcome_var, "~",
                       ses_term, "*", health_term, "*", regime_var, covs_str)
  }

  formula_obj <- stats::as.formula(fixed_str)

  # Fit model: multilevel or OLS
  if (!is.null(cluster_var)) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' is required for multilevel models. Please install it.")
    }
    re_str   <- paste("+ (1 |", cluster_var, ")")
    ml_form  <- stats::as.formula(paste(fixed_str, re_str))
    fit <- lme4::lmer(ml_form, data = data, REML = FALSE)
  } else {
    if (!is.null(weights)) {
      fit <- stats::lm(formula_obj, data = data, weights = data[[weights]])
    } else {
      fit <- stats::lm(formula_obj, data = data)
    }
  }

  # Tidy coefficient table
  coef_tbl <- as.data.frame(summary(fit)$coefficients)
  coef_tbl$term <- rownames(coef_tbl)
  rownames(coef_tbl) <- NULL

  out <- list(
    model        = fit,
    summary      = summary(fit),
    formula      = formula_obj,
    centered     = center,
    ses_var      = ses_var,
    health_var   = health_var,
    regime_var   = regime_var,
    model_type   = model,
    coefficients = coef_tbl
  )
  class(out) <- "cedm_production"
  return(out)
}

#' @export
print.cedm_production <- function(x, ...) {
  cat("=== CEDM Capability-Conversion Production Function ===\n")
  cat("Model type    :", x$model_type, "\n")
  cat("SES variable  :", x$ses_var, "\n")
  cat("Health variable:", x$health_var, "\n")
  if (!is.null(x$regime_var))
    cat("Regime variable:", x$regime_var, "\n")
  cat("Centered      :", x$centered, "\n\n")
  print(x$summary)
  invisible(x)
}
