#' CEDM Sensitivity Analysis: Frank's ITCV and Robustness-to-Replacement (RIR)
#'
#' Computes sensitivity metrics for CEDM regression effects using Frank's (2000)
#' Impact Threshold for a Confounding Variable (ITCV) and the Robustness of
#' Inference to Replacement (RIR) index. Within the CEDM, these metrics serve
#' not merely as statistical diagnostics but as indicators of CAPABILITY
#' STABILITY (Proposition 5): large ITCV/RIR = stable conversion processes;
#' small ITCV/RIR = fragile conversion processes, especially in amplifying regimes.
#'
#' @param model A fitted \code{lm} or \code{lmerMod} object, OR the output
#'   of \code{cedm_production()}.
#' @param term Character string naming the coefficient of interest (e.g., "ses_c",
#'   "ses_c:bmi_c"). If NULL, all terms except the intercept are evaluated.
#' @param n_obs Integer: number of observations used in the model. Required when
#'   \code{model} is a \code{lmerMod} and \code{nobs()} may not be available.
#' @param alpha Numeric: significance threshold. Default 0.05.
#' @param benchmark Character string: label for the benchmark effect size
#'   (e.g., "SES main effect"). Used in the interpretation.
#'
#' @return A data.frame of class \code{"cedm_sensitivity"} with one row per
#'   evaluated term, containing:
#'   \itemize{
#'     \item \code{term}: predictor name.
#'     \item \code{estimate}: regression coefficient.
#'     \item \code{std_error}: standard error.
#'     \item \code{t_value}: t-statistic.
#'     \item \code{p_value}: p-value.
#'     \item \code{itcv}: Impact Threshold for a Confounding Variable.
#'     \item \code{rir_pct}: Robustness of Inference to Replacement (percent).
#'     \item \code{rir_n}: RIR in number of observations.
#'     \item \code{stability}: Qualitative label ("extremely robust", "robust",
#'       "moderately robust", "sensitive", "very sensitive").
#'     \item \code{cedm_interpretation}: CEDM-specific capability stability label.
#'   }
#'
#' @details
#' The ITCV is the minimum correlation an omitted confounder would need with
#' both the treatment and the outcome to nullify the observed effect:
#' \deqn{ITCV = \frac{t^2 - t_{crit}^2}{t^2 \cdot (n - q) + t_{crit}^2}}
#' where \eqn{t} is the observed t-statistic, \eqn{t_{crit}} is the critical
#' t-value, \eqn{n} is sample size, and \eqn{q} is the number of parameters.
#'
#' The RIR is the number (or percentage) of observations that would need to be
#' replaced with cases showing no effect to nullify the inference.
#'
#' Per CEDM Proposition 5:
#' \itemize{
#'   \item High ITCV/RIR -> Capability stability: conversion processes are
#'     robust to structural disruption.
#'   \item Low ITCV/RIR -> Capability fragility: conversion processes are
#'     easily disrupted, especially in amplifying ecological regimes.
#' }
#'
#' @references
#' Frank, K. A. (2000). Impact of a confounding variable on a regression
#' coefficient. \emph{Sociological Methods & Research}, 29(2), 147-194.
#'
#' Frank, K. A., Maroulis, S. J., Duong, M. Q., & Kelcey, B. M. (2013).
#' What would it take to change an inference? Using Rubin's causal model to
#' interpret the robustness of causal inferences. \emph{Educational Evaluation
#' and Policy Analysis}, 35(4), 437-460.
#'
#' Hait, S. (2026). Socioeconomic Status, Health, and Academic Achievement:
#' A Capability-Ecological Developmental Model.
#'
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   math = rnorm(500, 500, 100),
#'   ses  = rnorm(500),
#'   bmi  = rnorm(500, 25, 5)
#' )
#' fit  <- lm(math ~ ses * bmi, data = df)
#' sens <- cedm_sensitivity(fit, term = "ses")
#' print(sens)
#'
#' @export
cedm_sensitivity <- function(model,
                             term      = NULL,
                             n_obs     = NULL,
                             alpha     = 0.05,
                             benchmark = NULL) {

  # Handle cedm_production objects
  if (inherits(model, "cedm_production")) {
    model <- model$model
  }

  # Extract coefficient table
  if (inherits(model, "lmerMod")) {
    coef_mat <- summary(model)$coefficients
    if (is.null(n_obs)) n_obs <- nrow(model@frame)
    n_params  <- nrow(coef_mat)
  } else if (inherits(model, "lm")) {
    coef_mat <- summary(model)$coefficients
    if (is.null(n_obs)) n_obs <- stats::nobs(model)
    n_params  <- length(stats::coef(model))
  } else {
    stop("model must be an lm or lmerMod object, or a cedm_production result.")
  }

  all_terms  <- rownames(coef_mat)
  eval_terms <- if (is.null(term)) {
    all_terms[all_terms != "(Intercept)"]
  } else {
    term
  }

  missing_terms <- eval_terms[!eval_terms %in% all_terms]
  if (length(missing_terms) > 0) {
    stop(paste("Terms not found in model:", paste(missing_terms, collapse = ", ")))
  }

  df_resid <- n_obs - n_params
  t_crit   <- stats::qt(1 - alpha / 2, df = df_resid)

  results <- do.call(rbind, lapply(eval_terms, function(trm) {

    est <- coef_mat[trm, 1]
    se  <- coef_mat[trm, 2]
    t   <- if (ncol(coef_mat) >= 3) coef_mat[trm, 3] else est / se
    p   <- if (ncol(coef_mat) >= 4) coef_mat[trm, 4] else
      2 * stats::pt(-abs(t), df = df_resid)

    # ITCV calculation (Frank 2000)
    itcv <- (t^2 - t_crit^2) / (t^2 * df_resid + t_crit^2)
    itcv <- max(itcv, 0)

    # RIR: proportion of obs to replace to nullify inference
    prop_replace <- max(0, (abs(t) - t_crit) / abs(t))
    rir_n        <- round(prop_replace * n_obs)
    rir_pct      <- round(prop_replace * 100, 1)

    # Qualitative stability labels
    stability <- dplyr::case_when(
      rir_pct >= 75 ~ "extremely robust",
      rir_pct >= 50 ~ "robust",
      rir_pct >= 25 ~ "moderately robust",
      rir_pct >= 10 ~ "sensitive",
      TRUE          ~ "very sensitive"
    )

    # CEDM capability stability interpretation
    cedm_interp <- dplyr::case_when(
      rir_pct >= 75 ~ "High capability stability: conversion processes robust to structural disruption",
      rir_pct >= 50 ~ "Moderate capability stability: conversion relatively resilient",
      rir_pct >= 25 ~ "Moderate capability fragility: conversion somewhat sensitive to unmeasured forces",
      TRUE          ~ "High capability fragility: conversion processes easily disrupted (amplifying regime signal)"
    )

    data.frame(
      term                = trm,
      estimate            = round(est, 4),
      std_error           = round(se, 4),
      t_value             = round(t, 3),
      p_value             = round(p, 4),
      itcv                = round(itcv, 4),
      rir_pct             = rir_pct,
      rir_n               = rir_n,
      n_obs               = n_obs,
      stability           = stability,
      cedm_interpretation = cedm_interp,
      stringsAsFactors    = FALSE
    )
  }))

  class(results) <- c("cedm_sensitivity", "data.frame")
  attr(results, "alpha")     <- alpha
  attr(results, "benchmark") <- benchmark
  return(results)
}

#' @export
print.cedm_sensitivity <- function(x, ...) {
  cat("=== CEDM Sensitivity Analysis (Proposition 5: Capability Stability) ===\n")
  cat("Significance threshold (alpha):", attr(x, "alpha"), "\n\n")

  display_cols <- c("term", "estimate", "t_value", "p_value",
                    "itcv", "rir_pct", "rir_n", "stability")

  # Strip custom class before printing to prevent infinite recursion
  x_plain <- x[, display_cols, drop = FALSE]
  class(x_plain) <- "data.frame"
  print(x_plain, row.names = FALSE)

  cat("\nCEDM Interpretation (Capability Stability):\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  [%s] %s\n", x$term[i], x$cedm_interpretation[i]))
  }
  invisible(x)
}
