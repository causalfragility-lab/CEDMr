#' Random-Effects Within-Between (REWB) Decomposition
#'
#' Decomposes a health indicator (e.g., BMI) into between-person (stable,
#' chronic) and within-person (transient, time-varying) components to test
#' CEDM Proposition 3 (Developmental Recursion). This mirrors the REWB
#' design used in Hait (2025) for the ECLS-K:2011 data.
#'
#' Per the CEDM, between-child health differences (chronic status) are
#' expected to have significant associations with achievement, while
#' within-child fluctuations are expected to be non-significant --
#' indicating that health functions as a stable developmental risk factor
#' rather than a time-varying dynamic predictor.
#'
#' @param data A data.frame in LONG format.
#' @param outcome_var Character string: dependent variable (e.g., math score).
#' @param health_var Character string: health variable to decompose (e.g., BMI).
#' @param id_var Character string: person-level ID variable.
#' @param time_var Character string: time/wave indicator variable.
#' @param ses_var Character string: SES predictor (included as covariate).
#' @param covariates Character vector of additional covariate names.
#' @param cluster_var Character string: school or higher-level cluster variable
#'   for additional random intercept. If NULL, only person-level random
#'   intercepts are modelled.
#'
#' @return A list of class \code{"cedm_rewb"} with:
#'   \itemize{
#'     \item \code{model}: fitted \code{lmerMod} object.
#'     \item \code{summary}: model summary.
#'     \item \code{between_effect}: coefficient for between-person health variable.
#'     \item \code{within_effect}: coefficient for within-person health variable.
#'     \item \code{data_decomposed}: original data with added \code{_between}
#'       and \code{_within} columns.
#'     \item \code{interpretation}: CEDM interpretation string.
#'   }
#'
#' @details
#' The between component is each person's mean of the health variable across
#' waves. The within component is the wave-specific deviation from that mean:
#' \deqn{H_{between,i} = \bar{H}_i}
#' \deqn{H_{within,it} = H_{it} - \bar{H}_i}
#'
#' The model then includes both components as separate predictors:
#' \deqn{Y_{it} = \beta_0 + \beta_1 SES_i + \beta_2 H_{between,i} +
#'   \beta_3 H_{within,it} + (1|id) + \varepsilon}
#'
#' @references
#' Curran, P. J., Howard, A. L., Bainter, S. A., Lane, S. T., & McGinley,
#' J. S. (2014). The separation of between-person and within-person components
#' of individual change over time. \emph{Journal of Consulting and Clinical
#' Psychology}.
#'
#' Hait, S. (2025). Socioeconomic Status, Health, and Academic Achievement:
#' A Capability-Ecological Developmental Model.
#'
#' @examples
#' set.seed(42)
#' df_long <- data.frame(
#'   id     = rep(1:200, each = 5),
#'   wave   = rep(1:5, times = 200),
#'   math   = rnorm(1000, 500, 100),
#'   bmi    = rnorm(1000, 25, 5),
#'   ses    = rep(rnorm(200), each = 5),
#'   school = rep(sample(1:20, 200, replace = TRUE), each = 5)
#' )
#' result <- cedm_rewb(df_long, outcome_var = "math", health_var = "bmi",
#'                     id_var = "id", time_var = "wave", ses_var = "ses",
#'                     cluster_var = "school")
#' print(result)
#'
#' @export
cedm_rewb <- function(data,
                      outcome_var,
                      health_var,
                      id_var,
                      time_var,
                      ses_var     = NULL,
                      covariates  = NULL,
                      cluster_var = NULL) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required. Please install it.")
  }

  req_vars <- c(outcome_var, health_var, id_var, time_var,
                ses_var, covariates, cluster_var)
  req_vars <- req_vars[!is.null(req_vars)]
  missing  <- req_vars[!req_vars %in% names(data)]
  if (length(missing) > 0) {
    stop(paste("Missing variables:", paste(missing, collapse = ", ")))
  }

  # Compute between and within components
  between_var <- paste0(health_var, "_between")
  within_var  <- paste0(health_var, "_within")

  data <- dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(data, .data[[id_var]]),
      !!between_var := mean(.data[[health_var]], na.rm = TRUE),
      !!within_var  := .data[[health_var]] - mean(.data[[health_var]], na.rm = TRUE)
    )
  )

  # Build formula
  fixed_parts <- c(between_var, within_var, time_var)
  if (!is.null(ses_var))    fixed_parts <- c(ses_var, fixed_parts)
  if (!is.null(covariates)) fixed_parts <- c(fixed_parts, covariates)

  re_parts <- paste0("(1 | ", id_var, ")")
  if (!is.null(cluster_var)) {
    re_parts <- paste(re_parts, paste0("+ (1 | ", cluster_var, ")"))
  }

  formula_str <- paste(
    outcome_var, "~",
    paste(fixed_parts, collapse = " + "),
    "+", re_parts
  )
  formula_obj <- stats::as.formula(formula_str)

  fit         <- lme4::lmer(formula_obj, data = data, REML = FALSE)
  fit_summary <- summary(fit)

  coef_matrix <- fit_summary$coefficients
  between_eff <- if (between_var %in% rownames(coef_matrix)) {
    coef_matrix[between_var, ]
  } else {
    NA
  }
  within_eff <- if (within_var %in% rownames(coef_matrix)) {
    coef_matrix[within_var, ]
  } else {
    NA
  }

  # lmerMod omits p-values; compute from t-statistic
  df_resid <- nrow(data) - length(lme4::fixef(fit))
  get_pval <- function(eff) {
    if (all(is.na(eff))) return(NA)
    p_col <- which(names(eff) == "Pr(>|t|)")
    t_col <- which(names(eff) == "t value")
    if (length(p_col) > 0) return(as.numeric(eff[p_col]))
    if (length(t_col) > 0) {
      return(2 * stats::pt(-abs(as.numeric(eff[t_col])), df = df_resid))
    }
    return(NA)
  }
  b_pval <- get_pval(between_eff)
  w_pval <- get_pval(within_eff)

  interp <- paste0(
    "CEDM REWB Interpretation (Proposition 3 - Developmental Recursion):\n",
    "  Between-child ", health_var, " effect: ",
    if (!is.na(b_pval) && b_pval < 0.05) "SIGNIFICANT" else "non-significant",
    " (p = ", if (!is.na(b_pval)) round(b_pval, 4) else "NA", ")\n",
    "  Within-child ", health_var, " effect: ",
    if (!is.na(w_pval) && w_pval < 0.05) "SIGNIFICANT" else "non-significant",
    " (p = ", if (!is.na(w_pval)) round(w_pval, 4) else "NA", ")\n",
    if (!is.na(b_pval) && !is.na(w_pval) &&
        b_pval < 0.05 && w_pval >= 0.05) {
      paste0(
        "  -> CONSISTENT with CEDM: Chronic between-child health differences\n",
        "     (not transient within-child fluctuations) drive achievement gaps.\n",
        "     This supports health as a STABLE developmental risk marker."
      )
    } else if (!is.na(b_pval) && !is.na(w_pval) &&
               b_pval >= 0.05 && w_pval >= 0.05) {
      "  -> Both effects non-significant: health may be a weaker influence overall."
    } else {
      "  -> Mixed pattern: both between and within effects significant."
    }
  )

  out <- list(
    model           = fit,
    summary         = fit_summary,
    between_effect  = between_eff,
    within_effect   = within_eff,
    data_decomposed = data,
    between_var     = between_var,
    within_var      = within_var,
    health_var      = health_var,
    interpretation  = interp
  )
  class(out) <- "cedm_rewb"
  return(out)
}

#' @export
print.cedm_rewb <- function(x, ...) {
  cat("=== CEDM Random-Effects Within-Between (REWB) Decomposition ===\n\n")
  if (!all(is.na(x$between_effect))) {
    cat("Between-child", x$health_var, "effect:\n")
    print(round(x$between_effect, 4))
  }
  if (!all(is.na(x$within_effect))) {
    cat("\nWithin-child", x$health_var, "effect:\n")
    print(round(x$within_effect, 4))
  }
  cat("\n", x$interpretation, "\n")
  invisible(x)
}
