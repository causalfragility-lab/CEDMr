#' Nonlinear Moderation via Restricted Cubic Splines (CEDM Proposition 1 & 2)
#'
#' Fits a multilevel nonlinear moderation model using restricted cubic splines
#' (RCS) to capture threshold and nonlinear effects of health on the
#' SES-achievement relationship. Implements the spline-based approach used
#' in Hait (2025) for detecting nonlinearities in the BMI-achievement link
#' that are invisible in linear models.
#'
#' @param data A data.frame.
#' @param outcome_var Character string: dependent variable.
#' @param ses_var Character string: SES predictor.
#' @param health_var Character string: health variable to spline-transform (e.g., BMI).
#' @param df Integer: degrees of freedom for the restricted cubic spline. Default 5.
#' @param covariates Character vector of covariate names.
#' @param cluster_var Character string: cluster variable for random intercepts.
#'   If NULL, OLS is used.
#' @param interaction Logical: if TRUE (default), include SES x spline(health)
#'   interaction terms to model nonlinear moderation.
#' @param plot Logical: if TRUE (default), generate a marginal effects plot.
#'
#' @return A list of class \code{"cedm_spline"} with:
#'   \itemize{
#'     \item \code{model}: fitted model object.
#'     \item \code{summary}: model summary.
#'     \item \code{anova_test}: ANOVA test for overall nonlinearity.
#'     \item \code{plot}: ggplot2 object showing predicted achievement vs health
#'       by SES level (if \code{plot = TRUE}).
#'   }
#'
#' @details
#' Restricted cubic splines allow the health-achievement relationship to be
#' nonlinear and threshold-based -- exactly the pattern predicted by the CEDM
#' for amplifying contexts, where health constraints accelerate sharply at
#' the upper end of the health-risk distribution (Tomasi & Volkow, 2024).
#'
#' @references
#' Harrell, F. E. (2015). Regression modeling strategies. Springer.
#' Hait, S. (2025). Socioeconomic Status, Health, and Academic Achievement:
#' A Capability-Ecological Developmental Model.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' df <- data.frame(
#'   math   = rnorm(400, 500, 100),
#'   ses    = rnorm(400),
#'   bmi    = rnorm(400, 25, 5),
#'   school = sample(1:40, 400, replace = TRUE)
#' )
#' result <- cedm_spline_moderation(df, outcome_var = "math",
#'                                   ses_var = "ses", health_var = "bmi",
#'                                   cluster_var = "school")
#' print(result)
#' }
#'
#' @export
cedm_spline_moderation <- function(data,
                                   outcome_var,
                                   ses_var,
                                   health_var,
                                   df          = 5,
                                   covariates  = NULL,
                                   cluster_var = NULL,
                                   interaction = TRUE,
                                   plot        = TRUE) {

  if (!requireNamespace("rms", quietly = TRUE)) {
    stop("Package 'rms' is required for splines. Please install it.")
  }

  req_vars <- c(outcome_var, ses_var, health_var, covariates, cluster_var)
  req_vars <- req_vars[!is.null(req_vars)]
  missing  <- req_vars[!req_vars %in% names(data)]
  if (length(missing) > 0) stop(paste("Missing variables:", paste(missing, collapse = ", ")))

  # Center SES
  data[[paste0(ses_var, "_c")]] <- scale(data[[ses_var]], scale = FALSE)
  ses_c <- paste0(ses_var, "_c")

  # Create spline basis using rms::rcs
  dd <- rms::datadist(data)
  options(datadist = "dd")

  spline_term <- paste0("rms::rcs(", health_var, ", ", df, ")")

  if (interaction) {
    health_term <- paste0(ses_c, " * ", spline_term)
  } else {
    health_term <- spline_term
  }

  cov_str <- if (!is.null(covariates)) paste("+", paste(covariates, collapse = " + ")) else ""

  if (!is.null(cluster_var)) {
    if (!requireNamespace("lme4", quietly = TRUE)) stop("Package 'lme4' required.")
    formula_str <- paste(outcome_var, "~", health_term, "+", ses_c, cov_str,
                         "+ (1 |", cluster_var, ")")
    fit <- lme4::lmer(stats::as.formula(formula_str), data = data, REML = FALSE)
  } else {
    formula_str <- paste(outcome_var, "~", health_term, "+", ses_c, cov_str)
    fit <- stats::lm(stats::as.formula(formula_str), data = data)
  }

  fit_summary <- summary(fit)

  # Nonlinearity ANOVA test (OLS only)
  anova_result <- NULL
  if (is.null(cluster_var)) {
    formula_linear <- stats::as.formula(
      paste(outcome_var, "~", ses_c, "+", health_var, cov_str)
    )
    fit_linear   <- stats::lm(formula_linear, data = data)
    anova_result <- stats::anova(fit_linear, fit)
  }

  # Marginal effects plot
  p <- NULL
  if (plot) {
    tryCatch({
      health_seq <- seq(min(data[[health_var]], na.rm = TRUE),
                        max(data[[health_var]], na.rm = TRUE),
                        length.out = 100)

      ses_vals <- stats::quantile(data[[ses_var]], probs = c(0.25, 0.50, 0.75),
                                  na.rm = TRUE)
      ses_labels <- c("Low SES (25th)", "Medium SES (50th)", "High SES (75th)")

      pred_data_list <- lapply(seq_along(ses_vals), function(k) {
        d <- data[rep(1, 100), ]
        d[[health_var]] <- health_seq
        d[[ses_var]]    <- ses_vals[k]
        d[[ses_c]]      <- ses_vals[k] - mean(data[[ses_var]], na.rm = TRUE)
        if (!is.null(covariates)) {
          for (cv in covariates) {
            if (is.numeric(data[[cv]])) d[[cv]] <- mean(data[[cv]], na.rm = TRUE)
          }
        }
        preds <- stats::predict(fit, newdata = d)
        data.frame(health = health_seq, predicted = preds,
                   ses_level = ses_labels[k])
      })
      pred_df <- do.call(rbind, pred_data_list)
      pred_df$ses_level <- factor(pred_df$ses_level, levels = ses_labels)

      p <- ggplot2::ggplot(pred_df,
                           ggplot2::aes(x = health, y = predicted,
                                        color = ses_level, linetype = ses_level)) +
        ggplot2::geom_line(size = 1.1) +
        ggplot2::labs(
          title    = "CEDM Nonlinear Moderation: Health x SES -> Achievement",
          subtitle = paste("Restricted cubic splines (df =", df, ") with SES x Health interaction"),
          x        = paste(health_var, "(raw scale)"),
          y        = paste("Predicted", outcome_var),
          color    = "SES Level",
          linetype = "SES Level",
          caption  = "CEDM: diverging slopes indicate ecological capability regime effects"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    }, error = function(e) {
      message("Plot generation failed: ", e$message)
    })
  }

  out <- list(
    model       = fit,
    summary     = fit_summary,
    anova_test  = anova_result,
    plot        = p,
    outcome_var = outcome_var,
    ses_var     = ses_var,
    health_var  = health_var,
    df          = df
  )
  class(out) <- "cedm_spline"
  return(out)
}

#' @export
print.cedm_spline <- function(x, ...) {
  cat("=== CEDM Nonlinear Spline Moderation ===\n")
  cat("Health variable:", x$health_var, "| Spline df:", x$df, "\n\n")
  print(x$summary)
  if (!is.null(x$anova_test)) {
    cat("\n--- Nonlinearity Test (spline vs linear) ---\n")
    print(x$anova_test)
  }
  invisible(x)
}
