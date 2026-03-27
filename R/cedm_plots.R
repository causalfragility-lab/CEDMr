#' Plot Ecological Capability Regimes
#'
#' Generates a scatter plot of SES vs. a health variable, colored by CEDM
#' ecological capability regime, with optional outcome overlaid as point size.
#'
#' @param data A data.frame with regime classification (output of \code{classify_regime()}).
#' @param ses_var Character string: SES variable name.
#' @param health_var Character string: health variable name.
#' @param regime_var Character string: regime variable name. Default \code{"cedm_regime"}.
#' @param outcome_var Character string (optional): if provided, points are sized
#'   by this variable.
#' @param alpha_pt Numeric: point transparency. Default 0.5.
#' @param title Character string: plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' set.seed(42)
#' df <- data.frame(ses = rnorm(300), bmi = rnorm(300, 25, 5),
#'                  math = rnorm(300, 500, 100), opp = rnorm(300))
#' df <- classify_regime(df, ses_var = "ses", opportunity_var = "opp")
#' plot_regimes(df, ses_var = "ses", health_var = "bmi", outcome_var = "math")
#'
#' @export
plot_regimes <- function(data,
                         ses_var,
                         health_var,
                         regime_var  = "cedm_regime",
                         outcome_var = NULL,
                         alpha_pt    = 0.5,
                         title       = "CEDM Ecological Capability Regimes") {

  req_vars <- c(ses_var, health_var, regime_var, outcome_var)
  req_vars <- req_vars[!is.null(req_vars)]
  missing  <- req_vars[!req_vars %in% names(data)]
  if (length(missing) > 0) stop(paste("Missing variables:", paste(missing, collapse = ", ")))

  regime_colors <- c(
    "amplifying"    = "#d73027",
    "neutral"       = "#fee090",
    "compensatory"  = "#4575b4"
  )

  p <- ggplot2::ggplot(data,
                       ggplot2::aes(x     = .data[[ses_var]],
                                    y     = .data[[health_var]],
                                    color = .data[[regime_var]])) +
    ggplot2::geom_point(alpha = alpha_pt, size = if (is.null(outcome_var)) 2 else 1) +
    ggplot2::scale_color_manual(values = regime_colors, name = "CEDM Regime") +
    ggplot2::labs(
      title    = title,
      subtitle = "Red = Amplifying | Yellow = Neutral | Blue = Compensatory",
      x        = ses_var,
      y        = health_var,
      caption  = "CEDM: Regime classification based on SES and ecological opportunity"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(outcome_var)) {
    p <- p + ggplot2::aes(size = .data[[outcome_var]]) +
      ggplot2::scale_size_continuous(name = outcome_var, range = c(0.5, 4))
  }

  return(p)
}

#' Plot CEDM Interaction: SES x Health by Regime
#'
#' Creates a paneled interaction plot showing predicted achievement as a function
#' of health at different SES levels, separately for each ecological capability
#' regime. This is the core visualization for CEDM Proposition 2.
#'
#' @param cedm_prod_result Output from \code{cedm_production()} with
#'   \code{model = "regime"}.
#' @param data The original data.frame used to fit the model.
#' @param ses_var Character string: SES variable name.
#' @param health_var Character string: health variable name.
#' @param regime_var Character string: regime variable name.
#' @param outcome_var Character string: outcome variable name.
#' @param n_points Integer: number of health values for prediction grid. Default 50.
#'
#' @return A ggplot2 object.
#'
#' @export
plot_cedm_interaction <- function(cedm_prod_result,
                                   data,
                                   ses_var,
                                   health_var,
                                   regime_var,
                                   outcome_var,
                                   n_points = 50) {

  fit <- if (inherits(cedm_prod_result, "cedm_production")) {
    cedm_prod_result$model
  } else {
    cedm_prod_result
  }

  regimes   <- levels(data[[regime_var]])
  ses_vals  <- stats::quantile(data[[ses_var]], probs = c(0.25, 0.75), na.rm = TRUE)
  health_rng <- range(data[[health_var]], na.rm = TRUE)
  health_seq <- seq(health_rng[1], health_rng[2], length.out = n_points)

  ses_labels <- c("Low SES (25th pct)", "High SES (75th pct)")
  regime_colors <- c("amplifying" = "#d73027", "neutral" = "#fee090",
                     "compensatory" = "#4575b4")

  # Identify the exact variable names the model was fitted with
  ses_mean    <- mean(data[[ses_var]],    na.rm = TRUE)
  health_mean <- mean(data[[health_var]], na.rm = TRUE)
  ses_c_var    <- paste0(ses_var,    "_c")
  health_c_var <- paste0(health_var, "_c")

  # Get all variable names the model expects
  model_vars <- tryCatch(all.vars(stats::formula(fit)), error = function(e) character(0))

  pred_list <- lapply(regimes, function(reg) {
    lapply(seq_along(ses_vals), function(j) {

      # Start with a named list to build the prediction row cleanly
      pred_list_row <- list()
      pred_list_row[[ses_var]]     <- rep(ses_vals[j], n_points)
      pred_list_row[[health_var]]  <- health_seq
      pred_list_row[[regime_var]]  <- factor(rep(reg, n_points), levels = regimes)
      pred_list_row[[ses_c_var]]   <- rep(ses_vals[j] - ses_mean, n_points)
      pred_list_row[[health_c_var]] <- health_seq - health_mean

      # Add any other model variables at their mean/reference level
      other_vars <- setdiff(model_vars,
                            c(ses_var, health_var, regime_var,
                              ses_c_var, health_c_var))
      for (col in other_vars) {
        if (col %in% names(data)) {
          if (is.numeric(data[[col]])) {
            pred_list_row[[col]] <- rep(mean(data[[col]], na.rm = TRUE), n_points)
          } else {
            pred_list_row[[col]] <- rep(data[[col]][1], n_points)
          }
        }
      }

      pred_d <- as.data.frame(pred_list_row, stringsAsFactors = FALSE)

      preds <- tryCatch(
        stats::predict(fit, newdata = pred_d, re.form = NA),
        error = function(e) {
          tryCatch(
            stats::predict(fit, newdata = pred_d),
            error = function(e2) rep(NA_real_, n_points)
          )
        }
      )

      data.frame(
        health    = health_seq,
        predicted = as.numeric(preds),
        ses_level = ses_labels[j],
        regime    = reg
      )
    })
  })

  pred_df <- do.call(rbind, unlist(pred_list, recursive = FALSE))
  pred_df$regime    <- factor(pred_df$regime, levels = regimes)
  pred_df$ses_level <- factor(pred_df$ses_level)

  p <- ggplot2::ggplot(pred_df,
                       ggplot2::aes(x     = health,
                                    y     = predicted,
                                    color = ses_level,
                                    group = ses_level)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::facet_wrap(~ regime,
                        labeller = ggplot2::labeller(
                          regime = c(
                            amplifying   = "Amplifying Regime",
                            neutral      = "Neutral Regime",
                            compensatory = "Compensatory Regime"
                          )
                        )) +
    ggplot2::scale_color_manual(
      values = c("Low SES (25th pct)" = "#d73027", "High SES (75th pct)" = "#4575b4"),
      name   = "SES Level"
    ) +
    ggplot2::labs(
      title    = "CEDM: SES x Health Interaction by Ecological Capability Regime",
      subtitle = "Proposition 2: interaction sign and magnitude differ across regimes",
      x        = paste(health_var, "(centered)"),
      y        = paste("Predicted", outcome_var),
      caption  = paste0(
        "Amplifying: health constrains SES returns | ",
        "Compensatory: SES buffers health effects | ",
        "Neutral: minimal interaction"
      )
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text      = ggplot2::element_text(face = "bold")
    )

  return(p)
}
