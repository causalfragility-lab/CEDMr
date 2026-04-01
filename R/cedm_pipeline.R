#' Run the Full CEDM Analysis Pipeline
#'
#' A convenience wrapper that runs all major CEDM analytical steps in sequence:
#' regime classification, production function estimation, causal mediation,
#' REWB decomposition, spline moderation, trajectory clustering, and sensitivity
#' analysis. Designed for quick application to new datasets (e.g., any study
#' with longitudinal SES, health, and outcome data).
#'
#' @param data A data.frame in LONG format.
#' @param outcome_var Character string: dependent variable.
#' @param ses_var Character string: SES predictor.
#' @param health_var Character string: health variable (e.g., BMI).
#' @param id_var Character string: person-level ID (for REWB and trajectory).
#' @param time_var Character string: wave/time variable.
#' @param opportunity_var Character string (optional): school/neighborhood
#'   opportunity index for regime classification.
#' @param cluster_var Character string (optional): school/cluster ID for random
#'   intercepts.
#' @param covariates Character vector of additional covariate names.
#' @param n_boot Integer: bootstrap replications for mediation. Default 500.
#' @param k_trajectories Integer: number of health trajectory clusters. Default 3.
#' @param run_mediation Logical: whether to run mediation (can be slow). Default TRUE.
#' @param run_spline Logical: whether to run spline moderation. Default TRUE.
#' @param run_trajectory Logical: whether to run trajectory clustering. Default TRUE.
#' @param seed Integer: random seed. Default 123.
#' @param verbose Logical: print progress messages. Default TRUE.
#'
#' @return A named list of class \code{"cedm_pipeline"} containing outputs from
#'   each analysis step:
#'   \itemize{
#'     \item \code{data}: data with regime classification added.
#'     \item \code{regime_table}: frequency table of capability regimes.
#'     \item \code{model_base}: CEDM base production model (SES x health).
#'     \item \code{model_regime}: CEDM full regime model (SES x health x regime).
#'     \item \code{mediation}: causal mediation results (if run).
#'     \item \code{rewb}: within-between decomposition results.
#'     \item \code{spline}: nonlinear spline moderation results (if run).
#'     \item \code{trajectory}: health trajectory clustering (if run).
#'     \item \code{sensitivity}: sensitivity analysis for key terms.
#'     \item \code{plots}: named list of ggplot2 objects.
#'   }
#'
#' @examples
#' \donttest{
#' sim <- cedm_simulate(n = 3000, n_waves = 5, seed = 42)
#' pipeline <- cedm_full_pipeline(
#'   data        = sim,
#'   outcome_var = "achievement",
#'   ses_var     = "SES",
#'   health_var  = "health",
#'   id_var      = "id",
#'   time_var    = "wave",
#'   n_boot      = 200
#' )
#' summary(pipeline)
#' pipeline$plots$regimes
#' pipeline$plots$interaction
#' }
#'
#' @export
cedm_full_pipeline <- function(data,
                               outcome_var,
                               ses_var,
                               health_var,
                               id_var,
                               time_var,
                               opportunity_var  = NULL,
                               cluster_var      = NULL,
                               covariates       = NULL,
                               n_boot           = 500,
                               k_trajectories   = 3,
                               run_mediation    = TRUE,
                               run_spline       = TRUE,
                               run_trajectory   = TRUE,
                               seed             = 123,
                               verbose          = TRUE) {

  .msg <- function(...) if (verbose) message(...)

  results <- list()

  # Step 1: Regime Classification
  .msg("Step 1/7: Classifying ecological capability regimes...")
  data <- classify_regime(
    data            = data,
    ses_var         = ses_var,
    opportunity_var = opportunity_var,
    ses_tertiles    = is.null(opportunity_var)
  )
  results$data         <- data
  results$regime_table <- table(data$cedm_regime)

  # Step 2: Base production model (SES x health)
  .msg("Step 2/7: Fitting CEDM base production function (SES x health)...")
  results$model_base <- cedm_production(
    data        = data,
    outcome_var = outcome_var,
    ses_var     = ses_var,
    health_var  = health_var,
    regime_var  = NULL,
    covariates  = covariates,
    cluster_var = cluster_var,
    model       = "base",
    center      = TRUE
  )

  # Step 3: Full regime model (SES x health x regime)
  .msg("Step 3/7: Fitting CEDM full regime model (SES x health x regime)...")
  results$model_regime <- cedm_production(
    data        = data,
    outcome_var = outcome_var,
    ses_var     = ses_var,
    health_var  = health_var,
    regime_var  = "cedm_regime",
    covariates  = covariates,
    cluster_var = cluster_var,
    model       = "regime",
    center      = TRUE
  )

  # Step 4: Causal mediation
  if (run_mediation) {
    .msg("Step 4/7: Running causal mediation analysis (may take a moment)...")
    results$mediation <- tryCatch(
      cedm_mediation(
        data        = data,
        outcome_var = outcome_var,
        ses_var     = ses_var,
        health_var  = health_var,
        covariates  = covariates,
        n_boot      = n_boot,
        sensitivity = FALSE,
        seed        = seed
      ),
      error = function(e) {
        message("Mediation failed: ", e$message)
        NULL
      }
    )
  } else {
    .msg("Step 4/7: Mediation skipped.")
    results$mediation <- NULL
  }

  # Step 5: REWB decomposition
  .msg("Step 5/7: Running REWB within-between decomposition...")
  results$rewb <- tryCatch(
    cedm_rewb(
      data        = data,
      outcome_var = outcome_var,
      health_var  = health_var,
      id_var      = id_var,
      time_var    = time_var,
      ses_var     = ses_var,
      covariates  = covariates,
      cluster_var = cluster_var
    ),
    error = function(e) {
      message("REWB failed: ", e$message)
      NULL
    }
  )

  # Step 6: Spline moderation
  if (run_spline) {
    .msg("Step 6/7: Running nonlinear spline moderation...")
    results$spline <- tryCatch(
      cedm_spline_moderation(
        data        = data,
        outcome_var = outcome_var,
        ses_var     = ses_var,
        health_var  = health_var,
        covariates  = covariates,
        cluster_var = cluster_var,
        plot        = TRUE
      ),
      error = function(e) {
        message("Spline moderation failed: ", e$message)
        NULL
      }
    )
  } else {
    .msg("Step 6/7: Spline moderation skipped.")
    results$spline <- NULL
  }

  # Step 7: Trajectory clustering
  if (run_trajectory) {
    .msg("Step 7/7: Clustering health trajectories...")
    results$trajectory <- tryCatch(
      cedm_trajectory(
        data        = data,
        health_var  = health_var,
        id_var      = id_var,
        time_var    = time_var,
        k           = k_trajectories,
        outcome_var = outcome_var,
        ses_var     = ses_var,
        seed        = seed,
        plot        = TRUE
      ),
      error = function(e) {
        message("Trajectory clustering failed: ", e$message)
        NULL
      }
    )
  } else {
    .msg("Step 7/7: Trajectory clustering skipped.")
    results$trajectory <- NULL
  }

  # Sensitivity analysis on the base production model
  .msg("Running sensitivity analysis on key model terms...")
  results$sensitivity <- tryCatch(
    cedm_sensitivity(results$model_base),
    error = function(e) NULL
  )

  # Plots
  .msg("Generating summary plots...")
  plots <- list()
  plots$regimes <- tryCatch(
    plot_regimes(data, ses_var = ses_var, health_var = health_var,
                 outcome_var = outcome_var),
    error = function(e) NULL
  )
  plots$interaction <- tryCatch(
    plot_cedm_interaction(
      cedm_prod_result = results$model_regime,
      data             = data,
      ses_var          = ses_var,
      health_var       = health_var,
      regime_var       = "cedm_regime",
      outcome_var      = outcome_var
    ),
    error = function(e) NULL
  )
  if (!is.null(results$spline))     plots$spline     <- results$spline$plot
  if (!is.null(results$trajectory)) plots$trajectory <- results$trajectory$plot

  results$plots <- plots

  class(results) <- "cedm_pipeline"
  .msg("CEDM pipeline complete.")
  return(results)
}

#' @export
print.cedm_pipeline <- function(x, ...) {
  cat("=== CEDM Full Analysis Pipeline Results ===\n\n")
  cat("Ecological Capability Regime Distribution:\n")
  print(x$regime_table)
  cat("\n--- Base Model (SES x Health) ---\n")
  if (!is.null(x$model_base)) {
    cat("Key interaction term:\n")
    base_coefs      <- x$model_base$coefficients
    interaction_row <- base_coefs[grepl(":", base_coefs$term), ]
    if (nrow(interaction_row) > 0) print(interaction_row)
  }
  if (!is.null(x$mediation)) {
    cat("\n--- Mediation Summary ---\n")
    cat(sprintf("  Proportion of SES effect mediated through health: %.4f%%\n",
                x$mediation$prop_mediated * 100))
  }
  if (!is.null(x$sensitivity)) {
    cat("\n--- Sensitivity (Capability Stability) ---\n")
    x_plain <- x$sensitivity[, c("term", "rir_pct", "stability")]
    class(x_plain) <- "data.frame"
    print(x_plain, row.names = FALSE)
  }
  invisible(x)
}
