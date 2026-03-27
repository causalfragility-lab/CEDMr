#' Classify Ecological Capability Regimes
#'
#' Assigns each observation to one of three CEDM ecological capability regimes
#' (amplifying, neutral, compensatory) based on individual-level SES and
#' school/context-level opportunity index. Implements the formal operationalization
#' from Proposition 2 of the CEDM (Hait, 2025).
#'
#' @param data A data.frame containing the variables specified below.
#' @param ses_var Character string naming the SES variable (numeric).
#' @param opportunity_var Character string naming the ecological opportunity variable
#'   (e.g., school resources index, neighborhood opportunity score). If NULL,
#'   classification is based on SES alone using tertile cutpoints.
#' @param ses_cutpoint Numeric cutpoint for SES. Defaults to the sample median.
#' @param opportunity_cutpoint Numeric cutpoint for the opportunity variable.
#'   Defaults to the sample median.
#' @param method One of \code{"hard"} (discrete three-category assignment) or
#'   \code{"continuous"} (returns a continuous capability index). Default is \code{"hard"}.
#' @param ses_tertiles Logical. If TRUE and \code{opportunity_var} is NULL,
#'   classify regimes using SES tertiles only. Default FALSE.
#'
#' @return The original data.frame with an added \code{cedm_regime} column
#'   (factor: "amplifying", "neutral", "compensatory") and, when
#'   \code{method = "continuous"}, an additional \code{cedm_capability_index} column.
#'
#' @details
#' The CEDM defines three ecological capability regimes (Hait, 2025):
#' \itemize{
#'   \item \strong{Amplifying}: Low SES AND low ecological opportunity. Health
#'     constraints intensify the academic penalty of low SES.
#'   \item \strong{Neutral}: Moderate SES and/or moderate opportunity. Weak
#'     SES x health moderation.
#'   \item \strong{Compensatory}: High SES AND high ecological opportunity.
#'     Ecological supports buffer health constraints.
#' }
#' Formally: R_ij = Amplifying if SES_i < c_SES and O_j < c_O;
#'                  Compensatory if SES_i >= c_SES and O_j >= c_O;
#'                  Neutral otherwise.
#'
#' @references
#' Hait, S. (2025). Socioeconomic Status, Health, and Academic Achievement:
#' A Capability-Ecological Developmental Model. OSF Preprints.
#'
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   ses   = rnorm(500),
#'   opp   = rnorm(500),
#'   bmi   = rnorm(500, 25, 5),
#'   math  = rnorm(500, 100, 15)
#' )
#' df <- classify_regime(df, ses_var = "ses", opportunity_var = "opp")
#' table(df$cedm_regime)
#'
#' @export
classify_regime <- function(data,
                            ses_var,
                            opportunity_var = NULL,
                            ses_cutpoint = NULL,
                            opportunity_cutpoint = NULL,
                            method = c("hard", "continuous"),
                            ses_tertiles = FALSE) {

  method <- match.arg(method)

  if (!ses_var %in% names(data)) {
    stop(paste("ses_var '", ses_var, "' not found in data.", sep = ""))
  }

  ses <- data[[ses_var]]

  # Determine SES cutpoint
  if (is.null(ses_cutpoint)) {
    ses_cutpoint <- stats::median(ses, na.rm = TRUE)
  }

  if (ses_tertiles && is.null(opportunity_var)) {
    # Tertile-based classification when no opportunity index is available
    ses_tertile_cuts <- stats::quantile(ses, probs = c(1/3, 2/3), na.rm = TRUE)
    regime <- dplyr::case_when(
      ses < ses_tertile_cuts[1]  ~ "amplifying",
      ses >= ses_tertile_cuts[2] ~ "compensatory",
      TRUE                       ~ "neutral"
    )
  } else if (!is.null(opportunity_var)) {
    if (!opportunity_var %in% names(data)) {
      stop(paste("opportunity_var '", opportunity_var, "' not found in data.", sep = ""))
    }
    opp <- data[[opportunity_var]]
    if (is.null(opportunity_cutpoint)) {
      opportunity_cutpoint <- stats::median(opp, na.rm = TRUE)
    }

    regime <- dplyr::case_when(
      ses < ses_cutpoint & opp < opportunity_cutpoint  ~ "amplifying",
      ses >= ses_cutpoint & opp >= opportunity_cutpoint ~ "compensatory",
      TRUE                                              ~ "neutral"
    )
  } else {
    # SES-only fallback using median split
    regime <- dplyr::case_when(
      ses < ses_cutpoint ~ "amplifying",
      ses >= ses_cutpoint ~ "compensatory",
      TRUE ~ "neutral"
    )
    message("No opportunity_var supplied; classifying on SES alone (median split).")
  }

  data$cedm_regime <- factor(regime,
                             levels = c("amplifying", "neutral", "compensatory"))

  if (method == "continuous") {
    ses_z <- as.numeric(scale(ses))
    if (!is.null(opportunity_var)) {
      opp_z <- as.numeric(scale(data[[opportunity_var]]))
      data$cedm_capability_index <- (ses_z + opp_z) / 2
    } else {
      data$cedm_capability_index <- ses_z
    }
  }

  return(data)
}
