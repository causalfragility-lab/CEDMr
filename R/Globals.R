# Suppress R CMD check NOTEs for non-standard evaluation (NSE) tokens.
# These names appear as bare symbols inside ggplot2::aes() and dplyr verbs,
# and as internal function references, which the static checker flags as
# "no visible binding" or "no visible global function definition".
# globalVariables() tells the checker they are intentional — it does not
# change runtime behaviour in any way.

utils::globalVariables(c(

  # ggplot2 aes() tokens — cedm_plots.R and cedm_spline_moderation.R
  "health",
  "predicted",
  "ses_level",

  # dplyr summarise() token — cedm_trajectory.R
  "mean_health",

  # cluster column created by k-means — cedm_trajectory.R
  "cluster",

  # rlang .data pronoun — cedm_rewb.R and cedm_trajectory.R
  ".data",

  # internal function called inside cedm_full_pipeline — cedm_pipeline.R
  "cedm_sensitivity"

))
