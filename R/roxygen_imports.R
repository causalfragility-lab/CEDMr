# =============================================================
# FIX 3: Missing @importFrom declarations
#
# These roxygen2 tags go in ONE central file (a common pattern).
# Run devtools::document() after adding this file so the
# NAMESPACE is regenerated.
#
# WHAT WAS WRONG:
#   a) complete.cases() used in cedm_trajectory.R but not
#      declared → "no visible global function definition"
#   b) := and .data come from rlang; using them without
#      importFrom generates "no visible binding" NOTEs
#   c) %>% imported via @importFrom magrittr %>% scattered in
#      multiple files — centralise here instead
# =============================================================

#' @importFrom stats complete.cases
#' @importFrom rlang .data :=
#' @importFrom magrittr %>%
NULL
