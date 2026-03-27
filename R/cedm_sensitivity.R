# =============================================================
# FIX 1: Infinite recursion in print.cedm_sensitivity
# ROOT CAUSE: subsetting a cedm_sensitivity data.frame with
#   x[, display_cols]  preserves the class, so print() calls
#   print.cedm_sensitivity again → infinite recursion.
# FIX: strip the class before calling print(), using
#   class(x_df) <- "data.frame"  OR  print.data.frame(...)
# =============================================================

#' @export
print.cedm_sensitivity <- function(x, ...) {
  cat("=== CEDM Sensitivity Analysis (Proposition 5: Capability Stability) ===\n")
  cat("Significance threshold (alpha):", attr(x, "alpha"), "\n\n")

  display_cols <- c("term", "estimate", "t_value", "p_value",
                    "itcv", "rir_pct", "rir_n", "stability")

  # --- THE FIX ---
  # Strip the "cedm_sensitivity" class so print() dispatches to
  # print.data.frame, not back to print.cedm_sensitivity.
  x_plain <- x[, display_cols, drop = FALSE]
  class(x_plain) <- "data.frame"          # <-- prevents recursion
  print(x_plain, row.names = FALSE)
  # ----------------

  cat("\nCEDM Interpretation (Capability Stability):\n")
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  [%s] %s\n", x$term[i], x$cedm_interpretation[i]))
  }
  invisible(x)
}
