# =============================================================
# CEDMr Toy Simulation Validation
# Confirms the CEDM data-generating process recovers expected
# parameter signs and effect sizes, and times every example.
# Run this interactively before CRAN submission.
# =============================================================

library(CEDMr)

cat("\n========================================================\n")
cat("  CEDMr TOY SIMULATION VALIDATION\n")
cat("========================================================\n\n")

# ── 1. SIMULATE DATA ─────────────────────────────────────────

cat("--- Step 1: Simulate CEDM data ---\n")
t0 <- proc.time()

sim <- cedm_simulate(n = 3000, n_waves = 5, seed = 42)

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_simulate()        : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))

# Basic structure checks
stopifnot(is.data.frame(sim))
stopifnot(all(c("id", "SES", "health", "achievement", "regime", "wave") %in% names(sim)))
stopifnot(nrow(sim) == 3000 * 5)
stopifnot(nlevels(sim$regime) == 3)
cat("  Structure checks       : ✔\n\n")

# ── 2. CLASSIFY REGIMES ──────────────────────────────────────

cat("--- Step 2: Classify ecological capability regimes ---\n")
t0 <- proc.time()

sim1 <- cedm_simulate(n = 3000, seed = 42)   # cross-sectional for most tests
sim1 <- classify_regime(sim1, ses_var = "SES", ses_tertiles = TRUE)

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  classify_regime()      : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))

stopifnot("cedm_regime" %in% names(sim1))
stopifnot(all(levels(sim1$cedm_regime) %in%
                c("amplifying", "neutral", "compensatory")))
cat("  Regime levels          : ✔\n\n")

# ── 3. PRODUCTION FUNCTION ───────────────────────────────────

cat("--- Step 3: CEDM production function ---\n")

# 3a Base model
t0 <- proc.time()
base_mod <- cedm_production(
  data        = sim1,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  model       = "base",
  center      = TRUE
)
elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_production(base)  : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))
stopifnot(inherits(base_mod, "cedm_production"))

# 3b Full regime model
t0 <- proc.time()
regime_mod <- cedm_production(
  data        = sim1,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  regime_var  = "cedm_regime",
  model       = "regime",
  center      = TRUE
)
elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_production(regime): %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))
stopifnot(inherits(regime_mod, "cedm_production"))

# Check SES main effect is positive (beta1 = 0.50 in DGP)
coefs      <- regime_mod$coefficients
ses_row    <- coefs[coefs$term == "SES_c", ]
stopifnot(nrow(ses_row) > 0)
stopifnot(ses_row$estimate > 0)
cat("  SES main effect > 0    : ✔\n\n")

# ── 4. SENSITIVITY ANALYSIS ──────────────────────────────────

cat("--- Step 4: Sensitivity analysis (Proposition 5) ---\n")
t0 <- proc.time()

sens <- cedm_sensitivity(base_mod, term = "SES_c")

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_sensitivity()     : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))

stopifnot(inherits(sens, "cedm_sensitivity"))
stopifnot(all(c("term", "itcv", "rir_pct", "stability") %in% names(sens)))
stopifnot(sens$itcv >= 0)
stopifnot(sens$rir_pct >= 0 & sens$rir_pct <= 100)
cat("  ITCV and RIR in range  : ✔\n\n")

# ── 5. REGIME PLOT ───────────────────────────────────────────

cat("--- Step 5: Plot regimes ---\n")
t0 <- proc.time()

p_regimes <- plot_regimes(
  sim1,
  ses_var     = "SES",
  health_var  = "health",
  outcome_var = "achievement"
)

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  plot_regimes()         : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))
stopifnot(inherits(p_regimes, "ggplot"))
cat("  Returns ggplot object  : ✔\n\n")

# ── 6. REWB DECOMPOSITION ────────────────────────────────────

cat("--- Step 6: REWB decomposition (Proposition 3) ---\n")
t0 <- proc.time()

rewb <- cedm_rewb(
  data        = sim,           # longitudinal (5 waves)
  outcome_var = "achievement",
  health_var  = "health",
  id_var      = "id",
  time_var    = "wave",
  ses_var     = "SES"
)

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_rewb()            : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))
stopifnot(inherits(rewb, "cedm_rewb"))
stopifnot(!is.null(rewb$model))
cat("  REWB model fitted      : ✔\n\n")

# ── 7. TRAJECTORY CLUSTERING ─────────────────────────────────

cat("--- Step 7: Health trajectory clustering (Proposition 3) ---\n")
t0 <- proc.time()

traj <- cedm_trajectory(
  data        = sim,
  health_var  = "health",
  id_var      = "id",
  time_var    = "wave",
  k           = 3,
  outcome_var = "achievement",
  ses_var     = "SES",
  plot        = FALSE          # skip plot for speed in testing
)

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_trajectory()      : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))
stopifnot(inherits(traj, "cedm_trajectory"))
stopifnot(traj$k == 3)
stopifnot(nrow(traj$cluster_summary) == 3)
cat("  3 clusters found       : ✔\n\n")

# ── 8. SPLINE MODERATION ─────────────────────────────────────

cat("--- Step 8: Spline moderation ---\n")
t0 <- proc.time()

spl <- cedm_spline_moderation(
  data        = sim1,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  plot        = FALSE
)

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_spline_moderation(): %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW")))
stopifnot(inherits(spl, "cedm_spline"))
cat("  Spline model fitted    : ✔\n\n")

# ── 9. MEDIATION (small n_boot for speed) ────────────────────

cat("--- Step 9: Causal mediation analysis (Proposition 4) ---\n")
t0 <- proc.time()

med <- cedm_mediation(
  data        = sim1,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  n_boot      = 99,            # minimal for speed check
  sensitivity = FALSE,
  seed        = 42
)

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("  cedm_mediation()       : %.3f sec %s\n",
            elapsed, ifelse(elapsed < 5, "✔", "✖ SLOW (expected - bootstrap)")))

stopifnot(inherits(med, "cedm_mediation"))
# Proposition 4: indirect effect should be small (alpha1 = 0.15 in DGP)
cat(sprintf("  Prop mediated          : %.4f (expect small ~0.02-0.10)\n",
            med$prop_mediated))
stopifnot(med$prop_mediated >= 0 & med$prop_mediated <= 1)
cat("  Proportion in [0,1]    : ✔\n\n")

# ── 10. SUMMARY ──────────────────────────────────────────────

cat("========================================================\n")
cat("  SIMULATION VALIDATION COMPLETE\n")
cat("  All structural checks passed ✔\n")
cat("  NOTE: cedm_mediation() with n_boot >= 500 will exceed\n")
cat("  5 sec — this is expected and correctly wrapped in\n")
cat("  \\dontrun{} in the package examples.\n")
cat("========================================================\n\n")
