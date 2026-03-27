# CEDMr <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/causalfragility-lab/CEDMr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/causalfragility-lab/CEDMr/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

**CEDMr** implements the **Capability-Ecological Developmental Model (CEDM)**
(Hait, 2025), a theoretical framework for studying how socioeconomic status
(SES), health indicators (e.g., BMI), and ecological contexts jointly shape
academic achievement across childhood.

The package operationalizes all five CEDM theoretical propositions into
directly callable R functions and is designed to work with any longitudinal
multi-level dataset, including ECLS-K:2011 and similar national surveys.

---

## The Five CEDM Propositions

| # | Proposition | Function |
|---|-------------|----------|
| 1 | **Capability-Conversion Production Function** — SES converts to achievement through health as a moderating capability constraint | `cedm_production()` |
| 2 | **Ecological Regime Heterogeneity** — Three regimes (amplifying, neutral, compensatory) govern the SES × health interaction | `classify_regime()`, `cedm_production()` |
| 3 | **Developmental Recursion** — Chronic between-child health differences (not transient fluctuations) drive achievement gaps | `cedm_rewb()`, `cedm_trajectory()` |
| 4 | **Weak Mediation, Strong Moderation** — Health mediates SES effects weakly but moderates them strongly | `cedm_mediation()` |
| 5 | **Capability Stability** — Robustness of SES–achievement effects to unmeasured confounding | `cedm_sensitivity()` |

---

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("causalfragility-lab/CEDMr")
```

---

## Quick Start

### Simulate CEDM data

```r
library(CEDMr)

# Simulate 3000 observations across three ecological regimes
sim <- cedm_simulate(n = 3000, n_waves = 5, seed = 42)
table(sim$regime)
#>
#>   amplifying compensatory      neutral
#>         1000         1000         1000
```

### Classify ecological capability regimes

```r
sim <- classify_regime(sim, ses_var = "SES", ses_tertiles = TRUE)
table(sim$cedm_regime)
```

### Fit the CEDM production function

```r
# Base model: SES × health interaction
base_model <- cedm_production(
  data        = sim,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  model       = "base",
  center      = TRUE
)
print(base_model)

# Full regime model: SES × health × regime (three-way interaction)
regime_model <- cedm_production(
  data        = sim,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  regime_var  = "cedm_regime",
  model       = "regime",
  center      = TRUE
)
print(regime_model)
```

### Causal mediation analysis (Proposition 4)

```r
med <- cedm_mediation(
  data        = sim,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  n_boot      = 500,
  seed        = 42
)
print(med)
```

### Within-between decomposition (Proposition 3)

```r
rewb <- cedm_rewb(
  data        = sim,
  outcome_var = "achievement",
  health_var  = "health",
  id_var      = "id",
  time_var    = "wave",
  ses_var     = "SES"
)
print(rewb)
```

### Health trajectory clustering (Proposition 3)

```r
traj <- cedm_trajectory(
  data        = sim,
  health_var  = "health",
  id_var      = "id",
  time_var    = "wave",
  k           = 3,
  outcome_var = "achievement",
  ses_var     = "SES"
)
print(traj)
traj$plot
```

### Sensitivity analysis (Proposition 5)

```r
sens <- cedm_sensitivity(base_model, term = "SES")
print(sens)
```

### Run the full pipeline in one call

```r
pipeline <- cedm_full_pipeline(
  data        = sim,
  outcome_var = "achievement",
  ses_var     = "SES",
  health_var  = "health",
  id_var      = "id",
  time_var    = "wave",
  n_boot      = 200,
  seed        = 42
)
print(pipeline)
pipeline$plots$regimes
pipeline$plots$interaction
```

---

## Function Reference

| Function | Description |
|----------|-------------|
| `cedm_simulate()` | Simulate data from the CEDM data-generating process |
| `classify_regime()` | Classify observations into amplifying / neutral / compensatory regimes |
| `cedm_production()` | Fit the CEDM capability-conversion production function |
| `cedm_mediation()` | Doubly robust causal mediation analysis |
| `cedm_rewb()` | Random-effects within-between (REWB) decomposition |
| `cedm_trajectory()` | Longitudinal health trajectory clustering |
| `cedm_spline_moderation()` | Nonlinear moderation via restricted cubic splines |
| `cedm_sensitivity()` | Sensitivity analysis using Frank's ITCV and RIR |
| `cedm_full_pipeline()` | Run all CEDM analyses in a single call |
| `plot_regimes()` | Plot SES × health scatter by ecological regime |
| `plot_cedm_interaction()` | Plot predicted achievement by SES level and regime |

---

## Citation

If you use CEDMr in your research, please cite:

```
Hait, S. (2026). Socioeconomic Status, Health, and Academic Achievement:
A Capability-Ecological Developmental Model. Manuscript under review.
```

```
Hait, S. (2026). CEDMr: Capability-Ecological Developmental Model (CEDM)
Analysis Toolkit. R package version 0.1.0.
https://github.com/causalfragility-lab/CEDMr
```

---

## License

MIT © 2026 Subir Hait
