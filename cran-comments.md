## Resubmission

This is a resubmission. The following NOTEs from the initial CRAN pre-check
have been addressed:

1. Clarified the use of acronyms in DESCRIPTION (e.g., 'CEDM', 'SES', 'BMI',
   'REWB', 'ITCV', 'RIR') to ensure consistency and readability.

2. Removed the VignetteBuilder field from DESCRIPTION. No vignettes are
   included in this submission.

---

## R CMD check results

0 errors | 0 warnings | 0 notes

---

## Test environments

- Windows (R release)   : OK
- macOS (R release)     : OK
- Ubuntu (R release)    : OK
- Ubuntu (R oldrel-1)   : OK
- Ubuntu (R-devel)      : 1 NOTE (no errors or warnings)

---

## Submission notes

- This is the first CRAN submission of CEDMr (v0.1.0).
- The package provides tools for implementing the
  Capability-Ecological Developmental Model (CEDM) in longitudinal and
  multilevel data.
- Functions correspond to key components of the framework, including
  moderation, mediation, decomposition, and sensitivity analysis.

---

## CRAN policies

- All URLs in documentation are valid and reachable.
- Examples run within a few seconds on a standard machine.
- More computationally intensive examples are wrapped in \\dontrun{}.
- No external system calls or internet access are required.
- No files are written outside of tempdir().
