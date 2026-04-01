## Resubmission

This is a resubmission. The following issues raised by the CRAN reviewer
have been addressed:

1. Removed "Tools for" from the start of the Title field in DESCRIPTION.

2. Removed single quotes around acronyms in DESCRIPTION (CEDM, SES, REWB,
   ITCV, RIR) as they are not required for acronyms.

3. Replaced \dontrun{} with \donttest{} in examples for functions involving
   bootstrapping or clustering (cedm_mediation, cedm_full_pipeline,
   cedm_trajectory), where examples are executable by the user but may
   take more than a few seconds.

4. Unwrapped \dontrun{} entirely for fast-running examples (cedm_rewb,
   cedm_spline_moderation), as these execute well within 5 seconds.

5. Fixed inst/validation/cedm_paper_simulation.R to write plot output
   to tempdir() instead of the working directory, in compliance with
   CRAN file-writing policies.

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

- This is the second submission of CEDMr (v0.1.0).
- The package provides tools for implementing the
  Capability-Ecological Developmental Model (CEDM) in longitudinal and
  multilevel data.
- Functions correspond to key components of the framework, including
  moderation, mediation, decomposition, and sensitivity analysis.

---

## CRAN policies

- All URLs in documentation are valid and reachable.
- Examples run within a few seconds on a standard machine.
- More computationally intensive examples are wrapped in \donttest{}.
- No external system calls or internet access are required.
- No files are written outside of tempdir().
