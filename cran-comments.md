## Resubmission:
This is an update incorporating various changes broadly
    related to data interpolation, reduced time complexities,
    robustness of file reading functions, and a bug fix for
    mismatched types in declaration/definition for one
    particular internal function.

## Test environments
* local Windows 10 installs, interchanging R 3.5.0 and R 3.5.2
* ubuntu 16.04.6 (on travis-ci), R 3.6.1 and R Under Development
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies
revdep_check indicates no problems for `Observation` or `Sojourn`.
    The latter has one NOTE that AGread (and caret) is imported
    but not used, which I will fix on next submission (I am the
    maintainer).
