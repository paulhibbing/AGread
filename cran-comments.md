## Resubmission:
This update is being submitted in response to an email from
    Prof Brian Ridley, related to correcting an error that
    occurred for the Solaris build.
    
* The ambiguous call to overloaded `pow` function has been
    corrected, along with the call to `abs` using a non-integer.

## Test environments
* local Windows 10 installs, interchanging R 3.5.0 and R 3.5.2
* ubuntu 16.04.6 (on travis-ci), R 3.6.1 and R Under Development
* x86_64-pc-linux-gnu (64-bit) on Rhub (R 3.6.2)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies
revdep_check indicates no problems for `Observation` or `Sojourn`.
    The latter has one NOTE that AGread (and caret) is imported
    but not used, which I will fix on next submission (I am the
    maintainer).
