## Resubmission:
This update is in response to the archiving of 'binaryLogic', a previous 
dependency. The dependency has been circumvented, and additional minor
enhancements have been made throughout the package.

## Test environments
* local Windows 10 install, R 4.0.5
* x86_64-pc-linux-gnu (64-bit) on R-hub (R 3.6.2)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies
revdep_check indicates no problems for `Observation` or `Sojourn`.
    The latter has one NOTE that AGread (and caret) is imported
    but not used, which I will fix on next submission (I am the
    maintainer).
