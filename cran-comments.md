## Resubmission:
This is a resubmission, which has incorporated the following changes:


## Test environments
* local Windows 10 install, R 3.5.0
* ubuntu 14.04.5 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
              user system elapsed
AGread        9.60   2.44   14.91
read_gt3x     6.08   1.87    7.93
collapse_gt3x 5.39   0.06    5.50

The referenced examples are wrapped in \donttest{}.

## Reverse dependencies
There were no ERRORs, WARNINGs, or NOTEs for reverse dependencies.
