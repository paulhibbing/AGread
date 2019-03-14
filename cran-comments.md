## Resubmission:
This is a resubmission, which is being made to resolve the use of
    multiple licenses in the previously-rejected submission (0.2.1).
    After discussing with the copyright holders, the package license
    has been changed from GPL-3 to MIT, to accommodate the usage
    conditions of material derived from
    <https://github.com/actigraph/GT3X-File-Format>.

## Test environments
* local Windows 10 installs, interchanging R 3.5.0 and R 3.5.2
* ubuntu 14.04.5 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

* checking examples ... NOTE
    Examples with CPU or elapsed time > 5s
              user system elapsed
    AGread    9.70   2.39   12.48
    read_gt3x 5.55   1.75    7.28

The affected examples are wrapped in donttest{}.

## Reverse dependencies
There were no ERRORs, WARNINGs, or NOTEs for reverse dependencies.
