## Resubmission:
This is a resubmission, which has incorporated the following changes:

* Added support for reading binary .gt3x files
* Added reintegration function
* Expanded support for files with inclinometer columns, to allow un-coded or
    dummy-coded data
* Added chunking as an option for reading RAW.csv files that are too large
    and cause memory issues
* Added option to apply autocalibration with GGIR
* Added option to return raw (uncollapsed) data (#3, @srlamunion)
* Updated time stamp calculations and time zone specifications where needed
* Set up automated testing for continued development

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
