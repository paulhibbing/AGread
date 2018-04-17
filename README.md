
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/paulhibbing/AGread.svg?branch=master)](https://travis-ci.org/paulhibbing/AGread)

# AGread

AGread is for automatically detecting the format of .csv data files from
ActiGraph accelerometers, and bringing the data into R. It is designed
to streamline and standardize the file-reading process, prior to further
processing. Thus, it can be used flexibly to develop new methods for
handling ActiGraph data, or to invoke existing methods, many of which
exist in other R packages (e.g.
[PhysicalActivity](https://cran.r-project.org/package=PhysicalActivity)
and [TwoRegression](https://cran.r-project.org/package=TwoRegression)),
or will soon.

## Installation

You can install AGread from github with:

``` r
# install.packages("devtools")
devtools::install_github("paulhibbing/AGread")
```
