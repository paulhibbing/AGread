
[![Project Status: Active ? The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build
Status](https://travis-ci.org/paulhibbing/AGread.svg?branch=master)](https://travis-ci.org/paulhibbing/AGread)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

-----

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AGread)](https://cran.r-project.org/package=AGread)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.2.9000-orange.svg?style=flat-square)](commits/master)

-----

[![Last-changedate](https://img.shields.io/badge/last%20change-2018--04--19-yellowgreen.svg)](/commits/master)

<!-- README.md is generated from README.Rmd. Please edit that file -->

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

You can install the development version of AGread from github with:

``` r
# install.packages("devtools")
devtools::install_github("paulhibbing/AGread")
```

Or, AGread v0.1.2 is now available on CRAN, and can be installed with:

``` r
install.packages("AGread")
```
