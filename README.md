
[![Project Status: Active ? The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build
Status](https://travis-ci.org/paulhibbing/AGread.svg?branch=master)](https://travis-ci.org/paulhibbing/AGread)
[![License](https://img.shields.io/badge/licence-MIT-blue.svg)](https://opensource.org/licenses/MIT)

-----

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AGread)](https://cran.r-project.org/package=AGread)
[![packageversion](https://img.shields.io/badge/Package%20version-0.2.2.9000-orange.svg?style=flat-square)](commits/master)

-----

[![Last-changedate](https://img.shields.io/badge/last%20change-2019--09--15-yellowgreen.svg)](/commits/master)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# AGread

AGread is for bringing ActiGraph sensor data into R. It is designed to
streamline and standardize the file-reading process, regardless of which
file format is provided (currently supporting `.gt3x` and `.csv`).

AGread can be used flexibly to develop new methods for handling
ActiGraph data, or to invoke existing methods, many of which exist in
other R packages (e.g.
[PhysicalActivity](https://cran.r-project.org/package=PhysicalActivity)
and [TwoRegression](https://cran.r-project.org/package=TwoRegression)),
or will soon.

New in AGread 1.0.0 (under development), `Rcpp` has been invoked to
speed up the process of reading `.gt3x` files. There is still some
trouble with the comparability of parsed SENSOR\_DATA packet values and
the `*-IMU.csv` output from ActiLife. That is, IMU values are usually
equivalent whether read from `.gt3x` or `*-IMU.csv`, but for ~2-3% of
values there are puzzling differences. Thus, the binary file reading
function of AGread should still be considered “under development”, but
is safe to use for primary accelerometer data (i.e., ACTIVITY2 packets).

## Installation

You can install the development version of AGread from github with:

``` r
# install.packages("devtools")
devtools::install_github("paulhibbing/AGread")
```

Windows users, make sure you have
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed
before running the above.

Alternatively, AGread v0.2.2 is available on CRAN. It does not use Rcpp,
and thus can be slow for binary reading. Install it with:

``` r
install.packages("AGread")
```
