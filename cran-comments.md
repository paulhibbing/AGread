## Test environments
* local Windows 10 install, R 3.4.3
* ubuntu 14.04.5 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  ActiGraph

This is the first submission of AGread. The "possibly mis-spelled word"
    is the name of a device whose data are manipulated by the package
    functions.

## Other comments

In addition to the original functions supplied in this package, the
    following data sets and documentation are duplicated (or modifed)
    from the `TwoRegression` package, of which I am the author and
    maintainer:

* imu_to_check
* imu_to_collapse
* raw_to_collapse

The following functions are duplicated (or modified) internal
    functions from the `TwoRegression` package:

* get_minute
* get_day_of_year
* check_columns
* check_second
* get_raw_file_meta
* get_imu_file_meta
* AG_collapse
* imu_collapse
* imu_filter_gyroscope
* classify_magnetometer
* get_VM

The following functions are duplicated (or modified) exported
    functions from the `TwoRegression` package:

* read_AG_IMU (re-named from read_IMU)
* read_AG_raw

In retrospect, a standalone package (i.e., `AGread`) was always
    the best place for these functions and data sets to live.
    Therefore, my intent is to transition them from `TwoRegression`
    to `AGread` by:

* deprecating in the next submission of `TwoRegression`
* removing, with a major version increment, in the subsequent of
    submission of `TwoRegression`

## Reverse dependencies

There are currently no reverse dependencies for AGread. As implied
above, `TwoRegression` will become a reverse dependency.
