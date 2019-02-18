## Resubmission:
This is a resubmission, which is being made to address the following:

* LICENSE was updated in the submission of version 0.2.0, but the
    changes were not documented. In this resubmission, DESCRIPTION has
    been updated accordingly. Specifically, copyright ownership has been
    clarified, related to the use of open source material from
    <https://github.com/actigraph/GT3X-File-Format>, and contributors have
    been listed from the same site. The license field of DESCRIPTION has
    been changed to `file LICENSE` to prevent misrepresentation of the
    package copyright.
    
* Dependency `reshape2` was removed to resolve a NOTE on
    <https://cran.r-project.org/web/checks/check_results_AGread.html> for
    r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, and
    r-patched-solaris-x86.
    
* Documentation has been updated.


## Test environments
* local Windows 10 install, R 3.5.0
* ubuntu 14.04.5 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were two NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

Days since last update: 1

Change to non-FOSS package license.
New license:
  file LICENSE
Old license:
  GPL-3 | file LICENSE

* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
              user system elapsed
AGread        9.60   2.44   14.91
read_gt3x     6.08   1.87    7.93
collapse_gt3x 5.39   0.06    5.50

The referenced examples are wrapped in \donttest{}.

## Reverse dependencies
There were no ERRORs, WARNINGs, or NOTEs for reverse dependencies.
