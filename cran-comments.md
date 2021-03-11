## rater 1.1.0

This is a new version of the package which corrects the check problems and adds many new improvements and features.

## Test environments

* local mac OS install, R 4.0.2
* ubuntu 16.04 (on github actions), R 4.0.4
* mac OS 10.15.4 (on github actions) R 4.0.4
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.0.4
* win-builder (devel)

## R CMD check results

Dawid and Skene are names and not misspelled.

rater uses rstan which causes the two additional notes.  

0 errors | 0 warnings | 3 notes

* Maintainer: 'Jeffrey Pullin <jeffrey.pullin@gmail.com>'
  
  New submission
  
  Package was archived on CRAN
  
  Possibly mis-spelled words in DESCRIPTION:
    Dawid (12:50, 12:70)
    Skene (12:56, 13:3)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2021-01-10 as check problems were not
       corrected in time.
 
* checking installed package size ... NOTE
    installed size is  5.8Mb
    sub-directories of 1Mb or more:
    libs   5.2Mb

* checking for GNU extensions in Makefiles ... NOTE
    GNU make is a SystemRequirements.
