## rater 1.2.0

This is a new version of the package fixes bugs and adds many new model comparison function.

## Test environments

* local mac OS install, R 4.1.0
* ubuntu 16.04 (on github actions), R 4.1.0
* mac OS 10.15.4 (on github actions) R 4.1.0
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.1.0
* win-builder (devel)

## R CMD check results

Dawid and Skene are names and not misspelled.

rater uses rstan which causes the two additional notes.  

0 errors | 0 warnings | 3 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jeffrey Pullin <jeffrey.pullin@gmail.com>'
  
* checking installed package size ... NOTE
    installed size is  5.8Mb
    sub-directories of 1Mb or more:
    libs   4.6Mb

* checking for GNU extensions in Makefiles ... NOTE
    GNU make is a SystemRequirements.
