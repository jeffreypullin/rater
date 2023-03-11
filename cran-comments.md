## rater 1.3.0

This is a new version of the package that adds new functionality focused on the hierarchical Dawid-Skene model.

## Test environments

* local mac OS install, R 4.2.2
* ubuntu 22.04 (on github actions), R 4.2.2
* mac OS Monterey 12 (on github actions) R 4.2.2
* Microsoft Windows Server 2022 (on github actions) R 4.2.2
* win-builder (devel)

## R CMD check results

Dawid and Skene are names and not misspelled.

rater uses the rstan package which causes the two additional notes.  

0 errors | 0 warnings | 3 notes

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jeffrey Pullin <jeffrey.pullin@gmail.com>'
  
* checking installed package size ... NOTE
    installed size is  5.8Mb
    sub-directories of 1Mb or more:
    libs   4.6Mb

* checking for GNU extensions in Makefiles ... NOTE
    GNU make is a SystemRequirements.
