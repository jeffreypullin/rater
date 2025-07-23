## rater 1.3.2

This is a new patch release of the package that updates tests for compatibility with an upcoming release of ggplot2. 

## Test environments

* local mac OS install, R 4.3.1
* ubuntu 24.04 (on github actions), R 4.5.1
* mac OS Sonoma 14 (on github actions) R 4.5.1
* Microsoft Windows Server 2022 (on github actions) R 4.5.1
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
    libs   4.3Mb

* checking for GNU extensions in Makefiles ... NOTE
    GNU make is a SystemRequirements.
