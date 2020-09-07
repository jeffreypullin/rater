## Resubmission 

This is a resubmission with changes based on CRAN's comments. In this version 
I have: 

* Added a citation to the Description. Note that the added DOI triggers a 
possibly invalid DOI note as the paper is behind a paywall on JSTOR. 

* Changed from using \dontrun to \donttest in the examples

## Test environments

* local mac OS install, R 4.0.2
* ubuntu 16.04 (on github actions), R 4.0.2
* mac OS 10.15.4 (on github actions) R 4.0.2
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.0.2
* win-builder (devel)

## R CMD check results

Dawid and Skene are names and not mispelled. The DOI points to JSTOR causing 
the note. 

rater uses rstan which causes the two additional notes.  

0 errors | 0 warnings | 3 notes

* Maintainer: 'Jeffrey Pullin <jeffrey.pullin@gmail.com>'
  
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    Dawid (12:50, 12:70)
    Skene (12:56, 13:3)
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.2307/2346806
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
 
* checking installed package size ... NOTE
    installed size is  5.8Mb
    sub-directories of 1Mb or more:
    libs   5.2Mb

* checking for GNU extensions in Makefiles ... NOTE
    GNU make is a SystemRequirements.
