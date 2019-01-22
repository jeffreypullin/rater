# rater <img src="man/figures/rater.png" align="right" width="160" />

[![Build
Status](https://travis-ci.com/Voltemand/rater.svg?branch=master)](https://travis-ci.com/Voltemand/rater)

The rater package is designed to allow the easy fitting of Bayesian
versions of nosiy categorical rating models using
[Stan](https://mc-stan.org/).

### IMPORTANT

This package is still in the very early stages of development interfaces
will change *without* warning

### INSTALLATION

``` r
# we need a specific version of rstantools
devtools::install_github(“stan-dev/rstantools”, ref = "c3c59fb")

# you may need to run: `remove.packages("rstantools")` and restart R if rstantools is already loaded

devtools::install_github("Voltemand/rater")

# There (unfortunately) will be many compiler warnings. 
# Don't worry they are harmless but difficult to remove
```

### Usage:

``` r
library(rater)

data(anesthesia) # example dataset

# fit the bayesian Dawid and Skene model with default priors
fit <- mcmc(anesthesia, dawid_skene())

# plot the latent class
plot(fit, "latent_class")
```
