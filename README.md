# rateR

The rateR package is designed to allow the easy fitting of Bayesian versions of nosiy categorical rating models using [Stan](https://mc-stan.org/).

***IMPORTANT***

This pacakge is still in the very early stages of development interfaces will change *without* warning

To install the pacakge use:

``` r
# we need a specific version of rstantools:
devtools::install_github(“stan-dev/rstantools”, ref = "c3c59fb")

# you may need to run: `remove.packages("rstantools")` and restart R if rstantools is already loaded

devtools::install_github("Voltemand/rateR")

# There will be many compiler warnings. Don't worry they are harmles but difficult to remove
```

Example usage:

```r
library(rateR)

data(anesthesia) # example dataset

# fit the bayesian Dawid and Skene model with default priors
fit <- mcmc(anesthesia, dawid_skene())

# plot the latent class
plot_latent_class(fit)
```
