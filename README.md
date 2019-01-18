# rateR

The rateR package is designed to allow the easy fitting of Bayesian versions of nosiy categorical rating models using [Stan](https://mc-stan.org/).

***IMPORTANT***

This pacakge is still in the very early stages of development interfaces will change *without* warning

To install the pacakge use:

``` r
devtools::install_github("Voltemand/rateR")
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
