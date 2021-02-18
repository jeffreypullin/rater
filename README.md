
# rater <img src="man/figures/rater.png" align="right" width="160" />

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rater)](https://cran.r-project.org/package=rater)
[![R build
status](https://github.com/jeffreypullin/rater/workflows/R-CMD-check/badge.svg)](https://github.com/jeffreypullin/rater/actions)
[![Coverage
status](https://codecov.io/gh/jeffreypullin/rater/branch/master/graph/badge.svg)](https://codecov.io/github/jeffreypullin/rater?branch=master)
![pkgdown](https://github.com/jeffreypullin/rater/workflows/pkgdown/badge.svg)
<!-- badges: end -->

**rater** provides tools for fitting and interrogating statistical
models of repeated categorical rating data. The package provides a
simple interface to fit a selection of these models, with arbitrary
prior parameters, using MCMC and optimisation provided by
[Stan](https://mc-stan.org/). A selection of functions are also provided
to plot parts of these models and extract key parameters.

## Example usage:

``` r
library(rater)

fit <- rater(anesthesia, "dawid_skene") # Sampling output suppressed.
```

Get the posterior mean of the “pi” parameter.

``` r
point_estimate(fit, "pi")
```

    ## $pi
    ## [1] 0.37586040 0.40654649 0.14337154 0.07422156

Plot the accuracy matrices of the raters.

``` r
plot(fit, "raters")
```

![](man/figures/README-plot-demo-1.png)<!-- -->

## Installation

**rater** requires the **rstan** package to fit models. Detailed
instructions to install **rstan** can be found
[here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)

### CRAN

Install **rater** from CRAN with:

``` r
install.packages("rater")
```

### Development

To install the development version of **rater** from GitHub run:

``` r
# install.packages("remotes")
remotes::install_github("jeffreypullin/rater")
```

#### Installation notes:

-   When installing from source, i.e. when installing the development
    version or installing from CRAN on Linux, the **Stan** models in the
    package will be compiled - this will lead to an install time of few
    minutes. Please be patient - this compilation means that **no**
    compilation is required when using the package

-   During compilation many warnings may be displayed in the terminal;
    these are harmless but impossible to suppress.
