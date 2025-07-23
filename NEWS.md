# rater 1.3.2

Fix the compatibility of plotting tests with upcoming ggplot2 version.

# rater 1.3.1

* Fix the description of the number of raters in the anesthesia data. The documentation had erroneously stated there were four anesthetists, not five.

* Update the stan code for compatibility with rstan v2.26.0 (@andrjohns)

# rater 1.3.0

* Updated the Stan implementation, priors, and initialisation points of the hierarchical Dawid-Skene model, leading to much more reliable convergence. 

* Added the ability to visualise the theta parameter with uncertainty.

* Added row names to the output of `class_probabilities()`.

* Added the ability to specify the column names of long format data passed to `rater()`.

* Added `simulate_dawid_skene_model()` and `simulate_hier_dawid_skene_model()` to simulate data from the Dawid-Skene and hierarchical Dawid-Skene models.

* Re-export `loo_compare()`.

* Allowed the theta parameter to be extracted from the hierarchical Dawid-Skene model.

# rater 1.2.0

* Add `waic()` function for model comparison

* Silence warnings with the latest ggplot2 version 

* Fix validation bug in `posterior_predict()`

# rater 1.1.0

* `summary()` now works with the class conditional and hierarchical Dawid-Skene models.

* All functions applied to fitted class conditional Dawid-Skene models will automatically convert the relevant parameters of the model into a full theta parameter equivalent to the Dawid-Skene model. This is designed to allow easier comparison of the class conditional model with the full Dawid-Skene model.

* Plotting via `plot()` of the `rater_fit` object has been changed in several ways. `plot.rater_fit` now:

  - Only returns one plot 
  - Only returns the theta plot by default
  - Exposes the `prob`, `which` (called `rater_index`) and new `item_index` 
    arguments in the plot generic.
    
* Add the ability to only plot a subset of items when plotting the class probabilities. This can be controlled by the new `item_index` argument to `plot()`

* Added the function `wide_to_long()` to convert wide data to long data.

* Add the option `data_format = "wide"` to `rater()` to allow wide data to be passed into `rater()` directly.

* Added the `get_stanfit()` function to extract the underlying stanfit object from a rater fit object.

* Added an implementation of the `posterior_predict` generic from {rstantools} allowing simulation from the posterior predictive distribution of fitted standard, and class conditional, Dawid-Skene models. (The hierarchical Dawid-Skene model is not yet supported).

* Added an implementation of the `prior_summary` generic from {rstantools} for `rater_fit` objects.

* Add the `loo.rater_fit` method to allow the calculation of loo, a modern Bayesian model comparison metric, for rater models. loo values can be compared using the excellent {loo} package.

* Added the `loo.rater_fit` method to allow the calculation of loo, a modern Bayesian model comparison metric, for rater models. loo values can be compared using the excellent {loo} package.

* Rater specific prior parameters can now be used in the Dawid-Skene model for both grouped and long data. In practice this means that it is now possible to pass a J * K * K array for `beta` into `dawid_skene()` which encodes a K * K prior parameter for each of the J raters' error matrices. For backwards compatibility and ease of use it is still possible to pass a single matrix for `beta` which will still be interpreted as the prior parameter for all the of the raters' error matrices.

* The plot produced for the pi parameter has been changed. The new plot represents the uncertainty in the point estimates when MCMC has been used to fit the model.

* Prior parameters for the Dawid-Skene and class conditional Dawid-Skene models have been altered slightly to improve convergence of optimization when the number of classes is small. 

* `summary.mcmc_fit` now displays the number of remaining parameters correctly.

* Added the `as_mcmc.list()` function to convert MCMC fits to {coda} `mcmc.list` objects.

# rater 1.0.0

* Initial release
