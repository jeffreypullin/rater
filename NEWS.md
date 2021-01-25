# rater (development version)

* Added an implementation of the `posterior_predict` generic from {rstantools} allowing simulation from the posterior predictive distribution of fitted standard, and class conditional, Dawid-Skene models. (The hierarchical Dawid-Skene model is not yet supported).

* Added an implementation of the `prior_summary` generic from {rstantools} for `rater_fit` objects.

* Added the `loo.rater_fit` method to allow the calculation of loo, a modern Bayesian model comparison metric, for rater models. loo values can be compared using the excellent {loo} package.

* Rater specific prior parameters can now be used in the Dawid-Skene model for both grouped and long data. In practice this means that it is now possible to pass a J * K * K array for `beta` into `dawid_skene()` which encodes a K * K prior parameter for each of the J raters' error matrices. For backwards compatibility and ease of use it is still possible to pass a single matrix for `beta` which will still be interpreted as the prior parameter for all the of the raters' error matrices.

* The plot produced for the pi parameter has been changed. The new plot represents the uncertainty in the point estimates when MCMC has been used to fit the model.

* Prior parameters for the Dawid-Skene and class conditional Dawid-Skene models have been altered slightly to improve convergence of optimization when the number of classes is small. 

* `summary.mcmc_fit` now displays the number of remaining parameters correctly.

* Added the `as_mcmc.list()` function to convert MCMC fits to {coda} `mcmc.list` objects.

# rater 1.0.0

* Initial release
