# rater (development version)

* The plot produced for the pi parameter has been changed. The new plot represents the uncertainty in the point estimates when MCMC has been used to fit the model.

* Prior parameters for the Dawid-Skene and class conditional Dawid-Skene models have been altered slightly to improve convergence of optimization when the number of classes is small. 

* `summary.mcmc_fit` now displays the number of remaining parameters correctly.

* Added the `as_mcmc.list()` function to convert MCMC fits to {coda} `mcmc.list` objects.

# rater 1.0.0

* Initial release
