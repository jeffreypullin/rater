# package startup messages

# necessary machine info taken from rstan startup message
.onAttach <- function(...) {
  packageStartupMessage("* The rater package uses `Stan` to fit bayesian models.\n",
                        "* If you are working on a local, multicore CPU with excess RAM please call:\n",
                        "* options(mc.cores = parallel::detectCores())\n",
                        "* This will allow Stan to run inference on multiple cores in parallel.")
}
