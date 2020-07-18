#' Fit statistical models to repeated categorical rating data using Stan
#'
#' This functions allows the user to fit statistical models of noisy
#' categorical rating, based on the Dawid-Skene model, using Bayesian
#' inference. A variety of data formats and models are supported. Inference
#' is done using Stan, allowing models to be fit efficiently, using both
#' optimisation and Markov Chain Monte Carlo (MCMC).
#'
#' @param data A 2D data object: data.frame, matrix, tibble etc. with data in
#'   either long or grouped format.
#' @param model Model to fit to data - must be rater_model or a character
#'   string - the name of the model. If the character string is used, the
#'   prior parameters will be set to their default values.
#' @param method A length 1 character vector, either `"mcmc"` or `"optim"`.
#'   This will be fitting method used by Stan. By default `"mcmc"`
#' @param data_format A length 1 character vector, either `"long"` or
#'   `"grouped"`. The format that the passed data is in. Defaults to `"long"`.
#' @param inits The initialization points of the fitting algorithm
#' @param verbose Should `rater()` produce information about the progress
#'   of the chains while using the MCMC algorithm. Defaults to `TRUE`
#' @param ... Extra parameters which are passed to the Stan fitting interface.
#
#' @return An object of class rater_fit containing the fitted parameters.
#'
#' @details The default MCMC algorithm used by Stan is No U Turn Sampling
#'   (NUTS) and the default optimisation method is LGFGS. For MCMC 4 chains
#'   are run be default with 2000 iterations in total each.
#'
#' @importFrom rstan sampling optimizing
#' @importFrom utils capture.output
#'
#' @seealso [rstan::sampling()], [rstan::optimizing()]
#'
#' @examples
#'
#' \dontrun{
#'
#' # Fit a model using MCMC (the default).
#' mcmc_fit <- rater(anesthesia, "dawid_skene")
#'
#' # Fit a model using optimisation.
#' optim_fit <- rater(anesthesia, dawid_skene(), method = "optim")
#'
#' # Fit a model using passing data grouped data.
#' grouped_fit <- rater(caries, dawid_skene(), data_format = "grouped")
#'
#' }
#'
#' @export
#'
rater <- function(data,
                  model,
                  method = "mcmc",
                  data_format = "long",
                  inits = NULL,
                  verbose = TRUE,
                  ...) {

  method <- match.arg(method, choices = c("mcmc", "optim"))
  data_format <- match.arg(data_format, choices = c("long", "grouped"))

  model <- validate_model(model)
  data <- validate_input(data, model, data_format)

  stan_data_list <- as_stan_data(data, data_format)

  # Check the priors and data are consistent.
  check_K(stan_data_list, model)

  # Create the full passed info for stan and the initialisation points.
  stan_data <- c(stan_data_list, parse_priors(model, stan_data_list$K))

  if (is.null(inits)) {
    inits <- creat_inits(model, stan_data_list)
  }

  # TODO This could be made more complex if automatic switching is used.
  file <- get_stan_file(data_format, model)

  if (method == "mcmc") {

    if (!verbose) {
      capture.output(samples <- rstan::sampling(stanmodels[[file]], stan_data,
                                                init = inits, ...))
    } else {
      samples <- rstan::sampling(stanmodels[[file]], stan_data, init = inits,
                                 ...)
    }

    out <- new_mcmc_fit(model, samples, stan_data, data_format)
  } else if (method == "optim") {
    estimates <- rstan::optimizing(stanmodels[[file]], stan_data, init = inits, ...)
    out <- new_optim_fit(model, estimates, stan_data, data_format)
  }

  out
}

#' Convert validated passed data into data for Stan.
#'
#' @param data Validated passed data
#' @param data_format String specifying the format of the data
#'
#' @details The function accepts validated data. So we know that the data
#'   will be a data.frame with the appropriate column names. See
#'   [validate_data()] for details.
#'
#' @return A list of component data parts as required by the Stan models.
#'
#' @noRd
#'
as_stan_data <- function(data, data_format) {

  if (data_format == "long") {
     stan_data <- list(
       N = nrow(data),
       I = max(data$item),
       J = max(data$rater),
       K = max(data$rating),
       ii = data$item,
       jj = data$rater,
       y = data$rating
      )
  } else if (data_format == "grouped") {
    tally <- data[, ncol(data)]
    key <- data[, 1:(ncol(data) - 1)]
    stan_data <- list(
      N = nrow(data),
      K = max(key),
      J = ncol(key),
      key = key,
      tally = tally
    )
  }

  stan_data
}

#' Converts default prior parameter specification to full priors
#'
#' @param model The rater_model.
#' @param K The number of categories in the data.
#'
#' @return The fully realised prior parameters
#'
#' @noRd
#'
parse_priors <- function(model, K) {
  switch(get_file(model),
    "dawid_skene" = ds_parse_priors(model, K),
    "class_conditional_dawid_skene" =
      class_conditional_ds_parse_priors(model, K),
    "hierarchical_dawid_skene" = hier_ds_parse_priors(model, K),
    stop("Unsupported model type", call. = FALSE))
}

ds_parse_priors <- function(model, K) {
  # These are the priors from the stan manual.
  pars <- get_parameters(model)
  if (is.null(pars$alpha)) {
    pars$alpha <- rep(3, K)
  }
  if (is.null(pars$beta)) {
    pars$beta <- matrix(1, nrow = K, ncol = K)
    diag(pars$beta) <- 2.5 * K
  }
  pars
}

hier_ds_parse_priors <- function(model, K) {
  pars <- get_parameters(model)
  if (is.null(pars$alpha)) {
    pars$alpha <- rep(3, K)
  }
  pars
}

class_conditional_ds_parse_priors <- function(model, K) {
  pars <- get_parameters(model)
  if (is.null(pars$alpha)) {
    pars$alpha <- rep(3, K)
  }
  # These priors are selected so that:
  # mode(prior) = 0.7
  # alpha + beta - 2  = 5 (mode denom, usual beta distribution parameters)
  if (is.null(pars$beta_1)) {
    pars$beta_1 <- rep(4.5, K)
  }
  if (is.null(pars$beta_2)) {
    pars$beta_2 <- rep(2.5, K)
  }
  pars
}

#' Creates initialization points for the Stan.
#'
#' @param model rater model
#' @param stan_data data in list form
#'
#' @return Initialization points for the chains in the format required by Stan.
#'
#' @noRd
#'
creat_inits <- function(model, stan_data) {
  # better to have another short unique id...
  K <- stan_data$K
  J <- stan_data$J
  switch(get_file(model),
    "dawid_skene" = dawid_skene_inits(K, J),
    "class_conditional_dawid_skene" = class_conditional_dawid_skene_inits(K, J),
    "hierarchical_dawid_skene" = "random",
    stop("Unsupported model type", call. = FALSE))
}

#' Creates initialization points for the dawid and skene model.
#'
#' @param K number of categories
#' @param J number of raters
#'
#' @return Initialization points in the format required by Stan.
#'
#' @noRd
#'
dawid_skene_inits <- function(K, J) {
  pi_init <- rep(1/K, K)
  theta_init <- array(0.2 / (K - 1), c(J, K, K))
  for (j in 1:J) {
      diag(theta_init[j, ,]) <- 0.8
  }
  function(n) list(theta = theta_init, pi = pi_init)
}

#' Creates initialization points for the class conditional model
#'
#' @param K number of categories
#' @param J number of raters
#'
#' @return initialization points in the format required by stan
#'
#' @noRd
#'
class_conditional_dawid_skene_inits <- function(K, J) {
  pi_init <- rep(1/K, K)
  theta_init <- matrix(0.8, nrow = J, ncol = K)
  function(n) list(theta = theta_init, pi = pi_init)
}

#' Helper to check if the prior parameters and data have consistent dimensions
#'
#' @param stan_data data in stan format
#' @param model the passed model
#'
#' @return The fully realised prior parameters
#'
#' @noRd
#'
check_K <- function(stan_data, model) {
  # NB: this does not/cannot tell the user which of the pars is inconsistent
  # but we can return a vector (with NULLs and parse cleverly)
  if (!is.null(model$K) && (stan_data$K != model$K)) {
    stop("The number of categories is inconsistent between data and the prior",
         " parameters", call. = FALSE)
  }
}

#' Helper get the correct stan file to run for model/data combination
#'
#' @param data_format A string containing the specification of the data format
#' @param model a rater_model object
#'
#' @return the name (no .stan) of the stan file that should be run
#'
#' @noRd
#'
get_stan_file <- function(data_format, model) {

  file <- get_file(model)
  # If the data is grouped override this. We are assuming we have a
  # valid model/format pair.
  if (data_format == "grouped") {
    file <- "grouped_data"
  }
  file
}

#' Helper to check if the passed model is valid.
#'
#' This function will return a rater_model object if one can be constructed
#' from the input.
#'
#' @param model The `model` argument passed to [`rater()`]
#'
#' @return A rater model object.
#'
#' @noRd
#'
validate_model <- function(model) {

  if (is.character(model)) {
    model <- switch(model,
      "dawid_skene" = dawid_skene(),
      "hier_dawid_skene" = hier_dawid_skene(),
      "class_conditional_dawid_skene" = class_conditional_dawid_skene(),
      stop("Invalid model string specification.", .call = FALSE))
  }

  if (!is.rater_model(model)) {
    stop("`model` must be a rater model object.", call. = FALSE)
  }

  model
}

#' Helper to check if passed data and model are valid and consistent
#'
#' @param data The `data` argument passed to [rater()]
#' @param model The `model` argument passed to [rater()]
#' @param data_format The `data_format` argument passed to [rater()]
#'
#' @return Validated data. This will always be a data.frame with the
#'   appropriate column names for the column names.
#'
#' @noRd
#'
validate_input <- function(data, model, data_format) {

  if (data_format == "grouped" & !is.dawid_skene(model)) {
    stop("Grouped data can only be used with the Dawid and Skene model",
         call. = FALSE)
  }

  validate_data(data, data_format)
}

#' Validate the data passed into the rater function
#'
#' @param data The `data` argument passed to [rater()]
#' @param data_format The `data_format` argument passed to [rater()]
#'
#' @return Validated data. This will always be a data.frame with the
#'   appropriate column names for the column names.
#'
#' @noRd
#'
validate_data <- function(data, data_format) {

  # TODO: The error message in this function should refer to a vignette
  # or vignette section about data format.

  # Note that this test for allow things like tibbles to be accepted. We
  # next use as.data.frame to standardise the input.
  if (!inherits(data, "data.frame") &&  !inherits(data, "matrix")) {
    stop("`data` must be a data.frame or matrix.", call = FALSE)
  }
  data <- as.data.frame(data)

  # FIXME We should accept non-numeric data (GitHub issue: #81) but for
  # now we explicitly check that is all columns contain numeric values.
  if (!all(vapply(data, is.numeric, FUN.VALUE = logical(1)))) {
    stop("All columns in `data` must contain only numeric values.",
         call. = FALSE)
  }

  if (data_format == "long") {

    if (!ncol(data) == 3L) {
      stop("Long format `data` must have exactly three columns.", call. = FALSE)
    }

    if (!(all(c("rater", "item", "rating") %in% colnames(data)))) {
      stop("Long `data` must have three columns with names: `rater`, `item`,",
           "and `rating`.", call. = FALSE)
    }

    # The following are errors about 0 elements. We try to show these errors
    # all at once to prevent undue frustration.
    error_messages <- character(0)
    if (any(data[[1]] == 0)) {
       error_messages <- c(error_messages, paste0(
        "Some item indexes are 0. All indexes must be in 1:I",
        " where I is the number of items."))
    }

    if (any(data[[2]] == 0)) {
      error_messages <- c(error_messages, paste0(
        "Some rater indexes are 0. All indexes must be in 1:J",
        " where J is the number of raters."))
    }

    if (any(data[[3]] == 0)) {
      error_messages <- c(error_messages, paste0(
        "Some ratings are 0. All ratings must be in 1:K",
        " where K is the number of classes."))
    }

    if (length(error_messages) > 0) {

      if (length(error_messages) == 1) {
        stop(error_messages[[1]], call. = FALSE)
      } else if (length(error_messages) > 1) {
        stop("\n", paste0("* ", error_messages, collapse = "\n"), call. = FALSE)
      }

    }

  } else if (data_format == "grouped") {

    last_col_name <- colnames(data)[[ncol(data)]]
    if (!last_col_name == "n") {
      stop("The last column must be named `n`.", call. = FALSE)
    }

    error_messages <- character(0)
    tally <- data$n
    if (any(tally == 0)) {
      error_messages <- c(error_messages,
        "All elements of the column `n` must be > 0."
      )
    }

    rest <- data[1:(ncol(data) - 1)]
    if (any(rest == 0)) {
      error_messages <- c(error_messages, paste0(
        "Some ratings are 0. All ratings must be in 1:K",
        " where K is the number of classes."))
    }

    if (length(error_messages) == 1) {
        stop(error_messages[[1]], call. = FALSE)
    }

    if (length(error_messages) == 2) {
        stop("\n", paste0("* ", error_messages, collapse = "\n"), call. = FALSE)
    }

  }

  data
}

