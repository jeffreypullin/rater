#' Draw from the posterior predictive distribution
#'
#' @param object A `rater_fit` object.
#' @param new_data New data for the model to be fit to. The must be in the form
#'   used in `rater()` except without the 'rating' column.
#' @param seed An optional random seed to use.
#' @param ... Other arguments.
#'
#' @return The passed `new_data` augmented with a column 'z' containing the
#'   latent class of each item and 'rating' containing the simulated rating.
#'
#' @details The number of raters implied by the entries in the rater column
#'   must match the number of raters in the fitted model.
#'
#'   Due to technical issues drawing from the posterior predictive distribution
#'   of the hierarchical Dawid-Skene model is currently not supported.
#'
#' @examples
#'
#' \donttest{
#'
#' fit <- rater(anesthesia, "dawid_skene", verbose = FALSE)
#' new_data <- data.frame(item = rep(1:2, each = 5), rater = rep(1:5, 2))
#'
#' predictions <- posterior_predict(fit, new_data)
#' predictions
#'
#' }
#'
#' @aliases posterior_predict
#' @method posterior_predict rater_fit
#' @importFrom rstantools posterior_predict
#' @export
#' @export posterior_predict
#'
posterior_predict.rater_fit <- function(object, new_data, seed = NULL, ...) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  fit <- object
  if (inherits(get_model(fit), "hier_dawid_skene")) {
    stop("The posterior_predict is not currently implmented for the ",
         "\n Hierarchical Dawid-Skene model.", call. = FALSE)
  }

  new_data <- as.data.frame(new_data)
  col_names <- colnames(new_data)
  if (ncol(new_data) != 2 || !all(c("item", "rater") %in% col_names)) {
    stop("`new_data` must have two columns 'item' and 'rater'", call. = FALSE)
  }

  if (max(new_data$rater) != fit$stan_data$J) {
    stop("The number of raters in the fitted and new data must match",
         call. = FALSE)
  }

  pi <- point_estimate(fit, pars = "pi")$pi
  theta <- point_estimate(fit, pars = "theta")$theta
  n <- nrow(new_data)
  K <- length(pi)
  I <- max(new_data$item)

  item_z <- sample(1:K, size = I, replace = TRUE, prob = pi)
  z <- item_z[new_data$item]

  ratings <- numeric(n)
  for (i in seq_len(n)) {
    j <- new_data$rater[[i]]
    ratings[[i]] <- sample(1:K, 1, prob = theta[j, z[[i]], ])
  }

  pred <- cbind(new_data, z = z, ratings = ratings)
  pred
}
