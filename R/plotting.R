#' Plot the prevalence estimates
#'
#' @param fit A rater fit object.
#' @param prob A single probability. The size of the credible interval
#'   returned, if the fit is an `mcmc_fit`. Silently ignored if a the fit is
#'   an `optim_fit` object. By default 0.9.
#' @return A plot of the prevalence estimates extracted from the fit. If the
#'   fit is a `mcmc_fit` this will include credible intervals, if it is an
#'   `optim_fit` it will not.
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text coord_cartesian labs
#'     theme_bw
#' @importFrom rlang .data
#'
#' @noRd
#'
plot_pi <- function(fit, prob = 0.9) {
  UseMethod("plot_pi")
}

#' @rdname plot_pi
#' @noRd
plot_pi.mcmc_fit <- function(fit, prob = 0.9) {
  pi <- point_estimate(fit, pars = "pi")[[1]]

  # Here we know that the fit is an `mcmc_fit` so this will work.
  pi_cred_int <- posterior_interval(fit, prob = prob, pars = "pi")

  plot_data <- data.frame(
    cat = factor(paste0("Class ", 1:length(pi)),
                 levels = paste0("Class ", length(pi):1)),
    pi = pi,
    pi_lower = pi_cred_int[, 1],
    pi_upper = pi_cred_int[, 2]
  )

  percent <- paste0(prob * 100, "%")
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$cat, y = .data$pi)) +
    ggplot2::geom_point(size = 2, colour = "steelblue") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$pi_lower,
                                        ymax = .data$pi_upper),
                           width = 0.15, colour = "steelblue") +
    ggplot2::coord_flip(ylim = c(0, 1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(x = "",
                  y = "Prevalence probability",
                  caption = paste0(percent, " credible intervals")) +
    ggplot2::theme_bw()

  plot
}

#' @rdname plot_pi
#' @noRd
plot_pi.optim_fit <- function(fit, prob = 0.9) {
   pi <- point_estimate(fit, pars = "pi")[[1]]

  plot_data <- data.frame(
    cat = factor(paste0("Class ", 1:length(pi)),
                 levels = paste0("Class ", length(pi):1)),
    pi = pi
  )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$cat, y = .data$pi)) +
    ggplot2::geom_point(size = 2, colour = "steelblue") +
    ggplot2::coord_flip(ylim = c(0, 1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(x = "",
                  y = "Prevalence probability") +
    ggplot2::theme_bw()

  plot
}

#' Plot the rater accuracy estimates
#'
#' @param fit rater fit object
#' @param which which raters to plot
#'
#' @return Plot of the rate accuracy estimates
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text facet_wrap labs guides
#'      scale_fill_gradient theme_bw theme element_rect element_blank
#' @importFrom rlang .data
#'
#' @noRd
#'
plot_theta <- function(fit, which = NULL) {
  theta <- theta_point_estimate(fit, which = which)

  # theta will always have dim[[2]] and it will always be == K
  K <- dim(theta)[[2]]

  # would be great if we could treat in arrays and matrices the 'same'
  if (length(dim(theta)) > 2) {
    J <- dim(theta)[[1]]
    value <- unlist(lapply(1:J, function(x) as.vector(theta[x, , ])))
  } else {
    J <- 1
    value <- as.vector(theta)
  }
  which <- if (is.null(which)) 1:J else which

  plot_data <- data.frame(
                  x = factor(rep(rep(1:K, each = K), J), levels = 1:K),
                  y = factor(rep(rep(1:K, K), J), levels = K:1),
                  rater = rep(which, each = K^2),
                  value = value,
                  round_value = round(value, 2))
  rownames(plot_data) <- NULL

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
   ggplot2::geom_tile(ggplot2::aes(fill = .data$value), col = "black") +
   ggplot2::geom_text(ggplot2::aes(label = .data$round_value)) +
   ggplot2::facet_wrap(~ rater) +
   # TODO add way to change defaults
   ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
   ggplot2::labs(y = "True label",
                 x = "Assigned label") +
   ggplot2::guides(fill = FALSE) +
   ggplot2::theme_bw() +
   ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                  panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank(),
                  panel.border     = ggplot2::element_blank()) +
   NULL

  plot
}

#' Plot the latent class estimates of a rater fit.
#'
#' @param fit A `rater_fit` object.
#' @param ... Other arguments
#'
#' @return Plot of the rate accuracy estimates
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text labs theme_bw theme
#'     scale_fill_gradient guides element_blank
#' @importFrom rlang .data
#'
#' @noRd
#'
plot_class_probabilities <- function(fit, item_index = NULL) {

  x <- class_probabilities(fit)
  I <- nrow(x)
  K <- ncol(x)

  if (is.null(item_index)) {
    plot_data <- data.frame(
      x = factor(rep(1:K, each = I), levels = 1:K),
      y = factor(rep(1:I, K), levels = I:1),
      prob = as.vector(x),
      round_prob = round(as.vector(x), 2)
    )
  } else {

    if (!is.numeric(item_index) || !(item_index %in% 1:I)) {
      stop("`item_index` must be a numeric vector with elements in 1:I",
           call. = FALSE)
    }

    x <- x[item_index, ]
    plot_data <- data.frame(
      x = factor(rep(1:K, each = length(item_index)), levels = 1:K),
      y = factor(rep(item_index, K), levels = rev(item_index)),
      prob = as.vector(x),
      round_prob = round(as.vector(x), 2)
    )
  }

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$prob), colour = "black") +
    ggplot2::geom_text(ggplot2::aes(label = .data$round_prob)) +
    ggplot2::labs(x = "Latent Class",
                  y = "Item") +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::guides(fill = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border     = ggplot2::element_blank()) +
    NULL

  plot
}
