#' Plot the popuation prevalence estimates
#'
#' @param fit rater fit object
#' @return Plot of the population prevelance esitmates extracted in fits
#'
#' @export
#' @import ggplot2
plot_prevalance <- function(fit) {

  plot_data <- extract_prevalance(fit)
  plot_data$cat <- plot_data$category

  plot <- ggplot(plot_data, aes(x = cat, y = "prob")) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round("prob", 2)), vjust = -6) +
    geom_errorbar(aes(ymin = "prob" - "sd", ymax = "prob" + "sd"),
                      width = 0.2,
                      position = position_dodge(0.9)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Category",
         y = "Pop. prevelance prob.") +
    theme_bw()

  plot

}

#' Plot the rater accuracy estimates
#'
#' @param fit rater fit object
#' @return Plot of the rate accuracy estimates
#'
#' @export
#' @import ggplot2
plot_raters <- function(fit, which = NULL) {

  raters <- extract_raters(fit = fit, which = which)

  if (is.list(raters)) {
    # list of matrices - theta 3D
    K <- nrow(raters[[1]])
    J <- length(raters)
  } else {
    # one matrix - theta 2D
    K <- nrow(raters)
    J <- 1
  }

   if (is.null(which)) {
    which <- 1:J # code duplication: also in `extract_raters`
  }

  value <- unlist(lapply(raters, function(x) as.vector(x)))

  plot_data <- data.frame(
                  x = factor(rep(rep(1:K, each = K), J), levels = 1:K),
                  y = factor(rep(rep(1:K, K), J), levels = K:1),
                  rater = rep(which, each = K^2),
                  value = value,
                  round_value = round(value, 2))

  # clean up the rownames
  rownames(plot_data) <- NULL

  plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
   geom_tile(aes_string(fill = "value"), col = "black") +
   geom_text(aes_string(label = "round_value")) +
   facet_wrap(~ rater) +
   # TODO add way to change defaults
   scale_fill_gradient(low = "white", high = "steelblue") +
   labs(y = "True label",
        x = "Assigned label") +
   guides(fill = FALSE) +
   theme_bw() +
   theme(strip.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border     = element_blank()) +
   NULL

  plot
}

#' Plot the latent class estimates
#'
#' @param fit rater fit object
#' @return Plot of the rate accuracy estimates
#'
#' @export
#' @import ggplot2
plot_latent_class <- function(fit){

  p_z <- extract_latent_class(fit)

  I <- nrow(p_z)
  K <- ncol(p_z)

  plot_data <- data.frame(x = factor(rep(1:K, each = I), levels = 1:K),
                          y = factor(rep(1:I, K), levels = I:1),
                          prob = as.vector(p_z),
                          round_prob = round(as.vector(p_z), 2))

  plot <- ggplot(plot_data, aes_string(x = "x", y = "y")) +
    geom_tile(aes_string(fill = "prob"), colour = "black") +
    geom_text(aes_string(label = "round_prob")) +
    labs(x = "Latent Class",
         y = "Item") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    guides(fill = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border     = element_blank()) +
    NULL

  plot

}
