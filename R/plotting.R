#' Plot the popuation prevalence estimates
#'
#' @param fit rateR fit object
#' @return Plot of the population prevelance esitmates extracted in fits
#'
#' @export
#' @import ggplot2
plot_prevalance <- function(fit) {

  validate_fit(fit)

  pi_samps <- rstan::extract(fit$draws)$pi

  pi_mean <- apply(pi_samps, 2, mean)
  pi_sd   <- apply(pi_samps, 2, sd)

  plot_data <- data.frame(cat = 1:ncol(pi_samps),
                          prob = pi_mean,
                          sd = pi_sd)

  plot <- ggplot(plot_data, aes(x = cat, y = prob)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(prob, 2)), vjust = -6) +
    geom_errorbar(aes(ymin = prob - sd, ymax = prob + sd),
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
#' @param fit rateR fit object
#' @return Plot of the rate accuracy estimates
#'
#' @export
#' @import ggplot2
plot_raters <- function(fit) {

  validate_fit(fit)

  fit_ss <- rstan::extract(fit$draws)
  theta_samps <- fit_ss$theta

  J <- dim(theta_samps)[2]
  K <- dim(theta_samps)[3]

  raters <- list()
  for(j in 1:J){
    rate_mat <- matrix(0, nrow = K, ncol = K)
    for (n in 1:K){
      for (m in 1:K){
        rate_mat[n,m] <- mean(theta_samps[,j,n,m])
      }
    }
    raters[[j]] <- rate_mat
  }

  plot_data <- data.frame(x = factor(rep(rep(1:K, each = K), J), level = 1:K),
                      y = factor(rep(rep(1:K, K), J), level = K:1),
                      rater = rep(1:J, each = K^2),
                      value = unlist(lapply(raters, function(x) as.vector(x))))

  plot <- ggplot(plot_data, aes(x, y)) +
   geom_tile(aes(fill = value), col = "black") +
   geom_text(aes(label = round(value, 2))) +
   facet_wrap(~ rater) +
   # TODO add way to change defaults
   scale_fill_gradient(low = "gray90", high = "orangered") +
   labs(y = "True label",
        x = "Assigned label") +
   guides(fill = FALSE) +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border     = element_blank()) +
   NULL

  plot
}

#' Plot the latent class estimates
#'
#' @param fit rateR fit object
#' @return Plot of the rate accuracy estimates
#'
#' @export
#' @import ggplot2
plot_latent_class <- function(fit){

  p_z <- extract_latent_class(fit)

  plot_data <- data.frame(x = factor(rep(1:K, each = I), levels = 1:K),
                          y = factor(rep(1:I, K), levels = I:1),
                          prob = as.vector(p_z))

  plot <- ggplot(plot_data, aes(x, y)) +
    geom_tile(aes(fill = prob), colour = "black") +
    geom_text(aes(label = round(prob, 2))) +
    labs(x = "Latent Class",
         y = "Item") +
    scale_fill_gradient(low = "gray90", high = "orangered") +
    guides(fill = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank()) +
    NULL

  plot

}
