#' Anesthetist ratings for patient suitability for surgery
#'
#' A dataset containing anesthetist ratings of patient readiness for surgery for
#' 5 anesthetists on 45 patients with 4 categories in 'long' format. Each row
#' represents one unique rating or annotation
#'
#' @format A matrix with 315 rows and 3 columns:
#' \describe{
#'   \item{item}{item index, which item is being rated}
#'   \item{rater}{rater index, which rater is doing the rater}
#'   \item{rating}{what rating was given}
#' }
"anesthesia"

#' Caries ratings of whether caries are 'sound' or not (c...)
#'
#' A dataset containing anesthetist ratings of patient readiness for surgery for
#' 5 anesthetists on 45 patients with 4 categories in 'long' format. Each row
#' represents one unique rating or annotation
#'
#' @format A matrix
#' \describe{
#'   \item{ii}{item index, which item is being rated}
#'   \item{jj}{rater index, which rater is doing the rater}
#'   \item{y}{what rating was given}
#' }
"caries"
