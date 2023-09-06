#' Anaesthetist ratings for patient suitability for surgery
#'
#' The data consist of ratings, on a 4-point scale, made by five anaesthetists
#' of patients' pre-operative health. The ratings were based on the
#' anaesthetists assessments of a standard form completed for all of the
#' patients. There are 45 patients (items) and five anaesthetists (raters) in
#' total. The first anaesthetist assessed the forms a total of three times,
#' spaced several weeks apart. The other anaesthetists each assessed the forms
#' once. The data is in 'long' format.
#'
#' @format A `data.frame` with 315 rows and 3 columns:
#' \describe{
#'   \item{item}{The item index - which item is being rated}
#'   \item{rater}{The rater index - which rater is doing the rating}
#'   \item{rating}{The rating given}
#' }
#'
#' @references
#' Dawid, A. P., and A. M. Skene. "Maximum Likelihood Estimation of Observer
#' Error-Rates Using the EM Algorithm." Applied Statistics 28, no. 1 (1979): 20.
#'
"anesthesia"

#' Dentist ratings of whether caries are healthy or not based on X-rays
#'
#' It consists of binary ratings, made by 5 dentists, of whether a given tooth
#' was healthy (sound) or had caries, also known as cavities. The ratings were
#' performed using X-ray only, which was thought to be more error-prone than
#' visual/tactile assessment of each tooth. In total 3,689 ratings were made.
#' This data is in 'grouped' format. Each row is one of the 'pattern' with
#' the final columns being a tally of how many times that pattern occurs in
#' the dataset.
#'
#' @format A `data.frame` with 6 columns and 32 rows.
#' \describe{
#'   \item{rater_1}{The rating of the dentist 1}
#'   \item{rater_2}{The rating of the dentist 2}
#'   \item{rater_3}{The rating of the dentist 3}
#'   \item{rater_4}{The rating of the dentist 4}
#'   \item{rater_5}{The rating of the dentist 5}
#'   \item{n}{The number of times the rating pattern appears in the dataset}
#' }
#'
#' @references
#' Espeland, Mark A., and Stanley L. Handelman. “Using Latent Class Models to
#' Characterize and Assess Relative Error in Discrete Measurements.”
#' Biometrics 45, no. 2 (1989): 587–99.
#'
"caries"
