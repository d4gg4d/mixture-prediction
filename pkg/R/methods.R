#' For interfacing feature extraction
#'
#' @return structure that ready to be used in predict(...)
#' 
fit <- function(x, ...) {
  UseMethod("fit")
}

#' Interfacing failure case in feature extraction
#'
#' @return values that represent failed fitting
#' 
failure <- function(x, ...) {
  UseMethod("failure")
}

#' Interfacing name retrieval for feature
#'
#' @return values that represent extracted features
#' 
name <- function(x, ...) {
  UseMethod("name")
}

#'
#' @param x class to contain function parameters
#'
#' @param predictions data.frame of predictions
#'
#' @param validations data.frame of real values, one2one match for
#' prediction values
#' 
#' @return vector of scoring for predictions based on validation
#' data.
#' 
validate <- function(x, predictions, validations, ...) {
  UseMethod("validate")
}
