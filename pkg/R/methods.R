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

#'
#' @param mixture class that implements method mixture or a mixture function
#'
#' @param history data.frame consisting validation values from some
#' time range. history is a form of: data.frame(.id, time, validation1, validation2, ...)
#'
#' @param t.dist temporal distance of to be selected value and history windows last value.
#'
#' @return id of selected model.
#' 
select <- function(mixture, history, t.dist, ...) {
  if (class(mixture) == "function") {
    mixture(history, t.dist)
  } else {
    UseMethod("mixture")
  }
}
