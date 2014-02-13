featurelm <- function(formula, name) {
  return(structure(list(formula=formula, name=name), class="featurelm"))
}

fit.featurelm <- function(x, data, ...) {
  return(lm(x$formula, data))
}

failure.featurelm <- function(x, ...) {
  return(NA)
}

name.featurelm <- function(x, ...) {
  return(x$name)
}

scoreFn <- function(parameters, name, fn) {
  structure(list(parameters=parameters, id=name, fn=fn), class="scoreFn")
}

validate.scoreFn <- function(x, predictions, validations) {
  return(x$fn(predictions, validations, x$parameters))
}

name.scoreFn <- function(x, ...) {
  return(x$id)
}
