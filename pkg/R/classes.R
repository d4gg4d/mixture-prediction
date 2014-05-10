scoreFn <- function(parameters, name, fn) {
  structure(list(parameters=parameters, id=name, fn=fn), class="scoreFn")
}

validate.scoreFn <- function(x, predictions, validations) {
  return(x$fn(predictions, validations, x$parameters))
}

name.scoreFn <- function(x, ...) {
  return(x$id)
}
