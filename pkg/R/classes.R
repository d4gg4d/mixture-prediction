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
