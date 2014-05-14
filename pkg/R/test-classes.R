featureTest <- function(formula, name, time.range=0) {
  return(structure(
    list(formula=formula, name=name, time.range=time.range),
    class=c("featureTest", "feature")))
}

fit.featureTest <- function(x, data, ...) {
  fit <- list(lm(x$formula, data))
  names(fit) <- x$name
  return(fit)
}

failure.featureTest <- function(x, ...) {
  return(NA)
}

create.test.features <- function(variables, names) {
  return(list(f.a=featureTest(a ~ time, "f.a1"),
              f.b=featureTest(b ~ time, "f.b1")))
}
