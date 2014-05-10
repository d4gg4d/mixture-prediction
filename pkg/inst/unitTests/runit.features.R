if (TRUE) {
  detach("package:mixturePrediction", unload=TRUE)
  library(mixturePrediction)
  library(RUnit)
}

featurelm <- function(formula, name, time.range=0) {
  return(structure(
    list(formula=formula, name=name, time.range=time.range),
    class=c("featurelm", "feature")))
}

fit.featurelm <- function(x, data, ...) {
  fit <- lm(x$formula, data[LatestFromRange(data, x$time.range), ])
  names(fit) <- x$name
  return(fit)
}

failure.featurelm <- function(x, ...) {
  return(NA)
}

create.test.features <- function(variables, names) {
  return(c(f.a=featurelm(a ~ time, "f.a1")),c(f.b=featurelm(b ~ time, "f.b1")))
}

test.FeatureExtraction <- function() {
  test.frame <- data.frame(time=1:100, a=sample(100), b=sample(100))
  test.features <- create.test.features()
  values <- FeatureExtraction(test.features, test.frame, 5, 10, interval=1)
  checkEquals(nrow(values), 85)
  checkEquals(min(values$time), 16)
  checkEquals(max(values$time), 100)
}

test.FeatureExtraction.failure.produces.NA <- function() {
  test.frame <- data.frame(time=1:100, a=c(rep(NA, 50), sample(50)), b=sample(100))
  test.features <- create.test.features()
  values <- FeatureExtraction(test.features, test.frame, 5, 10, interval=1)
  checkEquals(nrow(values), 85)
  checkEquals(min(values$time), 16)
  checkEquals(max(values$time), 100)
  checkEquals(which(is.na(values$f.a)), 1:40)
  checkTrue(all(!is.na(values$f.b)))
}
