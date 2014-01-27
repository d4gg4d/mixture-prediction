if (TRUE) {
  detach("package:mixturePrediction", unload=TRUE)
  library(mixturePrediction)
}
  
test.FeatureExtraction <- function() {
  test.frame <- data.frame(time=1:100, a=sample(100), b=sample(100))
  test.features <- create.lm.features(c(a ~ time, b ~ time), c("f.a","f.b"))
  values <- FeatureExtraction(test.features, test.frame, 5, 10, interval=1)
  checkEquals(nrow(values), 85)
  checkEquals(min(values$time), 16)
  checkEquals(max(values$time), 100)
}

test.FeatureExtraction.failure.produces.NA <- function() {
  test.frame <- data.frame(time=1:100, a=c(rep(NA, 50), sample(50)), b=sample(100))
  test.features <- create.lm.features(c(a ~ time, b ~ time), c("f.a","f.b"))
  values <- FeatureExtraction(test.features, test.frame, 5, 10, interval=1)
  checkEquals(nrow(values), 85)
  checkEquals(min(values$time), 16)
  checkEquals(max(values$time), 100)
  browser()
  checkEquals(which(is.na(values$f.a)), 1:40)
  checkTrue(all(!is.na(values$f.b)))
}
