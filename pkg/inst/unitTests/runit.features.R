if (TRUE) {
  detach("package:mixturePrediction", unload=TRUE)
  library(mixturePrediction)
  library(RUnit)
}

test.FeatureExtraction <- function() {
  test.frame <- data.frame(time=1:100, a=sample(100), b=sample(100))
  test.features <- mixturePrediction:::create.test.features()
  values <- FeatureExtraction(test.features, test.frame, t.dist=5, t.window.length=10, interval=2)
  ##:ess-bp-start::browser@nil:##
browser()##:ess-bp-end:##
  checkEquals(nrow(values), 43)
  checkEquals(min(values$time), 16)
  checkEquals(max(values$time), 100)
}

test.FeatureExtraction.failure.produces.NA <- function() {
  test.frame <- data.frame(time=1:100, a=c(rep(NA, 50), sample(50)), b=sample(100))
  test.features <- mixturePrediction:::create.test.features()
  values <- FeatureExtraction(test.features, test.frame, 5, 10, interval=2)
  checkEquals(nrow(values), 43)
  checkEquals(min(values$time), 16)
  checkEquals(max(values$time), 100)
  ##:ess-bp-start::browser@nil:##
browser()##:ess-bp-end:##
  checkEquals(which(is.na(values$f.a)), 1:20)
  checkTrue(all(!is.na(values$f.b)))
}
