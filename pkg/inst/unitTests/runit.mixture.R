if (TRUE) {
  detach("package:mixturePrediction", unload=TRUE)
  library(mixturePrediction)
}

test.MixtureSelection.gives.proper.data.frame.to.mixtureMethod <- function() {
  test <- data.frame(time=as.integer(2:101/2),
                     .id=rep(c("a1","b2"), 50),
                     a=sample(100),
                     b=sample(100))
  checkEquals(nrow(test), 100)
  mixture.assert <- function(hist, t.dist) {
    checkEquals(nrow(hist), 22)
    checkTrue(!all(duplicated(hist)))
    checkTrue(max(hist$time) - min(hist$time) == 10)
    return("b2")
  }
  values <- MixtureSelection(list(mixture.assert), test, 10, c("a","b"))
  checkEquals(nrow(values), 39)
}
