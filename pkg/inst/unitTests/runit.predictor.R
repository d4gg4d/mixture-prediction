if (TRUE) {
  detach("package:mixturePrediction", unload=TRUE)
  library(mixturePrediction)
}

test.create.proper.history <- function() {
  test <- data.frame(time=as.integer(1:100),
                     a=sample(100),
                     b=sample(100))
  test.validation <- data.frame(time=as.integer(1:10*5),
                     latitude=sample(10),
                     longitude=sample(10))
  score.fn <- scoreFn(list(p1=1), "foo", function(predictions, target, params) {
    return(params$p1*(predictions$latitude - target$latitude)^2 + (predictions$longitude - target$longitude)^2)
  })
  test.models <- ModelPairing(c("model1"), lm.create.models(c(a ~ time), test[1:30,]),
                              lm.create.models(c(b ~ time), test[1:30,]))
  values <- mixturePrediction:::PredictionsAndValidations(test.models, score.fn, test, test.validation)
  browser()
  checkEquals(nrow(values), 10)
  checkTrue(all(values$validations > 0))
}
