if (TRUE) {
  library(mixturePrediction)
  detach("package:mixturePrediction", unload=TRUE)
  library(mixturePrediction)
  library(caret) #TODO remove usage/replace predict.list, just for testing purposes only
  library(RUnit)
}

test.create.proper.history <- function() {
  test <- data.frame(time=as.integer(1:100),
                     a=sample(100),
                     b=sample(100))
  test.validation <- data.frame(time=as.integer(1:10*5),
                     a=sample(10),
                     b=sample(10))
  score.fn <- scoreFn(list(p1=1), "foo", function(predictions, target, params) {
    return(params$p1*(predictions$a - target$a)^2 + (predictions$b - target$b)^2)
  })
  test.models <- list(
    model1=list(a=lm(a ~ time, test[1:30,]), b=lm(b ~ time, test[1:30,])),
    model2=list(a=lm(a ~ time, test[1:30,]), b=lm(b ~ time, test[1:30,])))
  values <- mixturePrediction:::PredictionsAndValidations(test.models, score.fn, test, test.validation)
  browser()
  checkEquals(length(colnames(values)), 5)
  checkEquals(nrow(values), 20)
  checkTrue(all(values$validations > 0))
}
