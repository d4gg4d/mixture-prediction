#' Predicts values for test data. Uses list of trained model pairs to
#' predict values in testing data and makes selection based on given
#' mixture.
#'
#' @param trained.models list of trained model pairs that predict
#' latitude,longitude values based feature.data, e.g. trained.models
#' predictor variables must be sub set of feature.data variables
#'
#' @param mixture method that makes final selection from trained.model predictions
#'
#' @param score.function scoring function that calculates validation
#' value of a prediction from trained.models
#'
#' @param feature.data data.frame rows matched in time against
#' test.data and used to predict values with trained.models
#'
#' @param test.data validation data which contains real values what
#' predictor tries to predict.
#'
#' @param t.dist timewise distance of predicted vector and history
#' window that which it is going to be used.
#'
#' @param hist.length length in time where to gather history
#' information.
#'
#' @return dataframe of predicted values
#'
MixturePredict <- function(trained.models, mixture, score.function, feature.data, test.data, hist.length) {
  histories <- PredictionsAndValidations(trained.models, score.function, feature.data, test.data)
  validation.columns <- llply(list(score.function), function(el) { return(name(el)) })
  final.output <- MixtureSelection(mixture, histories, hist.length, validation.columns)
  stopifnot(!duplicated(final.output$time))
  return(final.output)
}

#' @param trained.models named list of trained models that predict
#' values based feature.data, e.g. trained.models predictor variables
#' must be sub set of feature.data variables
#'
#' @param score.function scoring function that calculates validation
#' value of a prediction from trained.models
#'
#' @param feature.data data.frame rows matched in time against
#' test.data and used to predict values with trained.models
#'
#' @param test.data validation data which contains real values what
#' predictor tries to predict.
#'
#' @return data.frame(time, modelid, prediction1, prediction2, ..., history1, history2, ...)
#'
PredictionsAndValidations <- function(trained.models, score.function, feature.data, test.data, do.parallel=FALSE) {
  stopifnot(!is.null(names(trained.models)))
  targets <- VectorsMatchingInTime(feature.data, test.data$time)
  values <- PredictInternal(trained.models, targets, do.parallel=do.parallel)
  values.with.validations <- ValidatePredictions(values, targets, score.function)
  return(values.with.validations)
}

#' takes model pair and predicts values for each target.data row
#'
#' @param trained.models list of models that contain trained model
#'
#' @param target.data extracted feature vectors to which predictions are made against
#'
#' @return data.frame of (modelid, time, prediction1, prediction2)
#'
PredictInternal <- function(trained.models, target.data, do.parallel=FALSE) {
  return(ldply(trained.models, function(model) {
    return(data.frame(time=target.data$time, predict(model, target.data)))
  }, .parallel=do.parallel))
}

#' takes predicted data.frame and calculates validation value(s) based on scoring function
#'
#' @param predictions (latitude,longitude) pairs which have been predicted (data.frame format)
#'
#' @param target.data (latitude,longitude) pairs which represent real values (data.frame format)
#'
#' @param score.fn given function that takes in predicted vector and expected vector and returns scalar value for it
#'
#' @return predictions data with column of value of score.fn applied to predicted and target.data
#' 
#' TODO here be logic over multiple scoring functions 
ValidatePredictions <- function(predictions, target.data, score.fn) {
  return(ddply(predictions, ~ .id, function(model.predictions) {
    predictions.over.model <- validate(score.fn, model.predictions, target.data)
    model.predictions$pred <- predictions.over.model
    colnames(model.predictions)[which(names(model.predictions) == "pred")] <- name(score.fn)
    return(model.predictions)
  }))
}

#' Method for doing final decision about prediction result. It can use
#' history information which is encapulated to datacube which is its
#' input.
#' 
#' @param mixture process that decides from inputted data which vector
#' should be the final output. TODO should this be maybe list of mixtures...
#'
#' @param history data.frame where each row represents output of one
#' model pair
#'
#' @param t.dist timewise distance of predicted vector and history
#' window that which it is going to be used.
#'
#' @param hist.length length in time where to gather history
#' information.
#' 
#' @return data.frame of final output for each target data
#' 
MixtureSelection <- function(mixtures, history, hist.length, validation.columns) {
  targets <- getBetween(history, min(history$time) + 1 + hist.length, max(history$time))
  times <- targets[with(targets, !duplicated(time)), ]$time
  values <- ldply(times, function(prediction.time) {
    prediction.candidates <- history[history$time == prediction.time, ]
    history.window <- subset(HistoryWindow(history, prediction.time - 1, hist.length), select=unlist(c(".id", "time", validation.columns)))
    return(ldply(mixtures, function(mixture) {
      final.model.id <- select(mixture, history.window)
      return(prediction.candidates[prediction.candidates$.id == final.model.id, ])
    }, .id="mixture"))
  })
  return(values)
}
