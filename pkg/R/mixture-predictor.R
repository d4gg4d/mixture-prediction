#' main method. Uses features to construct feature extraction data
#' rows for times corresponding to newdata rows times. Then does
#' prediction of latitude,longitude pairs with trained models based on
#' feature extration data (in which models are trained againts). Then
#' calculates scoring of each prediction and uses mixture to decide
#' which prediction is final output of the predictor.
#'
#' @param features list of feature extraction methods
#'
#' @param models list of trained model pairs that predict latitude,longitude values from feature extraction data
#'
#' @param newdata validation data which contains real values what predictor tries to predict
#'
#' @param score.fn scoring function that calculates validation value of a prediction
#'
#' @param mixture method that makes final selection based on known history (earlier predictions and their scoring value)
#'
#' @param t.dist time distance to last history window row which is used to calculate feature extraction values by fitting feature to it fit(last(history)$time + t.dist)
#' 
#' @param t.window.length size of the data frame in time units to which feature extractio is made
#'
#' @return dataframe of predicted values
#' 
mixture.predict <- function(features, trained.models, newdata, score.fn, mixture, t.dist, t.window.length, prediction.interval=360) {

  #' takes model pair and predicts values for each target.data row
  #'
  #' @param model.pair a list of models that contain trained model for latitude and longitude
  #'
  #' @param target.data extracted feature vectors to which predictions are made against
  #'
  #' @return prediction dataframe (rows of latitude,longitude pairs)
  predict.internal <- function(model.pair, target.data) {
    ##todo check if there are redundant df castings and namings here
    latitude <- data.frame(latitude=predict(model.pair$latitude, target.data));
    longitude <- data.frame(longitude=predict(model.pair$longitude, target.data));
    return(cbind(latitude=latitude, longitude=longitude));
  }

  #' takes predicted data.frame and calculates validation value(s) based on scoring function
  #'
  #' @param predictions (latitude,longitude) pairs which have been predicted (data.frame format)
  #'
  #' @param target.data (latitude,longitude) pairs which represent real values (data.frame format)
  #'
  #' @param score.fn given function that takes in predicted vector and expected vector and returns scalar value for it
  #'
  #' @return value of score.fn applied to predicted and target.data
  validate.predictions <- function(predictions, target.data, score.fn) {
    longitude <- predictions$longitude;
    latitude <- predictions$latitude;
    return(score.fn(longitude, latitude, xvalid=target.data$longitude, yvalid=target.data$latitude, scale=5000)); #todo pass this parameter as input (...?)
  }

  extracted.features <- features.extraction(features, newdata, t.dist, t.window.length, interval=prediction.interval)
  validation.data <- extract.target.data(newdata, t.dist, t.window.length, interval=prediction.interval)
  predictions <- lapply(trained.models, predict.internal, target.data=extracted.features)
  prediction.validations <- lapply(predictions, validate.predictions, target.data=validation.data, score.fn=score.fn);
  histories <- mapply(list,
                      time=validation.data$time,
                      predictions=predictions,
                      history=prediction.validations,
                      SIMPLIFY=FALSE);
  print(paste("predicted vectors: ", nrow(validation.data)))
  test.counter <<- 0
  return(ldply(validation.data$time, mixture.internal, mixture, histories, t.dist=t.dist, hist.times=take.times(histories)));
}
