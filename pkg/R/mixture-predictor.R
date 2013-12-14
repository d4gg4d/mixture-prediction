## TODO Rdocs...
## takes in list of models, data.frame of new data, function of mixture predictor.
## returns vector/matrix of mixture response corresponding to size of new data.
mixture.predict <- function(features, models, newdata, score.fn, mixture, t.dist, t.window.length) {

  # returns prediction vectors in data.frame
  predict.internal <- function(model.pair, target.data) {
    ##todo check if there are redundant df castings and namings here
    latitude <- data.frame(latitude=predict(model.pair$latitude, target.data));
    longitude <- data.frame(longitude=predict(model.pair$longitude, target.data));
    return(cbind(latitude=latitude, longitude=longitude));
  }

  # returns score vector length of prediction row count
  validate.predictions <- function(predictions, target.data, score.fn) {
    latitude <- predictions$latitude;
    longitude <- predictions$longitude;
    return(score.fn(latitude, longitude, xvalid=target.data$latitude, yvalid=target.data$longitude, scale=5000)); #todo pass this parameter as input (...?)
  }

  history.models <- list(trained.models=models);
  extracted.features <- features.extraction(features, newdata, t.dist, t.window.length);
  predictions <- lapply(history.models$trained.models, predict.internal, target.data=extracted.features);
  validation.data <- extract.target.data(newdata, t.dist, t.window.length)
  histories <- lapply(predictions, validate.predictions, target.data=validation.data, score.fn=score.fn);
  history.models <- mapply(list,
                           trained.model=models,
                           predictions=predictions,
                           history=histories,
                           SIMPLIFY=FALSE);
  ## end of manipulating history-models
  return(ldply(1:nrow(validation.data), mixture.internal, mixture, history.models));
}
