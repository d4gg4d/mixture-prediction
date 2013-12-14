## TODO Rdocs...
## takes in list of models, data.frame of new data, function of mixture predictor.
## returns vector/matrix of mixture response corresponding to size of new data.
mixture.predict <- function(features, models, newdata, score.fn, mixture, t.dist, t.window.length) {

  # returns prediction vectors in data.frame
  predict.internal <- function(model.pair, target.data) {
    latitude <- data.frame(predict(model.pair$latitude, target.data));
    longitude <- data.frame(predict(model.pair$longitude, target.data));
    return(cbind(latitude=latitude, longitude=longitude));
  }

  # returns score vector length of prediction row count
  validate.predictions <- function(predictions, target.data, score.fn) {
    latitude <- predictions$latitude.fit;
    longitude <- predictions$longitude.fit;
    return(score.fn(latitude, longitude, xvalid=target.data$latitude, yvalid=target.data$longitude, scale=5000)); #todo pass this parameter as input (...?)
  }

  history.models <- list(trained.models=models);
  extracted.features <- data.frame(t(laply(features, feature.extraction, data=newdata, t.dist=t.dist, t.window.length=t.window.length)));
  predictions <- lapply(history.models$trained.models, predict.internal, target.data=extracted.features);
  histories <- lapply(predictions, validate.predictions, target.data=newdata, score.fn=score.fn);
  history.models <- mapply(list,
                           trained.model=models,
                           predictions=predictions,
                           history=histories,
                           SIMPLIFY=FALSE);
  ## end of manipulating history-models

  ## todo remove 10 limit and verify that it still works
  mixture.results <- lapply(10:nrow(newdata), mixture.internal, mixture, history.models);
  return(ldply(mixture.results, identity));
}
