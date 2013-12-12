## TODO Rdocs...
## takes in list of models, data.frame of new data, function of mixture predictor.
## returns vector/matrix of mixture response corresponding to size of new data.
mixture.predict <- function(features, models, newdata, score.fn, mixture, t.dist, t.window.lenght) {

  # returns prediction vectors in data.frame
  prediction.prototype.call <- function(model.pair, target.data) {
    latitude <- data.frame(predict(model.pair$latitude, target.data));
    longitude <- data.frame(predict(model.pair$longitude, target.data));
    return(cbind(latitude=latitude, longitude=longitude));
  }

  # returns score vector length of prediction row count
  referee.prototype.call <- function(predictions, target.data, score.function) {
    latitude <- predictions$latitude.fit;
    longitude <- predictions$longitude.fit;
    return(score.function(latitude, longitude, xvalid=target.data$latitude, yvalid=target.data$longitude, scale=5000)); #todo pass this parameter as input (...?)
  }

  history.models <- list(trained.models=models);

  feature.extraction <- data.frame(t(tlaply(features, feature.extraction.prototype.call, data=newdata, t.dist=t.dist, t.window.length=t.window.length)));
  predictions <- lapply(history.models$trained.models, prediction.prototype.call, target.data=feature.extraction);
  histories <- lapply(predictions, referee.prototype.call, target.data=newdata, score.function=score.fn);

  history.models <- mapply(list,
                           trained.model=models,
                           predictions=predictions,
                           history=histories,
                           SIMPLIFY=FALSE);
  ## end of manipulating history-models

  ## todo remove 10 limit and verify that it still works
  mixture.results <- lapply(10:nrow(newdata), mixture.prototype.call, mixture, history.models);
  return(ldply(mixture.results, identity));
}
