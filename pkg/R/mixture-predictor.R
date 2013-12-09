## TODO Rdocs...
## takes in list of models, data.frame of new data, function of mixture predictor.
## returns vector/matrix of mixture response corresponding to size of new data.
mixture.predict <- function(features, models, newdata, score.fn, mixture, t.dist, t.window.lenght) {

  # calculates predictor variable vectors for each data point based on the t-values
  feature.extraction.prototype.call <- function(feature, data, t.dist, t.window.length) {

      ## generates data.frames for each row from [data.starttime + distance + length, data.endtime] so that
      ## it contains data between [row.time - distance - length, row.time - distance]
    slice.data <- function(data, time.dist, t.window.length) {
      sliding.cursors <- getBetween(data, min(data$time) + as.numeric(time.dist + t.window.length), max(data$time) - time.dist)
      return(alply(sliding.cursors, 1, function(row) {
        sliding.window <- getBetween(data, row$time - t.window.length, row$time)
        predicted.row <- getClosestTo(data, row$time + time.dist)
        return(rbind(sliding.window, predicted.row))
      }))
    }

    dataslices <- slice.data(data, t.dist, t.window.length); ##list of data.frames, size of t.window.length, moved backwards > t.dist, last row is target time
    return(ldply(dataslices, function(sliding.window) {
      last.row <- sliding.window[nrow(sliding.window),]
      fitted.feature <- feature$fit(sliding.window[-nrow(sliding.window),]);
      return(predict(fitted.feature, newdata=last.row))
    }));
  }

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

  feature.extraction <- ldply(features, feature.extraction.prototype.call, data=newdata, t.dist=t.dist, t.window.length=t.window.length);
  predictions <- lapply(history.models$trained.models, prediction.prototype.call, target.data=feature.extraction);
  histories <- lapply(predictions, referee.prototype.call, target.data=newdata, score.function=score.fn);

  history.models <- mapply(list,
                           trained.model=models,
                           predictions=predictions,
                           history=histories,
                           SIMPLIFY=FALSE);
  ## end of manipulating history-models

  ## input for mixture function will be datacube (length(models), length(response.variables), history.size)
  mixture.prototype.call <- function(index, mixture, history.models) {

    create.datacube <- function(index, history.models) {
      take.slice <- function(model, index) {
        return(cbind(model$prediction[index,], history=model$history[index]));
      };

      ##todo for clarity rewrite this one 
      data <- data.frame();
      for (i in max(1, index - 4) : index) {
        data <- rbind(data, cbind(ldply(history.models, take.slice, i), ID=i));
      };
      data$row <- 1:nrow(data) %% length(history.models);
      m <- melt(data, id.vars = c("row", "ID"));
      datacube <- acast(m, row ~ variable ~ ID);
      return(datacube);
    };
    
    return(mixture(create.datacube(index, history.models)));
  }
  ## todo remove 10 limit and verify that it still works
  mixture.results <- lapply(10:nrow(newdata), mixture.prototype.call, mixture, history.models);
  return(ldply(mixture.results, identity));
}
