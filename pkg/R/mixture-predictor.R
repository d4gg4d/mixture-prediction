
## takes in list of models, data.frame of new data, function of mixture predictor.

## returns vector/matrix of mixture response corresponding to size of new data.
mixture.predict <- function(models, newdata, score.fn, mixture) {

  history.models <- list(trained.models=models);

  # returns prediction vectors in data.frame
  prediction.prototype.call <- function(hmodel, target.data) {
    ##todo mangle data for each prediction variable inner-model
    return(data.frame(predict(hmodel$trained.model, target.data, interval="confidence")));
  }

  # returns score vector length of prediction row count 
  referee.prototype.call <- function(hmodel, target.data, score.function) {
    predicions <- hmodel$predictions$fit;
    return(score.function(predictions, target.data));
  }
  
  predictions <- lapply(history.models, prediction.prototype.call, target.data=newdata);
  histories <- lapply(history.models, referee.prototype.call, target.data=newdata, score.function=score.fn);
  
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
        return(cbind(model$prediction[index,], model$history[index]));
      };
      datacube <- list();
      ## todo lapply...
      for (i in min(1, index - 5) : index) {
        c(slice, lapply(history.models, take.slice, i));
      };
      return(datacube);
    };
    return(mixture(create.datacube(index, history.models)));
  }
  mixture.results <- lapply(1:nrows(newdata), mixture.prototype.call, mixture, history.models);
  return(data.frame(mixture.results));
}
