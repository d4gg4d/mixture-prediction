##todo documentation
take.slice <- function(model, index) {
  return(cbind(model$prediction[index,], history=model$history[index]));
};

##todo documentation
create.datacube <- function(index, history.models) {
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

#' Method for doing final decision about prediction result. It can use
#' history information which is encapulated to datacube which is its
#' input.
#' 
#' @param index of validation data row for which final prediction is made
#'
#' @param mixture given mixture function that returns final latitude,longitude pair for given index
#'
#' @return final prediction of (latitude, longitude) pair.
#' 
mixture.internal <- function(index, mixture, history.models) {
  return(mixture(create.datacube(index, history.models)));
}
