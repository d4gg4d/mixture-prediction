##todo documentation
take.slice <- function(model, index) {
  return(cbind(model$prediction[index,], history=model$history[index]));
};

##todo documentation
create.datacube <- function(indeces, history.models) {
  ##todo for clarity rewrite this one 
  data <- data.frame();
  for (i in indeces) {
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
#' @param time of validation data row for which final prediction is made
#'
#' @param mixture given mixture function that returns final latitude,longitude pair for given index
#'
#' @return final prediction of (latitude, longitude) pair.
#' 
mixture.internal <- function(time, mixture, history.models, t.dist, hist.times) {
    indeces <- create.indeces(time, t.dist, hist.times)
    return(mixture(create.datacube(indeces, history.models)));
}

create.indeces <- function(time, t.dist, time.rows, n.hist=10) {
    current <- which.min(abs(time.rows))
    last.of.history <- which.min(abs(time.rows - t.dist))
    histories <- max(1, last.of.history - n.hist) : current
    return(c(histories,current))
}
