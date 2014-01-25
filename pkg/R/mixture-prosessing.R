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
#' @param mixture process that decides from inputted data which vector
#' should be the final output
#'
#' @param history data.frame where each row represents output of one
#' model pair
#'
#' @return data.frame of final output for each target data
#' 
MixtureInternal <- function(mixture, history) {
  ## TODO process whole data cube stuff...
  return(function(time) {
    indeces <- create.indeces(time, t.dist, hist.times)
    return(mixture(Datacube(indeces, history))) ##TODO remember to return time as well...
  })
}

## todo documentation
create.indeces <- function(time, t.dist, time.rows, n.hist=10) {
    times <- time.rows - time
    current <- which.min(abs(times))
    last.of.history <- which.min(abs(times + t.dist))
    histories <- max(1, last.of.history - n.hist) : last.of.history
    indeces <- c(histories,current)
    return(indeces[!duplicated(indeces)])
}
