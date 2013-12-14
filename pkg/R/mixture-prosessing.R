take.slice <- function(model, index) {
  return(cbind(model$prediction[index,], history=model$history[index]));
};

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

## input for mixture function will be datacube (length(models), length(response.variables), history.size)
mixture.internal <- function(index, mixture, history.models) {
  return(mixture(create.datacube(index, history.models)));
}
