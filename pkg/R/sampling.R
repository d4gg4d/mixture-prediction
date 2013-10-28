
getSampleOf <- function(data, size=10, portion=NULL) {
  sampleSize <- ifelse(is.null(portion), size, portion*nrow(data))
  return(data[sample(1:nrow(data), as.integer(sampleSize)),]);
};

filterUser <- function(id, data) {
  return(data[data$userid == id,]);
}

getBetween <- function(data, start, end) {
  return(data[data$time >= as.numeric(start) & data$time <= as.numeric(end),]);
}
