
getSampleOf <- function(data, size=10, portion=NULL) {
  sampleSize <- ifelse(portion == NULL, size, portion*nrow(data))
  return(data[sample(1:nrow(data),as.integer(sampleSize)),]);
};
