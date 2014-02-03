#' Wrapper method for feature extraction. Creates data slices, extract
#' kept values and combines results from actual method (and names
#' columns). Notice that dataslices keeps predicted target vector in
#' its last row.
#' 
#' @param features list of features to be fitted.
#'
#' @param data to which against feature extraction is made out of
#'
#' @param t.dist time distance of sliding window and fitted value
#'
#' @param t.window.length size of the sliding window in time units
#'
#' @param interval in which time steps will the feature extraction to be made of, default 1 hour.
#' 
#' @param keep values that are kept from the original data and are combined with extracted features. Note: apparently keep column names must be in same order as in data...
#' 
#' @return dataframe where each feature is as its own column 
#' 
FeatureExtraction <- function(features, data, t.dist, t.window.length, interval=3600, keep=c("time"), maximum.sample=1111) {
  targets <- Cursors(data, t.dist, t.window.length, interval)
  fitted <- adply(targets, 1, function(prediction) {
    history.window <- HistoryWindow(data, prediction$time - t.dist, t.window.length, sample.max.size=maximum.sample)
    return(laply(features, FeatureFit, data=history.window, target=prediction))
  })
  feature.names <- unlist(sapply(features, function(f) { return(name(f))}))
  fitted <- setNames(fitted, c(names(targets), feature.names))
  to.keep <- c(keep, feature.names)
  return(fitted[, names(fitted) %in% to.keep])
}

Cursors <- function(data, time.dist, window.length, interval) {
  start <- min(data$time) + as.numeric(time.dist + window.length)
  end <- max(data$time)
  vectors <- filterWithInterval(getBetween(data, start, end), interval)
  stopifnot(!duplicated(vectors$time))
  return(vectors)
}

FeatureFit <- function(feature, data, target) {
  tryCatch(
    {
      return(predict(fit(feature, data=data), newdata=target))
    },
    error=function(e) {
      print(paste("failed extract feature", feature$name, "for time", target$time, sep=" "))
      return(failure(feature))
    })
}
