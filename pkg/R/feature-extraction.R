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
#' @return dataframe where each feature is added to it as a column
#' 
FeatureExtraction <- function(features, data, t.dist, t.window.length, interval=3600, maximum.sample=1111) {
  targets <- Cursors(data, t.dist, t.window.length, interval)
  fitted <- adply(targets, 1, function(prediction) {
    history.window <- HistoryWindow(data, prediction$time - t.dist, t.window.length, sample.max.size=maximum.sample)
    return(unlist(llply(features, FeatureFit, data=history.window, target=prediction)))
  })
  return(fitted)
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
      print(paste("failed extract feature", feature$name, "for time", target$time, ". Cause:", e, sep=" "))
      return(failure(feature))
    })
}
