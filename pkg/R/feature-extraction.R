#' function to constructs sliding window for each desired feature extraction timeslot
#' 
#' @param data from which sliding windows are constructed. Sliding
#' windows are constructed from beginning of data for each target prediction vector.
#' 
#' @param time.dist distance between sliding windows last and second last rows
#'
#' @param t.window.length size of sliding window in time units
#'
#' @return list of sliding windows, where prediction target is last row of the sliding window data.frame
#'
slice.data <- function(data, time.dist, t.window.length, interval=360) {
  predicted.rows <- predicted.target.data(data, time.dist, t.window.length, interval=interval)
  return(alply(predicted.rows, 1, function(row) {
    sliding.window <- getBetween(data, row$time - t.window.length - time.dist, row$time - time.dist)
    return(rbind(sliding.window, row))
  }, .expand=FALSE))
}

## todo documentation
## todo filter duplicated predicted.row slices, because getClosestTo can retun same value on multiple inputs
predicted.target.data <- function(data, time.dist, t.window.length, interval=360) {
  sliding.cursors <- cursors(data, time.dist, t.window.length, interval)
  predicted.rows <- adply(sliding.cursors, 1, function(cursor) {
    return(getClosestTo(data, cursor$time + time.dist))
  })
  return(predicted.rows[with(predicted.rows, !duplicated(time)),])
}

cursors <- function(data, time.dist, window.length, interval) {
  vectors <- filterWithInterval(getBetween(data, min(data$time) + as.numeric(time.dist + window.length), max(data$time) - time.dist), interval)
  stopifnot(!duplicated(vectors$time))
  return(vectors)
}

#' Extract feature from sliding windows. Given list of sliding
#' windows, feature is fitted to window data and then the fitted
#' feature is used to predict its value at desired time. If feature
#' fitting fails it results on NA for feature value.
#' 
#' @param feature feature to be fitted
#'
#' @param windows list of sliding windows where the feature is fitted
#'
#' @return array of extracted features at given time
#'
feature.extraction <- function(feature, windows) {

  feature.fit <- function(data, target) {
    tryCatch(
      {
        fitted.feature <- feature$fit(data)
        return(predict(fitted.feature, newdata=target))
      },
      error=function(e) {
        print(paste("failed extract feature",feature$name,"for time",target$time, sep=" "))
        return(NA)
      })
  }

  return(laply(windows, function(sliding.window) {
      last.row <- sliding.window[nrow(sliding.window),]
      window <- sliding.window[-nrow(sliding.window),]
      return(feature.fit(window, last.row))
    }))
}

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
#' @param keep values that are kept from the original data and are combined with extracted features. Note: apparently keep column names must be in same order as in data...
#'
#' @param interval in which time steps will the feature extraction to be made of, default 1 hour.
#' 
#' @return dataframe where each feature is as its own column 
#' 
features.extraction <- function(features, data, t.dist, t.window.length, keep=c("time"), interval=3600) {
  dataslices <- slice.data(data, t.dist, t.window.length, interval=interval)
  fitted <- data.frame(t(laply(features, feature.extraction, dataslices)))
  kept <- ldply(dataslices, function(slice) {
    return(slice[nrow(slice), names(data) %in% keep])
  })[,-1]
  return(setNames(cbind(kept,fitted), c(keep, unlist(lapply(features, function(feature){ return(feature$name)})))))
}
