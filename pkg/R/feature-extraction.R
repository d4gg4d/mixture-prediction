## generates data.frames for each row from [data.starttime + distance + length, data.endtime] so that
## it contains data between [row.time - distance - length, row.time - distance]
## returns list of data.frames, size of t.window.length, moved backwards > t.dist, last row is target time
slice.data <- function(data, time.dist, t.window.length) {
  sliding.cursors <- getBetween(data, min(data$time) + as.numeric(time.dist + t.window.length), max(data$time) - time.dist)
  return(alply(sliding.cursors, 1, function(row) {
    sliding.window <- getBetween(data, row$time - t.window.length, row$time)
    predicted.row <- getClosestTo(data, row$time + time.dist)
    return(rbind(sliding.window, predicted.row))
  }, .expand=FALSE))
}

## getting target vectors for validation
extract.target.data <- function(data, time.dist, t.window.length) {
  sliding.cursors <- getBetween(data, min(data$time) + as.numeric(time.dist + t.window.length), max(data$time) - time.dist)
  return(adply(sliding.cursors, 1, function(row) {
    return(getClosestTo(data, row$time + time.dist))
  }, .expand=FALSE))
}

## calculates predictor variable vectors for each data point based on the t-values
feature.extraction <- function(feature, windows) {

  feature.fit <- function(data, target) {
    fitted.feature <- feature$fit(data)
    ## todo or optionally with(sliding.window,{lm()}) via environment??
    ## should this be called from feature, e.g. feature$predict?
    return(predict(fitted.feature, newdata=target))
  }

  return(laply(windows, function(sliding.window) {
      last.row <- sliding.window[nrow(sliding.window),]
      window <- sliding.window[-nrow(sliding.window),]
      return(feature.fit(window, last.row))
    }))
}

features.extraction <- function(features, data, t.dist, t.window.length, keep=c("time")) {
  dataslices <- slice.data(data, t.dist, t.window.length)
  fitted <- data.frame(t(laply(features, feature.extraction, dataslices)))
  kept <- ldply(dataslices, function(slice) {
    return(slice[nrow(slice),names(data) %in% keep])
  })[,-1]
  return(setNames(cbind(kept,fitted), c(keep, unlist(lapply(features, function(feature){ return(feature$name)})))))
}
