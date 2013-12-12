## generates data.frames for each row from [data.starttime + distance + length, data.endtime] so that
## it contains data between [row.time - distance - length, row.time - distance]
slice.data <- function(data, time.dist, t.window.length) {
  sliding.cursors <- getBetween(data, min(data$time) + as.numeric(time.dist + t.window.length), max(data$time) - time.dist)
  return(alply(sliding.cursors, 1, function(row) {
    sliding.window <- getBetween(data, row$time - t.window.length, row$time)
    predicted.row <- getClosestTo(data, row$time + time.dist)
    return(rbind(sliding.window, predicted.row))
  }, .expand=FALSE))
}

## calculates predictor variable vectors for each data point based on the t-values
feature.extraction.prototype.call <- function(feature, data, t.dist, t.window.length) {
  ##list of data.frames, size of t.window.length, moved backwards > t.dist, last row is target time
  dataslices <- slice.data(data, t.dist, t.window.length);
  return(ldply(dataslices, function(sliding.window) {
    last.row <- sliding.window[nrow(sliding.window),]
    fitted.feature <- feature$fit(sliding.window[-nrow(sliding.window),])
    ## todo or optionally with(sliding.window,{lm()}) via environment??
    return(predict(fitted.feature, newdata=last.row))
  }))
}
