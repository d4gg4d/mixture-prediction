## TODO documentation
create.lm.features <- function(formulas, names) {
  stopifnot(length(formulas) == length(names))
  return(llply(mapply(list, formula=formulas, name=names, SIMPLIFY=FALSE), function(val) {
    return(featurelm(val$formula, val$name))
  }))
}

## TODO documentation
PredictionError <- function(prediction, valid, score.fn=NULL) {
    valid.predicted <- valid[valid$time %in% prediction$time,]
    valid.predicted <- valid.predicted[with(valid.predicted, !duplicated(time)),]
    valid.predicted <- valid.predicted[with(valid.predicted, order(time)),]
    stopifnot(valid.predicted$time == prediction$time)
    difference.long <- prediction$longitude - valid.predicted$longitude
    difference.lat <- prediction$latitude - valid.predicted$latitude
    score.value <- ifelse(!is.null(score.fn),score.fn(prediction$longitude, prediction$latitude, xvalid=valid.predicted$longitude, yvalid=valid.predicted$latitude, scale=5000), NA) ##TODO how to pass funtion properly with parameters
    return(data.frame(
      time=prediction$time,
      longitude=difference.long,
      latitude=difference.lat,
      score=score.value))
}
