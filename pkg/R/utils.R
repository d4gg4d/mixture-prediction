## TODO documentation
create.lm.features <- function(formulas, names) {
  stopifnot(length(formulas) == length(names))
  return(mapply(list, fit=lapply(formulas, function(formula) {return(eval(substitute(function(data) {return(lm(ff, data))}, list(ff=formula))))}), name=names, SIMPLIFY=FALSE))
}

## TODO documentation
ModelPairing <- function(ids, long.models, lat.models) {
  stopifnot(length(long.models) == length(lat.models))
  return(mapply(list, modelid=ids, longitude=long.models, latitude=lat.models, SIMPLIFY=FALSE));
}

## TODO documentation
lm.create.models <- function(formulas, data) {
  return(lapply(formulas, lm, data=data));
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
