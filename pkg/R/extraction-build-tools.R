## TODO documentation
create.lm.features <- function(formulas, names) {
  stopifnot(length(formulas) == length(names))
  return(mapply(list, fit=lapply(formulas, function(formula) {return(eval(substitute(function(data) {return(lm(ff, data))}, list(ff=formula))))}), name=names, SIMPLIFY=FALSE))
}

## TODO documentation
ModelPairing <- function(long.models, lat.models) {
  stopifnot(length(long.models) == length(lat.models))
  return(mapply(list, longitude=long.models, latitude=lat.models, SIMPLIFY=FALSE));
}

## TODO documentation
lm.create.models <- function(formulas, data) {
  return(lapply(formulas, lm, data=data));
}
