create.lm.features <- function(formulas, names) {
  stopifnot(length(formulas) == length(names))
  return(mapply(list, fit=lapply(formulas, function(formula) {return(eval(substitute(function(data) {return(lm(ff, data))}, list(ff=formula))))}), name=names, SIMPLIFY=FALSE))
}
