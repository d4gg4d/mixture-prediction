#' Method for doing final decision about prediction result. It can use
#' history information which is encapulated to datacube which is its
#' input.
#' 
#' @param mixture process that decides from inputted data which vector
#' should be the final output. TODO should this be maybe list of mixtures...
#'
#' @param history data.frame where each row represents output of one
#' model pair
#'
#' @param t.dist timewise distance of predicted vector and history
#' window that which it is going to be used.
#'
#' @param hist.length length in time where to gather history
#' information.
#' 
#' @return data.frame of final output for each target data
#' 
MixtureInternal <- function(Mixture, history, t.dist, hist.length) {
  targets <- getBetween(history, min(history$time) + t.dist + hist.length, max(history$time))
  times <- targets[with(targets, !duplicated(time)), ]$time
  values <- ldply(times, function(prediction.time) {
    prediction.candidates <- history[history$time == prediction.time, ]
    history.window <- getBetween(history, prediction.time - t.dist - hist.length, prediction.time - t.dist)
    final.model.id <- Mixture(history.window, t.dist)
    return(prediction.candidates[prediction.candidates == final.model.id, ])
  })
  return(values)
}
