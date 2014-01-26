getRandomInterval <- function(data, length=NULL, months=1, days=0, hours=0) {
  startDate <- as.POSIXlt(sample(data$time, size=1), origin=attr(data, "time.origin"));
  return(getFromInterval(data, startDate, length, months, days, hours));
}

getFromInterval <- function(data, startDate, length, months=1, days=0, hours=0) {
  toEnd <- function(date, months, days, hours) {
    .date <- date
    .date$mon <- endDate$mon + months;
    .date$day <- endDate$day + days;
    .date$hours <- endDate$hours + hours;
    return(.date)
  }
  endDate <- if (is.null(length)) toEnd(startDate, months, days, hours) else startDate + length;
  return(getBetween(data, startDate, endDate));
}

filterUser <- function(data, id) {
  return(data[data$userid == id,]);
}

getBetween <- function(data, start, end) {
  stopifnot(start < end)
  return(data[data$time >= as.numeric(start) & data$time <= as.numeric(end),]);
}

filterWithInterval <- function(data, interval=1) {
    start <- data$time[1]
    end <- data$time[nrow(data)]
    n.intervals <- (end - start) / interval + 1
    rows <- ldply(0:n.intervals, function(i) {
        return(getClosestTo(data, start + i * interval))
    })
    return(rows[!duplicated(rows),])
}

getSampleOf <- function(data, size=10, portion=NULL) {
  sampleSize <- ifelse(is.null(portion), size, portion*nrow(data))
  return(data[sample(1:nrow(data), as.integer(sampleSize)),])
}

getSubSample <- function(data, sampleError = 0.03) {
  n = as.integer(min(nrow(data), 1/(sampleError)^2));
  return(getSampleOf(data, size=n));
}

getClosestTo <- function(data, time) {
  return(data[which.min(abs(data$time - time)),])
}

PartitionHistoryData <- function(data, training.length) {
  train.indx <- data$time < min(data$time) + training.length
  training <- data[train.indx,]
  valid <- data[!train.indx,]
  return(list(train=training, valid=valid))
}

VectorsMatchingInTime <- function(vectors, times) {
  matched <- vectors[with(vectors, time %in% times), ]
  return(matched)
}

HistoryWindow <- function(data, end.time, window.length, sample.max.size=1111) {
  history.window <- getBetween(data, end.time - window.length, end.time)
  if (nrow(history.window) > sample.max.size) {
    history.window <- getSampleOf(history.window, size=sample.max.size)
  }
  return(history.window)
}
