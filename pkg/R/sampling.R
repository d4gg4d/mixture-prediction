getRandomInterval <- function(data, length, months=1, days=0, hours=0) {
  startDate <- as.POSIXlt(sample(data$time, size=1), origin=attr(data, "time.origin"));
  return(getFromInterval(startDate, data, length, months, days, hours));
}

getFromInterval <- function(startDate, data, length, months=1, days=0, hours=0) {
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

getSampleOf <- function(data, size=10, portion=NULL) {
  sampleSize <- ifelse(is.null(portion), size, portion*nrow(data))
  return(data[sample(1:nrow(data), as.integer(sampleSize)),]);
};

filterUser <- function(id, data) {
  return(data[data$userid == id,]);
}

getBetween <- function(data, start, end) {
  return(data[data$time >= as.numeric(start) & data$time <= as.numeric(end),]);
}

getSubSample <- function(data, sampleError = 0.03) {
  n = as.integer(min(nrow(data), 1/(sampleError)^2));
  return(getSampleOf(data, size=n));
}

getClosestTo <- function(data, time) {
  return(data[which.min(abs(data$time - time)),])
}

take.times <- function(histories) {
    return(laply(histories, function(el) {
        return(el$time)
    }))
}
