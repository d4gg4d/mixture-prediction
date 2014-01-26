if(FALSE) {
  ## Not really needed, but can be handy when writing tests
  library("RUnit")
  library("mixturePrediction")
}

test.getSampleOf <- function() {
  testSample <- data.frame(rep(1,10));

  checkEquals(getSampleOf(testSample, portion=0.2), c(1,1));
  checkEquals(getSampleOf(testSample, size=1), c(1));
  checkEquals(getSampleOf(testSample, size=1, portion=0.2), c(1,1));
}

test.filterUser <- function() {
  testFrame <- data.frame(userid=as.integer(1:10/2));

  checkEquals(filterUser(testFrame, 6), integer(0));
  checkEquals(filterUser(testFrame, 5), testFrame[10,]);
  checkEquals(filterUser(testFrame, 1), testFrame[2:3,]);
}

test.getBetween <- function() {
  testFrame <- data.frame(time=c(as.numeric(as.POSIXct("1970-02-02")), as.numeric(as.POSIXct("1970-02-03")), as.numeric(as.POSIXct("1970-02-04"))));

  checkEquals(getBetween(testFrame, as.POSIXct("1971-02-02"), as.POSIXct("1972-02-02")), integer(0));
  checkEquals(getBetween(testFrame, as.POSIXct("1970-02-02"), as.POSIXct("1970-02-03")), testFrame[1:2,]);
  checkEquals(getBetween(testFrame, as.POSIXct("1970-02-03"), as.POSIXct("1970-02-04")), testFrame[2:3,]);
}

test.getClosestTo <- function() {
    test <- data.frame(time=1:10)
    checkEquals(getClosestTo(test, 4.2), 4)
}

test.filterWithInterval <- function() {
    test <- data.frame(time=c(2:10,10), a=c((2:10)*5,50))
    expected <- data.frame(time=c(2,(2:5)*2), a=c(10,(2:5)*10))
    checkEquals(mixturePrediction:::filterWithInterval(test, interval=2), expected)
}

test.PartitionHistoryData <- function() {
    test <- data.frame(time=2:10, a=2:10)
    expected <- list(train=data.frame(time=2:4, a=2:4), valid=data.frame(time=5:10, a=5:10))
    #checkEquals(PartitionHistoryData(test, 3), expected) this complains about mean relative diff
    checkEquals(PartitionHistoryData(test, 3)$train, expected$train)
    checkEquals(PartitionHistoryData(test, 3)$valid$time, expected$valid$time)
    checkEquals(PartitionHistoryData(test, 3)$valid$a, expected$valid$a)
}
