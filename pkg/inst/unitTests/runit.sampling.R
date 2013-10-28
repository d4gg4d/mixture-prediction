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

  checkEquals(filterUser(6, testFrame), integer(0));
  checkEquals(filterUser(5, testFrame), testFrame[10,]);
  checkEquals(filterUser(1, testFrame), testFrame[2:3,]);
}

test.getBetween <- function() {
  testFrame <- data.frame(time=c(as.numeric(as.POSIXct("1970-02-02")), as.numeric(as.POSIXct("1970-02-03")), as.numeric(as.POSIXct("1970-02-04"))));

  checkEquals(getBetween(testFrame, as.POSIXct("1971-02-02"), as.POSIXct("1972-02-02")), integer(0));
  checkEquals(getBetween(testFrame, as.POSIXct("1970-02-02"), as.POSIXct("1970-02-03")), testFrame[1:2,]);
  checkEquals(getBetween(testFrame, as.POSIXct("1970-02-03"), as.POSIXct("1970-02-04")), testFrame[2:3,]);
}
