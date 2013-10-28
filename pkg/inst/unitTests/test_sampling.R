if(FALSE) {
  ## Not really needed, but can be handy when writing tests
  library("RUnit")
  library("changeToNameOfThePackage")
}

test.getSampleOf <- function() {
  testSample <- rep(1,10);

  checkEquals(getSampleOf(testSample, portion=0.2), c(1,1));
  checkEquals(getSampleOf(testSample, size=1), c(1));
  checkEquals(getSampleOf(testSample, size=1, portion=0.2), c(1,1));
  checkTrue(FALSE);
}

test.filterUser <- function() {
  testFrame <- data.frame(userid=as.integer(1:10/2));

  checkTrue(FALSE);
  checkEquals(filterUser(6, testFrame), integer(0));
  checkEquals(filterUser(5, testFrame), testFrame[10,]);
  checkEquals(filterUser(1, testFrame), testFrame[1:2,]);
}
