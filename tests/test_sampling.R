test.sample <- function() {
  testSample <- rep(1,10);

  checkEquals(getSampleOf(testSample, portion=0.2), c(1,1));
  checkEquals(getSampleOf(testSample, size=1), c(1));
  checkEquals(getSampleOf(testSample, size=1, portion=0.2), c(1,1));
}
