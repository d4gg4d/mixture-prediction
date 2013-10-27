library('RUnit')

source('R/sampling.R')
 
test.suite <- defineTestSuite("sampling",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test_\\w+\\.R')
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
