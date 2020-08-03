# call function using the given input parameters
testfun <- function(outFile) pFromGrADS(
  coordFile='coords.txt', 
  gridsPath='~/../Downloads/T/test_pFromGrADS/', 
  outFile=outFile, 
  bbox=c(xmin=70.05, xmax=140.05, ymin=15.05, ymax=59.05), 
  nx=700, 
  ny=440, 
  naValue=-999, 
  p4str="+proj=longlat +datum=WGS84", 
  dateTimeSep='-', 
  returnRasterList=TRUE, 
  recordSize=4
)

system("git checkout dev")
source('pFromGrADS.R')
rList_1 <- testfun('rain-1.txt')

system("git checkout dev-hauke")
source('pFromGrADS.R')
rList_2 <- testfun('rain-2.txt')

identical(rList_1, rList_2)
identical(readLines("rain-1.txt"), readLines("rain-2.txt"))
