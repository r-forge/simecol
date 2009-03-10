##################################################
## Benchmark on an AMD Athlon 6000 (3GHz), 2GB RAM
##################################################

library(simecol)

n <- 1000
# a matrix with n rows and n columns, randomly filled with zero and one
x<-matrix(sample(c(0, 1),  n^2, replace=TRUE), nrow=n, ncol=n)
image(x)
system.time(z <- eightneighbours(x))
#   user  system elapsed
#   0.28    0.00    0.28

n <- 5000
x<-matrix(sample(c(0, 1),  n^2, replace=TRUE), nrow=n, ncol=n)
#image(x) this would take much too long
system.time(z <- eightneighbours(x))
#   user  system elapsed
#  13.03    0.22   13.32

n <- 10000
x<-matrix(sample(c(0, 1),  n^2, replace=TRUE), nrow=n, ncol=n)
#image(x) this would take much too long
system.time(z <- eightneighbours(x))
#Error: cannot allocate vector of size 190.7 Mb
#In addition: Warning messages:
#1: In eightneighbours(x) :
#  Reached total allocation of 1535Mb: see help(memory.size)
