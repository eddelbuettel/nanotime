
library(nanotime)
library(zoo)

set.seed(42)
x <- 100 + cumsum(rnorm(10))
y <- 100 + cumsum(rnorm(10))

now <- Sys.Date()
dv <- now + 0:9
z <- zoo(cbind(x, y), dv)
z

now <- Sys.time()
dv <- now + 0:9
z <- zoo(cbind(x, y), dv)
z

now <- nanotime(as.numeric(Sys.time()))                  
dv <- now + 0:9
z <- zoo(cbind(x, y), dv)
z
