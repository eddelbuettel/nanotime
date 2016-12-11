
library(nanotime)
suppressMessages(library(zoo))          

set.seed(42)
x <- 100 + cumsum(rnorm(10))
y <- 100 + cumsum(rnorm(10))
mat <- cbind(x, y)

now <- Sys.Date()
z <- zoo(mat, now + 0:9)
z

now <- Sys.time()
z <- zoo(mat, now + 0:9)
z

now <- nanotime(Sys.time())                  
z <- zoo(mat, now + 11*(0:9))           # inc. by 11 to more visible, 1 works too
z

