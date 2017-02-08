
library(nanotime)
suppressMessages(library(bit64))

nt <- nanotime("2017-02-02T17:16:27.633531000+00:00")
print(nt)
print(as.integer64(nt))

