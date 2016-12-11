
library(nanotime)
pt <- nanotime(Sys.time())
format(pt, lcltzstr="America/Chicago", tgttzstr="America/Chicago")
