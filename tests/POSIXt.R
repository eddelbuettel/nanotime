
library(nanotime)
pt <- nanotime(Sys.time())
print(showNanotime(pt, lcltzstr="America/Chicago", tgttzstr="America/Chicago"))
