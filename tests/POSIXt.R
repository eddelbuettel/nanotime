
library(nanotime)

format(nanotime(Sys.time()),
       lcltzstr="America/Chicago", tgttzstr="America/Chicago")

format(nanotime(as.POSIXlt(Sys.time())),
       lcltzstr="America/Chicago", tgttzstr="America/Chicago")
       
format(nanotime(Sys.Date()),
       lcltzstr="America/Chicago", tgttzstr="America/Chicago")


