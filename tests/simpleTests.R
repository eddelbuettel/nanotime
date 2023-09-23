
library(nanotime)

z <- RcppCCTZ:::parseDouble("1970-01-01T00:00:00.000000001+00:00")
cat("z is: ")
print(z)

x <- nanotime("1970-01-01T00:00:00.000000001+00:00")
cat("x is: ")
print(x)
format(x)

cat("x+1 is: ")
x <- x + 1
print(x)
format(x)

cat("y is: ")
y <- nanotime(z)
print(y)
#print(class(y))
format(y)

cat("y+1 is: ")
y <- y + 1
print(y)
format(y)

print(x == y)

od <- getOption("digits.secs")
options("digits.secs"=6)
as.POSIXct(x)
as.POSIXct(x+1000)
as.POSIXlt(x)
as.POSIXlt(x+1000)
as.Date(x)
as.Date(x, tz="UTC")
options("digits.secs"=od)


y <- nanotime(1L)  # integer, may dispatch via nanotime.numeric
print(y)

y <- nanotime(1)   # numeric
print(y)

## v <- nanotime:::nanotime.default(1)     # forced call, gets imprecise value
## print(v)


options("nanotimeFormat"="%Y-%m-%d %H:%M:%S")
format(x <- nanotime("1970-01-01 00:00:00"))

options("nanotimeFormat"="%Y-%m-%d %H:%M:%E*S")
format(x <- nanotime("1970-01-01 00:00:00.123456789"))

options("nanotimeFormat"="%Y-%m-%d %H:%M:%E*S%Ez")   # default

cat("Done\n")
