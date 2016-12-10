
library(nanotime)

z <- RcppCCTZ:::parseDouble("1970-01-01T00:00:00.000000001+00:00")
cat("z is: ")
print(z)

x <- nanotime("1970-01-01T00:00:00.000000001+00:00")
cat("x is: ")
print(x)

cat("x+1 is: ")
x <- x + 1
print(x)

cat("y is: ")
y <- nanotime(z)
print(y)
#print(class(y))

cat("y+1 is: ")
y <- y + 1
print(y)

x == y
cat("Done\n")
