
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


y <- nanotime(1L)  # integer, may dispatch via nanotime.numeric
print(y)

y <- nanotime(1)   # numeric
print(y)

y <- nanotime:::nanotime.default(1)     # forced call, gets imprecise value
print(y)

cat("Done\n")
