
library(nanotime)
library(bit64)

milli <- 1e6
sec <- 1e9
minute <- 60 * sec
hour <- 60 * minute

## constructors
##test_nanoduration <- function() {
expect_identical(nanoduration(1,1,1,1), as.nanoduration("01:01:01.000_000_001"))
expect_identical(nanoduration(-1,0,0,1), as.nanoduration("-00:59:59.999_999_999"))
## R recycling:
expect_identical(nanoduration(1:2,1,1,1), c(as.nanoduration("01:01:01.000_000_001"),
                                        as.nanoduration("02:01:01.000_000_001")))

##test_as.nanoduration_character_and_numeric <- function() {
expect_identical(as.nanoduration("1:00:00"), as.nanoduration(hour))
expect_identical(as.nanoduration("-1:00:00"), -as.nanoduration(hour))
expect_identical(as.nanoduration("0:01:00"), as.nanoduration(minute))
expect_identical(as.nanoduration("0:00:01"), as.nanoduration(sec))
expect_identical(as.nanoduration("0:00:00.001"), as.nanoduration(milli))
expect_identical(c(as.nanoduration("1:00:00"), c(as.nanoduration("2:00:00"))),
                 c(as.nanoduration(hour), as.nanoduration(2*hour)))
expect_identical(as.nanoduration(NA_integer64_), as.nanoduration(as.integer64("-9223372036854775808")))

## check name:
n1 <- as.nanoduration(hour)
names(n1)  <- "a"
expect_identical(as.nanoduration(c(a="1:00:00")), as.nanoduration(n1))

expect_error(as.nanoduration("10:aa"), "cannot parse nanoduration")
expect_error(as.nanoduration("1000a"), "cannot parse nanoduration")
expect_error(as.nanoduration("10:10:10.1000a"), "cannot parse nanoduration")
expect_error(as.nanoduration("a"), "cannot parse nanoduration")
expect_error(as.nanoduration(""), "cannot parse nanoduration")

##test_as.nanoduration_integer64 <- function() {
expect_identical(as.nanoduration(as.integer64(hour)), as.nanoduration(hour))
expect_identical(as.nanoduration(c(a=as.integer64(hour), b=as.integer64(2*hour))),
                 as.nanoduration(c(a=hour, b=2*hour)))

##test_as.nanoduration_integer <- function() {
expect_identical(as.nanoduration(as.integer(sec)), as.nanoduration(sec))
expect_identical(as.nanoduration(c(a=as.integer(milli), b=as.integer(2*milli))),
                 as.nanoduration(c(a=milli, b=2*milli)))

##test_as.integer64 <- function() {
expect_identical(as.integer64(as.nanoduration(hour)), as.integer64(hour))
expect_identical(as.integer64(as.nanoduration(1:1000)), as.integer64(1:1000))



## show/print/format
##test_show <- function() {
d <- nanoduration(1,1,1,1)
expect_identical(show(d), "01:01:01.000_000_001")
expect_identical(show(-d), "-01:01:01.000_000_001")
expect_true(is.na(show(as.nanoduration(as.integer64("-9223372036854775808")))))

##test_print <- function() {
d <- nanoduration(1,1,1,1)
expect_identical(print(d), "01:01:01.000_000_001")

##test_print_name <- function() {
d <- nanoduration(1,1,1,1)
names(d) <- "a"
expect_identical(print(d), c(a="01:01:01.000_000_001"))

##test_format <- function() {
d <- nanoduration(1,1,1,1)
expect_identical(format(d), "01:01:01.000_000_001")


## subset:
##test_subset_int <- function() {
dd <- as.nanoduration(1:10)
expect_identical(dd[1], as.nanoduration(1))

##test_subset_logical <- function() {
dd <- as.nanoduration(1:4)
expect_identical(dd[c(T,F,F,F)], dd[1])
expect_identical(dd[c(F,T,F,F)], dd[2])
expect_identical(dd[c(F,F,T,F)], dd[3])
expect_identical(dd[c(F,F,F,T)], dd[4])
expect_identical(dd[TRUE], dd)
expect_identical(dd[c(F,T,T,F)], dd[2:3])

##test_subset_character <- function() {
dd <- c(x=as.nanoduration(1), y=as.nanoduration(2))
expect_identical(dd["x"], c(x=as.nanoduration(1)))
expect_identical(dd["y"], c(y=as.nanoduration(2)))
## also check out of bound subset
res <- as.nanoduration(as.integer64(NA))
names(res) <- NA
## LLL underlying issue with 'integer64':
## expect_identical(dd["a"], res)


## subassign:
##test_subassign_logical <- function() {
x <- as.nanoduration(1:10)
x[c(T,T,T,F,F,F,F,F,F,F)] <- as.nanoduration(2:4)
expect_identical(x, as.nanoduration(c(2:4, 4:10)))

##test_subassign_numeric <- function() {
x <- as.nanoduration(1:10)
x[1:3] <- as.nanoduration(2:4)
x[4:10] <- as.nanoduration(5:11)
expect_identical(x, as.nanoduration(2:11))

##test_subsassign_character <- function() {
dd <- c(a=as.nanoduration(1), b=as.nanoduration(2), c=as.nanoduration(3), d=as.nanoduration(4))
dd[c("b", "c")] <- as.nanoduration(20:21)
expected <- c(a=as.nanoduration(1), b=as.nanoduration(20), c=as.nanoduration(21), d=as.nanoduration(4))
expect_identical(dd, expected)


##test_square_bracket <- function() {
dd <- c(a=as.nanoduration(1), b=as.nanoduration(2), c=as.nanoduration(3), d=as.nanoduration(4))
dd_nonames <- as.nanoduration(1:4)
expect_identical(dd_nonames[1], dd[[1]])
expect_identical(dd_nonames[2], dd[[2]])
expect_identical(dd_nonames[3], dd[[3]])


## test names
##test_get_names <- function() {
dd <- c(a=as.nanoduration(1), b=as.nanoduration(2), c=as.nanoduration(3), d=as.nanoduration(4))
expect_identical(names(dd), c("a","b","c","d"))

##test_set_names <- function() {
names <- c("a","b","c","d")
dd <- as.nanoduration(1:4)
names(dd) <- names
expect_identical(names(dd), names)
names(dd)[1] <- "x"
expect_identical(names(dd), c("x","b","c","d"))


## test scalar/vector mix LLL

## ops
## -
##test_nanoduration_minus_nanoduration <- function() {
expect_identical(as.nanoduration("1:00:00") - as.nanoduration("00:01:00"), as.nanoduration("00:59:00"))
expect_identical(as.nanoduration("-1:00:00") - as.nanoduration("00:01:00"), as.nanoduration("-01:01:00"))
expect_identical(as.nanoduration("1:00:00.1") - as.nanoduration("1:00:00"), as.nanoduration("0.1"))
expect_identical(as.nanoduration(1:10) - as.nanoduration(0:9), as.nanoduration(rep(1, 10)))
expect_identical(as.nanoduration(1:10) - as.nanoduration(1), as.nanoduration(0:9))
expect_identical(c(a=as.nanoduration(1), b=as.nanoduration(1)) - as.nanoduration(1),
                 as.nanoduration(c(a=0, b=0)))

##test_nanoduration_minus_numeric <- function() {
expect_identical(as.nanoduration("1:00:00") - minute, as.nanoduration("00:59:00"))
expect_identical(as.nanoduration(1:10) - 0.0:9, as.nanoduration(rep(1, 10)))
expect_identical(as.nanoduration(1:10) - 1, as.nanoduration(0:9))
expect_identical(c(a=as.nanoduration(1), b=as.nanoduration(1)) - 1, as.nanoduration(c(a=0, b=0)))

##test_nanoduration_minus_integer <- function() {
expect_identical(as.nanoduration("1:00:00") - as.integer(milli), as.nanoduration("00:59:59.999"))
expect_identical(as.nanoduration(1:10) - 0:9, as.nanoduration(rep(1, 10)))
expect_identical(as.nanoduration(1:10) - as.integer(1), as.nanoduration(0:9))
expect_identical(c(a=as.nanoduration(1), b=as.nanoduration(1)) - as.integer(1),
                 as.nanoduration(c(a=0, b=0)))

##test_nanoduration_minus_integer64 <- function() {
expect_identical(as.nanoduration("1:00:00") - as.integer64(minute), as.nanoduration("00:59:00"))
expect_identical(as.nanoduration(1:10) - as.integer64(0:9), as.nanoduration(rep(1, 10)))
expect_identical(as.nanoduration(1:10) - as.integer64(1), as.nanoduration(0:9))
expect_identical(c(a=as.nanoduration(1), b=as.nanoduration(1)) - c(c=as.integer64(1)),
                 as.nanoduration(c(a=0, b=0)))

##test_integer64_minus_nanoduration <- function() {
expect_identical(as.integer64(hour) - as.nanoduration("1:00:00"), as.nanoduration("0:00:00"))

##test_nanoduration_minus_integer64 <- function() {
expect_identical(as.nanoduration("1:00:00") - as.integer64(hour), as.nanoduration("0:00:00"))

##test_numeric_minus_nanoduration <- function() {
expect_identical(hour - as.nanoduration("1:00:00"), as.nanoduration("0:00:00"))

##test_integer_minus_nanoduration <- function() {
expect_identical(as.integer(1e9) - as.nanoduration("1:00:00"), as.nanoduration("-00:59:59"))

## LLL name issue here due do to underlying issues with 'bit64'
## ##test_numeric_minus_nanoduration <- function() {
##     expect_identical(2 - c(a=as.nanoduration(1), b=as.nanoduration(2)),
##                    c(a=as.nanoduration(1), b=as.nanoduration(0)))
##     expect_identical(2 - c(a=as.integer64(1), b=as.integer64(2)),
##                    c(a=as.integer64(1), b=as.integer64(0)))

## 
##test_any_minus_nanoduration <- function() {
expect_error("a" - as.nanoduration("1:00:00"), "invalid operand types")

##test_nanoduration_minus_any <- function() {
expect_error(as.nanoduration("1:00:00") - "a", "invalid operand types")


## +
##test_nanoduration_plus_nanoduration <- function() {
expect_identical(as.nanoduration("1:00:00") + as.nanoduration("00:01:00"), as.nanoduration("01:01:00"))
expect_identical(as.nanoduration("-1:00:00") + as.nanoduration("00:01:00"), as.nanoduration("-00:59:00"))
expect_identical(as.nanoduration("-1:00:00.1") + as.nanoduration("1:00:00"), as.nanoduration("-0.1"))
expect_identical(-as.nanoduration(1:10) + as.nanoduration(0:9), as.nanoduration(rep(-1, 10)))
expect_identical(as.nanoduration(1:10) + as.nanoduration(1), as.nanoduration(2:11))
expect_identical(c(a=as.nanoduration(1), b=as.nanoduration(1)) + as.nanoduration(1),
                 as.nanoduration(c(a=2, b=2)))

##test_integer64_plus_nanoduration <- function() {
expect_identical(as.integer64(hour) + as.nanoduration("1:00:00"), as.nanoduration("2:00:00"))

##test_nanoduration_plus_integer64 <- function() {
expect_identical(as.nanoduration("1:00:00") + as.integer64(hour), as.nanoduration("2:00:00"))

##test_nanoduration_plus_numeric <- function() {
expect_identical(as.nanoduration("1:00:00") + hour, as.nanoduration("2:00:00"))

##test_numeric_plus_nanoduration <- function() {
expect_identical(hour + as.nanoduration("1:00:00"), as.nanoduration("2:00:00"))

##test_character_plus_nanoduration <- function() {
expect_error("hello" + as.nanoduration("1:00:00"), "invalid operand types")

##test_any_plus_nanoduration <- function() {
expect_error("a" + as.nanoduration("1:00:00"), "invalid operand types")

##test_nanoduration_plus_any <- function() {
expect_error(as.nanoduration("1:00:00") + "a", "invalid operand types")


## *
##test_nanoduration_times_numeric <- function() {
expect_identical(as.nanoduration("00:01:00") * 3, as.nanoduration("00:03:00"))

##test_numeric_times_nanoduration <- function() {
expect_identical(3 * as.nanoduration("00:01:00"), as.nanoduration("00:03:00"))

##test_nanoduration_times_integer64 <- function() {
expect_identical(as.nanoduration("00:01:00") * as.integer64(3), as.nanoduration("00:03:00"))

##test_nanoduration_times_nanoduration <- function() {
expect_error(as.nanoduration(1) * as.nanoduration(2), "invalid operand types")

## LLL have to fix this, dependent also on 'integer64' fixes...
## ##test_numeric_times_nanoduration <- function() {
##     expect_identical(1.5 * as.nanoduration("00:01:00"), as.nanoduration("00:01:30"))
## 
##test_numeric_times_nanoduration <- function() {
expect_identical(3 * as.nanoduration("00:01:00"), as.nanoduration("00:03:00"))

##test_integer64_times_nanoduration <- function() {
expect_identical(as.integer64(3) * as.nanoduration("00:01:00"), as.nanoduration("00:03:00"))

##test_any_times_nanoduration <- function() {
expect_error("hello" * as.nanoduration("00:01:00"), "invalid operand types")

##test_nanoduration_times_character <- function() {
expect_error(as.nanoduration("00:01:00") *  "hello", "invalid operand types")

## /
##test_nanoduration_div_numeric <- function() {
expect_identical(as.nanoduration("00:03:00") / 3, as.nanoduration("00:01:00"))

##test_nanoduration_div_integer64 <- function() {
expect_identical(as.nanoduration("00:03:00") / as.integer64(3), as.nanoduration("00:01:00"))

##test_nanoduration_div_integer <- function() {
expect_identical(as.nanoduration("00:03:00") / as.integer(3), as.nanoduration("00:01:00"))

##test_numeric_div_nanoduration <- function() {
expect_error(1.5 / as.nanoduration("00:01:00"), "invalid operand types")

##test_nanoduration_div_any <- function() {
expect_error(as.nanoduration("1:00:00") / "a", "invalid operand types")

##test_nanoduration_div_nanoduration <- function() {
expect_error(as.nanoduration(1) / as.nanoduration(2), "invalid operand types")


## unary
##test_unary_minus <- function() {
expect_identical(-as.nanoduration("00:01:00"), as.nanoduration("-00:01:00"))


##test_unary_plus <- function() {
expect_identical(+as.nanoduration("00:01:00"), as.nanoduration("00:01:00"))


## Summary
##test_sum <- function() {
## error in underlying bit64: LLL
## expect_identical(sum(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3)),
##                as.nanoduration(6))
expect_identical(sum(c(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3))),
                 as.nanoduration(6))                

##test_min <- function() {
## error in underlying bit64: LLL
## expect_identical(min(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3)),
##                as.nanoduration(1))
expect_identical(min(c(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3))),
                 as.nanoduration(1))                

##test_max <- function() {
## error in underlying bit64: LLL
## expect_identical(max(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3)),
##                as.nanoduration(3))
expect_identical(max(c(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3))),
                 as.nanoduration(3))                

##test_range <- function() {
## error in underlying bit64: LLL
## expect_identical(range(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3)),
##                c(as.nanoduration(1), as.nanoduration(3)))
expect_identical(range(c(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3))),
                 c(as.nanoduration(1), as.nanoduration(3)))            

##test_prod <- function() {
expect_error(prod(c(as.nanoduration(1), as.nanoduration(2), as.nanoduration(3))),
             "invalid 'type' \\(nanoduration\\) of argument")


## Complex
##test_Complex <- function() {
expect_error(Arg(as.nanoduration(1)), "non-numeric argument to function")


## Logic
##test_Logic <- function() {
expect_error(as.nanoduration(1) | as.nanoduration(1),
             "operations are possible only for numeric, logical or complex types")
expect_error(as.nanoduration(1) | "a",
             "operations are possible only for numeric, logical or complex types")
expect_error("a" & as.nanoduration(1),
             "operations are possible only for numeric, logical or complex types")


## Math
##test_abs <- function() {
expect_identical(abs(as.nanoduration(-1)), as.nanoduration(1))

##test_sign <- function() {
expect_identical(sign(as.nanoduration(-1)), as.integer64(-1))
expect_identical(sign(as.nanoduration(1)), as.integer64(1))

##test_Math <- function() {
expect_error(sqrt(as.nanoduration(1)), "non-numeric argument to mathematical function")


## Math2

##test_Math2 <- function() {
expect_error(round(as.nanoduration(1)), "non-numeric argument to mathematical function")


## Compare
##test_Compare_nanoduration_ANY <- function() {
expect_true(is.na(as.nanoduration(1) < "a")) # this is what "integer64" gives back, but do we want to change that to an error? LLL
## not quite clear in R either:
## > 1 < list(1)
## [1] FALSE
## > 1 > list(1)
## [1] FALSE
## > 1 == list(1)
## [1] TRUE  

##test_Compare_ANY_nanoduration <- function() {
expect_true(is.na("a" < as.nanoduration(1))) # this is what "integer64" gives back, but do we want to change that to an error? LLL


## Arith
##test_Arith <- function() {
expect_identical(as.nanoduration(4) %% 3, as.integer64(1))


## NA stuff
expect_true(is.na(as.nanoduration(as.integer(NA))))
expect_true(is.na(as.nanoduration(as.integer(NaN))))
d <- as.nanoduration(1:10)
is.na(d) <- 1:3
expect_true(all(is.na(d[1:3])))
expect_true(!any(is.na(d[4:10])))
expect_true(is.na(NA_nanoduration_))
expect_identical(is.na(c(a=NA_nanoduration_)), c(a=TRUE))

## test S4 conversions:
expect_identical(nanoduration(1,1,1,1), as("01:01:01.000_000_001", "nanoduration"))
expect_identical(as.nanoduration(as.integer64(hour)), as(hour, "nanoduration"))
expect_identical(as.nanoduration(hour), as(hour, "nanoduration"))
