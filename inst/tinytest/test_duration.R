
library(nanotime)
library(bit64)

milli <- 1e6
sec <- 1e9
minute <- 60 * sec
hour <- 60 * minute

## constructors
##test_duration <- function() {
expect_identical(duration(1,1,1,1), as.duration("01:01:01.000_000_001"))
## R recycling:
expect_identical(duration(1:2,1,1,1), c(as.duration("01:01:01.000_000_001"),
                                        as.duration("02:01:01.000_000_001")))

##test_as.duration_character_and_numeric <- function() {
expect_identical(as.duration("1:00:00"), as.duration(hour))
expect_identical(as.duration("-1:00:00"), -as.duration(hour))
expect_identical(as.duration("0:01:00"), as.duration(minute))
expect_identical(as.duration("0:00:01"), as.duration(sec))
expect_identical(as.duration("0:00:00.001"), as.duration(milli))
expect_identical(c(as.duration("1:00:00"), c(as.duration("2:00:00"))),
                 c(as.duration(hour), as.duration(2*hour)))
expect_identical(as.duration(NA_integer64_), as.duration(as.integer64("-9223372036854775808")))

expect_error(as.duration("10:aa"), "cannot parse duration")
expect_error(as.duration("1000a"), "cannot parse duration")
expect_error(as.duration("10:10:10.1000a"), "cannot parse duration")
expect_error(as.duration("a"), "cannot parse duration")
expect_error(as.duration(""), "cannot parse duration")

##test_as.duration_integer64 <- function() {
expect_identical(as.duration(as.integer64(hour)), as.duration(hour))
expect_identical(as.duration(c(a=as.integer64(hour), b=as.integer64(2*hour))),
                 as.duration(c(a=hour, b=2*hour)))

##test_as.duration_integer <- function() {
expect_identical(as.duration(as.integer(sec)), as.duration(sec))
expect_identical(as.duration(c(a=as.integer(milli), b=as.integer(2*milli))),
                 as.duration(c(a=milli, b=2*milli)))

##test_as.integer64 <- function() {
expect_identical(as.integer64(as.duration(hour)), as.integer64(hour))
expect_identical(as.integer64(as.duration(1:1000)), as.integer64(1:1000))



## show/print/format
##test_show <- function() {
d <- duration(1,1,1,1)
expect_identical(show(d), "01:01:01.000_000_001")
expect_identical(show(-d), "-01:01:01.000_000_001")
expect_identical(show(as.duration(as.integer64("-9223372036854775808"))), "NA")

##test_print <- function() {
d <- duration(1,1,1,1)
expect_identical(print(d), "01:01:01.000_000_001")

##test_print_name <- function() {
d <- duration(1,1,1,1)
names(d) <- "a"
expect_identical(print(d), c(a="01:01:01.000_000_001"))

##test_format <- function() {
d <- duration(1,1,1,1)
expect_identical(format(d), "01:01:01.000_000_001")


## subset:
##test_subset_int <- function() {
dd <- as.duration(1:10)
expect_identical(dd[1], as.duration(1))

##test_subset_logical <- function() {
dd <- as.duration(1:4)
expect_identical(dd[c(T,F,F,F)], dd[1])
expect_identical(dd[c(F,T,F,F)], dd[2])
expect_identical(dd[c(F,F,T,F)], dd[3])
expect_identical(dd[c(F,F,F,T)], dd[4])
expect_identical(dd[TRUE], dd)
expect_identical(dd[c(F,T,T,F)], dd[2:3])

##test_subset_character <- function() {
dd <- c(x=as.duration(1), y=as.duration(2))
expect_identical(dd["x"], c(x=as.duration(1)))
expect_identical(dd["y"], c(y=as.duration(2)))
## also check out of bound subset
res <- as.duration(as.integer64(NA))
names(res) <- NA
## LLL underlying issue with 'integer64':
## expect_identical(dd["a"], res)


## subassign:
##test_subassign_logical <- function() {
x <- as.duration(1:10)
x[c(T,T,T,F,F,F,F,F,F,F)] <- as.duration(2:4)
expect_identical(x, as.duration(c(2:4, 4:10)))

##test_subassign_numeric <- function() {
x <- as.duration(1:10)
x[1:3] <- as.duration(2:4)
x[4:10] <- as.duration(5:11)
expect_identical(x, as.duration(2:11))

##test_subsassign_character <- function() {
dd <- c(a=as.duration(1), b=as.duration(2), c=as.duration(3), d=as.duration(4))
dd[c("b", "c")] <- as.duration(20:21)
expected <- c(a=as.duration(1), b=as.duration(20), c=as.duration(21), d=as.duration(4))
expect_identical(dd, expected)


##test_square_bracket <- function() {
dd <- c(a=as.duration(1), b=as.duration(2), c=as.duration(3), d=as.duration(4))
dd_nonames <- as.duration(1:4)
expect_identical(dd_nonames[1], dd[[1]])
expect_identical(dd_nonames[2], dd[[2]])
expect_identical(dd_nonames[3], dd[[3]])


## test names
##test_get_names <- function() {
dd <- c(a=as.duration(1), b=as.duration(2), c=as.duration(3), d=as.duration(4))
expect_identical(names(dd), c("a","b","c","d"))

##test_set_names <- function() {
names <- c("a","b","c","d")
dd <- as.duration(1:4)
names(dd) <- names
expect_identical(names(dd), names)
names(dd)[1] <- "x"
expect_identical(names(dd), c("x","b","c","d"))


## test scalar/vector mix LLL

## ops
## -
##test_duration_minus_duration <- function() {
expect_identical(as.duration("1:00:00") - as.duration("00:01:00"), as.duration("00:59:00"))
expect_identical(as.duration("-1:00:00") - as.duration("00:01:00"), as.duration("-01:01:00"))
expect_identical(as.duration("1:00:00.1") - as.duration("1:00:00"), as.duration("0.1"))
expect_identical(as.duration(1:10) - as.duration(0:9), as.duration(rep(1, 10)))
expect_identical(as.duration(1:10) - as.duration(1), as.duration(0:9))
expect_identical(c(a=as.duration(1), b=as.duration(1)) - as.duration(1),
                 as.duration(c(a=0, b=0)))

##test_duration_minus_numeric <- function() {
expect_identical(as.duration("1:00:00") - minute, as.duration("00:59:00"))
expect_identical(as.duration(1:10) - 0.0:9, as.duration(rep(1, 10)))
expect_identical(as.duration(1:10) - 1, as.duration(0:9))
expect_identical(c(a=as.duration(1), b=as.duration(1)) - 1, as.duration(c(a=0, b=0)))

##test_duration_minus_integer <- function() {
expect_identical(as.duration("1:00:00") - as.integer(milli), as.duration("00:59:59.999"))
expect_identical(as.duration(1:10) - 0:9, as.duration(rep(1, 10)))
expect_identical(as.duration(1:10) - as.integer(1), as.duration(0:9))
expect_identical(c(a=as.duration(1), b=as.duration(1)) - as.integer(1),
                 as.duration(c(a=0, b=0)))

##test_duration_minus_integer64 <- function() {
expect_identical(as.duration("1:00:00") - as.integer64(minute), as.duration("00:59:00"))
expect_identical(as.duration(1:10) - as.integer64(0:9), as.duration(rep(1, 10)))
expect_identical(as.duration(1:10) - as.integer64(1), as.duration(0:9))
expect_identical(c(a=as.duration(1), b=as.duration(1)) - c(c=as.integer64(1)),
                 as.duration(c(a=0, b=0)))

##test_integer64_minus_duration <- function() {
expect_identical(as.integer64(hour) - as.duration("1:00:00"), as.duration("0:00:00"))

##test_duration_minus_integer64 <- function() {
expect_identical(as.duration("1:00:00") - as.integer64(hour), as.duration("0:00:00"))

##test_numeric_minus_duration <- function() {
expect_identical(hour - as.duration("1:00:00"), as.duration("0:00:00"))

##test_integer_minus_duration <- function() {
expect_identical(as.integer(1e9) - as.duration("1:00:00"), as.duration("-00:59:59"))

## LLL name issue here due do to underlying issues with 'bit64'
## ##test_numeric_minus_duration <- function() {
##     expect_identical(2 - c(a=as.duration(1), b=as.duration(2)),
##                    c(a=as.duration(1), b=as.duration(0)))
##     expect_identical(2 - c(a=as.integer64(1), b=as.integer64(2)),
##                    c(a=as.integer64(1), b=as.integer64(0)))

## 
##test_any_minus_duration <- function() {
expect_error("a" - as.duration("1:00:00"), "invalid operand types")

##test_duration_minus_any <- function() {
expect_error(as.duration("1:00:00") - "a", "invalid operand types")


## +
##test_duration_plus_duration <- function() {
expect_identical(as.duration("1:00:00") + as.duration("00:01:00"), as.duration("01:01:00"))
expect_identical(as.duration("-1:00:00") + as.duration("00:01:00"), as.duration("-00:59:00"))
expect_identical(as.duration("-1:00:00.1") + as.duration("1:00:00"), as.duration("-0.1"))
expect_identical(-as.duration(1:10) + as.duration(0:9), as.duration(rep(-1, 10)))
expect_identical(as.duration(1:10) + as.duration(1), as.duration(2:11))
expect_identical(c(a=as.duration(1), b=as.duration(1)) + as.duration(1),
                 as.duration(c(a=2, b=2)))

##test_integer64_plus_duration <- function() {
expect_identical(as.integer64(hour) + as.duration("1:00:00"), as.duration("2:00:00"))

##test_duration_plus_integer64 <- function() {
expect_identical(as.duration("1:00:00") + as.integer64(hour), as.duration("2:00:00"))

##test_duration_plus_numeric <- function() {
expect_identical(as.duration("1:00:00") + hour, as.duration("2:00:00"))

##test_numeric_plus_duration <- function() {
expect_identical(hour + as.duration("1:00:00"), as.duration("2:00:00"))

##test_character_plus_duration <- function() {
expect_error("hello" + as.duration("1:00:00"), "invalid operand types")

##test_any_plus_duration <- function() {
expect_error("a" + as.duration("1:00:00"), "invalid operand types")

##test_duration_plus_any <- function() {
expect_error(as.duration("1:00:00") + "a", "invalid operand types")


## *
##test_duration_times_numeric <- function() {
expect_identical(as.duration("00:01:00") * 3, as.duration("00:03:00"))

##test_numeric_times_duration <- function() {
expect_identical(3 * as.duration("00:01:00"), as.duration("00:03:00"))

##test_duration_times_integer64 <- function() {
expect_identical(as.duration("00:01:00") * as.integer64(3), as.duration("00:03:00"))

##test_duration_times_duration <- function() {
expect_error(as.duration(1) * as.duration(2), "invalid operand types")

## LLL have to fix this, dependent also on 'integer64' fixes...
## ##test_numeric_times_duration <- function() {
##     expect_identical(1.5 * as.duration("00:01:00"), as.duration("00:01:30"))
## 
##test_numeric_times_duration <- function() {
expect_identical(3 * as.duration("00:01:00"), as.duration("00:03:00"))

##test_integer64_times_duration <- function() {
expect_identical(as.integer64(3) * as.duration("00:01:00"), as.duration("00:03:00"))

##test_any_times_duration <- function() {
expect_error("hello" * as.duration("00:01:00"), "invalid operand types")

##test_duration_times_character <- function() {
expect_error(as.duration("00:01:00") *  "hello", "invalid operand types")

## /
##test_duration_div_numeric <- function() {
expect_identical(as.duration("00:03:00") / 3, as.duration("00:01:00"))

##test_duration_div_integer64 <- function() {
expect_identical(as.duration("00:03:00") / as.integer64(3), as.duration("00:01:00"))

##test_duration_div_integer <- function() {
expect_identical(as.duration("00:03:00") / as.integer(3), as.duration("00:01:00"))

##test_numeric_div_duration <- function() {
expect_error(1.5 / as.duration("00:01:00"), "invalid operand types")

##test_duration_div_any <- function() {
expect_error(as.duration("1:00:00") / "a", "invalid operand types")

##test_duration_div_duration <- function() {
expect_error(as.duration(1) / as.duration(2), "invalid operand types")


## unary
##test_unary_minus <- function() {
expect_identical(-as.duration("00:01:00"), as.duration("-00:01:00"))


##test_unary_plus <- function() {
expect_identical(+as.duration("00:01:00"), as.duration("00:01:00"))


## Summary
##test_sum <- function() {
## error in underlying bit64: LLL
## expect_identical(sum(as.duration(1), as.duration(2), as.duration(3)),
##                as.duration(6))
expect_identical(sum(c(as.duration(1), as.duration(2), as.duration(3))),
                 as.duration(6))                

##test_min <- function() {
## error in underlying bit64: LLL
## expect_identical(min(as.duration(1), as.duration(2), as.duration(3)),
##                as.duration(1))
expect_identical(min(c(as.duration(1), as.duration(2), as.duration(3))),
                 as.duration(1))                

##test_max <- function() {
## error in underlying bit64: LLL
## expect_identical(max(as.duration(1), as.duration(2), as.duration(3)),
##                as.duration(3))
expect_identical(max(c(as.duration(1), as.duration(2), as.duration(3))),
                 as.duration(3))                

##test_range <- function() {
## error in underlying bit64: LLL
## expect_identical(range(as.duration(1), as.duration(2), as.duration(3)),
##                c(as.duration(1), as.duration(3)))
expect_identical(range(c(as.duration(1), as.duration(2), as.duration(3))),
                 c(as.duration(1), as.duration(3)))            

##test_prod <- function() {
expect_error(prod(c(as.duration(1), as.duration(2), as.duration(3))),
             "invalid 'type' \\(duration\\) of argument")


## Complex
##test_Complex <- function() {
expect_error(Arg(as.duration(1)), "non-numeric argument to function")


## Logic
##test_Logic <- function() {
expect_error(as.duration(1) | as.duration(1),
             "operations are possible only for numeric, logical or complex types")
expect_error(as.duration(1) | "a",
             "operations are possible only for numeric, logical or complex types")
expect_error("a" & as.duration(1),
             "operations are possible only for numeric, logical or complex types")


## Math
##test_abs <- function() {
expect_identical(abs(as.duration(-1)), as.duration(1))

##test_sign <- function() {
expect_identical(sign(as.duration(-1)), as.integer64(-1))
expect_identical(sign(as.duration(1)), as.integer64(1))

##test_Math <- function() {
expect_error(sqrt(as.duration(1)), "non-numeric argument to mathematical function")


## Math2

##test_Math2 <- function() {
expect_error(round(as.duration(1)), "non-numeric argument to mathematical function")


## Compare
##test_Compare_duration_ANY <- function() {
expect_true(is.na(as.duration(1) < "a")) # this is what "integer64" gives back, but do we want to change that to an error? LLL
## not quite clear in R either:
## > 1 < list(1)
## [1] FALSE
## > 1 > list(1)
## [1] FALSE
## > 1 == list(1)
## [1] TRUE  

##test_Compare_ANY_duration <- function() {
expect_true(is.na("a" < as.duration(1))) # this is what "integer64" gives back, but do we want to change that to an error? LLL


## Arith
##test_Arith <- function() {
expect_identical(as.duration(4) %% 3, as.integer64(1))
