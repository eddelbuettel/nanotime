
suppressMessages({
    library(nanotime)
    library(bit64)
})
options(digits=7)                       # needed for error message below

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
expect_identical(nanoduration(), as.nanoduration(NULL))
expect_identical(length(nanoduration()), 0L)
expect_identical(nanoduration(), as.nanoduration())


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
expect_identical(format(d), "01:01:01.000_000_001")
expect_identical(format(-d), "-01:01:01.000_000_001")
expect_true(is.na(format(as.nanoduration(as.integer64("-9223372036854775808")))))
expect_identical(as.character(d), "01:01:01.000_000_001")
expect_identical(as.character(-d), "-01:01:01.000_000_001")
expect_true(is.na(as.character(as.nanoduration(as.integer64("-9223372036854775808")))))
expect_stdout(show(d))
expect_stdout(show(-d))
expect_stdout(show(as.nanoduration(as.integer64("-9223372036854775808"))))

##test_print <- function() {
d <- nanoduration(1,1,1,1)
expect_stdout(print(d))
expect_stdout(print(nanoduration()))

##test_print_name <- function() {
d <- nanoduration(1,1,1,1)
names(d) <- "a"
expect_identical(format(d), c(a="01:01:01.000_000_001"))
expect_stdout(print(d))

##test_format <- function() {
d <- nanoduration(1,1,1,1)
expect_identical(format(d), "01:01:01.000_000_001")


## subset:
##test_subset_int <- function() {
dd <- as.nanoduration(1:10)
expect_identical(dd[1:2], as.nanoduration(1:2))
expect_identical(dd[-1:-5], as.nanoduration(6:10))
expect_identical(dd[c(0,0,0,0,0,1,2,3,4,5)], as.nanoduration(1:5))
expect_warning(dd[1, 2], "unused indices or arguments in 'nanoduration' subsetting")

##test_subset_logical <- function() {
dd <- as.nanoduration(1:4)
expect_identical(dd[c(T,F,F,F)], dd[1])
expect_identical(dd[c(F,T,F,F)], dd[2])
expect_identical(dd[c(F,F,T,F)], dd[3])
expect_identical(dd[c(F,F,F,T)], dd[4])
expect_identical(dd[TRUE], dd)
expect_identical(dd[c(F,T,T,F)], dd[2:3])
expect_warning(dd[T, F], "unused indices or arguments in 'nanoduration' subsetting")
expect_identical(dd[c(F,F,F,NA)], NA_nanoduration_)

##test_subset_character <- function() {
dd <- c(x=as.nanoduration(1), y=as.nanoduration(2))
expect_identical(dd["x"], c(x=as.nanoduration(1)))
expect_identical(dd["y"], c(y=as.nanoduration(2)))
expect_warning(dd["x", "y"], "unused indices or arguments in 'nanoduration' subsetting")

## also check out of bound subset
res <- as.nanoduration(as.integer64(NA))
names(res) <- NA
expect_identical(dd["a"], res)
expect_warning(dd["a", "b"], "unused indices or arguments in 'nanoduration' subsetting")

## subset incorrect index type
expect_error(dd[as.integer64(1)], "']' not defined on 'nanoduration' for index of type 'ANY'")

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
expect_identical(as.nanoduration(6) / as.nanoduration(2), as.integer64(3))


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
expect_error(as.nanoduration(1) < "a", "cannot parse nanoduration")
## not quite clear in R:
## > 1 < list(1)
## [1] FALSE
## > 1 > list(1)
## [1] FALSE
## > 1 == list(1)
## [1] TRUE  

##test_Compare_ANY_nanoduration <- function() {
expect_error("a" < as.nanoduration(1), "cannot parse nanoduration")

##test_Compare_character_nanoduration <- function() {
expect_true("12:13:14.151617" == as.nanoduration("12:13:14.151617"))
expect_false("12:13:14.151617" == as.nanoduration("12:13:14.151617001"))
expect_true("12:13:14.151617" != as.nanoduration("12:13:14.151617001"))
expect_false("12:13:14.151617" != as.nanoduration("12:13:14.151617"))
expect_true("12:13:14.151617" < as.nanoduration("12:13:14.151617001"))
expect_false("12:13:14.151617001" < as.nanoduration("12:13:14.151617"))
expect_true("12:13:14.151617" <= as.nanoduration("12:13:14.151617"))
expect_false("12:13:14.151617001" <= as.nanoduration("12:13:14.151617"))
expect_true("12:13:14.151617001" > as.nanoduration("12:13:14.151617"))
expect_false("12:13:14.151617" > as.nanoduration("12:13:14.151617001"))
expect_true("12:13:14.151617" >= as.nanoduration("12:13:14.151617"))
expect_false("12:13:14.151617" >= as.nanoduration("12:13:14.151617001"))
 
##test_Compare_nanoduration_character <- function() {
expect_true(as.nanoduration("12:13:14.151617") == "12:13:14.151617")
expect_false(as.nanoduration("12:13:14.151617") == "12:13:14.151617001")
expect_true(as.nanoduration("12:13:14.151617") != "12:13:14.151617001")
expect_false(as.nanoduration("12:13:14.151617") != "12:13:14.151617")
expect_true(as.nanoduration("12:13:14.151617") < "12:13:14.151617001")
expect_false(as.nanoduration("12:13:14.151617001") < "12:13:14.151617")
expect_true(as.nanoduration("12:13:14.151617") <= "12:13:14.151617")
expect_false(as.nanoduration("12:13:14.151617001") <= "12:13:14.151617")
expect_true(as.nanoduration("12:13:14.151617001") > "12:13:14.151617")
expect_false(as.nanoduration("12:13:14.151617") > "12:13:14.151617001")
expect_true(as.nanoduration("12:13:14.151617") >= "12:13:14.151617")
expect_false(as.nanoduration("12:13:14.151617") >= "12:13:14.151617001")

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

## test seq:
expect_identical(seq(as.nanoduration("00:00:00"), by=as.nanoduration("00:00:01"), length.out=10),
                 as.nanoduration(seq(0, by=1e9, length.out=10)))
expect_identical(seq(as.nanoduration("00:00:00"), to=as.nanoduration("00:00:09"), length.out=10),
                 as.nanoduration(seq(0, by=1e9, length.out=10)))
expect_identical(seq(as.nanoduration("00:00:00"), to=as.nanoduration("00:00:09"), along.with=1:10),
                 as.nanoduration(seq(0, by=1e9, along.with=1:10)))

## all.equal
##test_all.equal_nanotime_any <- function() {
expect_true(!isTRUE(all.equal(nanotime(1), "a")))

##test_all.equal_any_nanotime <- function() {
expect_true(!isTRUE(all.equal("a", nanotime(1))))

## all.equal
expect_true(isTRUE(all.equal(as.nanoduration(1), as.nanoduration(1))))
expect_true(!isTRUE(all.equal(as.nanoduration(1), as.nanoduration(2))))  
expect_identical(all.equal(as.nanoduration(1), as.nanoduration(2)), "Mean relative difference: 1")  
expect_identical(all.equal(as.nanoduration(c(1L,NA)), as.nanoduration(1:2)), "'is.NA' value mismatch: 0 in current 1 in target")

expect_error(all.equal(as.nanoduration(1), as.nanoduration(1), tolerance="1"), "'tolerance' should be numeric")
expect_error(all.equal(as.nanoduration(1), as.nanoduration(1), scale="a"), "'scale' should be numeric or NULL")
expect_error(all.equal(as.nanoduration(1), as.nanoduration(1), check.attributes="a"), "'check.attributes' must be logical")
expect_error(all.equal(as.nanoduration(1), as.nanoduration(2), scale=-1), "all\\(scale > 0\\) is not TRUE")

expect_false(isTRUE(all.equal(as.nanoduration(1), as.nanoperiod("1d"))))
expect_identical(all.equal(as.nanoduration(1), as.nanoduration(1:2)), "Numeric: lengths (1, 2) differ")
expect_identical(all.equal(as.nanoduration(c(1,2,3)), as.nanoduration(c(1,1,2)), countEQ=TRUE), "Mean relative difference: 0.3333333")
expect_false(isTRUE(all.equal(as.nanoduration(1), 1i)))
expect_identical(all.equal(as.nanoduration(1), as.nanoduration(3), tolerance=1), "Mean absolute difference: 2")
expect_identical(all.equal(as.nanoduration(1), as.nanoduration(2e9), scale=1e9), "Mean scaled difference: 2")
expect_identical(all.equal(as.nanoduration(1), as.nanoduration(2e9), scale=1), "Mean absolute difference: 2e+09")

## test rounding functions:

## nano_ceiling:
## hours:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 UTC"), as.nanoduration("06:00:00")),
                 as.nanotime("2010-10-10T12:00:00+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 UTC"), as.nanoduration("01:00:00")),
                 as.nanotime("2010-10-10T12:00:00+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("06:00:00")),
                 as.nanotime("2010-10-10T18:00:00+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("07:00:00")),
                 as.nanotime("2010-10-10T19:00:00+00:00"))
expect_identical(nano_floor(as.nanotime("1940-12-12 00:12:23 UTC"), as.nanoduration("01:00:00")),
                 as.nanotime("1940-12-12T00:00:00+00:00"))
expect_identical(nano_ceiling(as.nanotime("1970-01-01 UTC"), as.nanoduration("06:00:00")),
                 as.nanotime("1970-01-01T00:00:00+00:00"))
expect_identical(nano_ceiling(as.nanotime("1970-01-01 UTC"), as.nanoduration("01:30:00")),
                 as.nanotime("1970-01-01T00:00:00+00:00"))

expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"),
                              as.nanoduration("07:00:00"),
                              origin = as.nanotime("2010-10-10 10:23:12 UTC")),
                 as.nanotime("2010-10-10T17:23:12+00:00"))

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("07:00:00"), origin = as.nanotime(1:10)),
             "'origin' must be scalar")

                 
expect_error(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("-06:00:00")),
             "'precision' must be strictly positive")

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("-06:00:00"), origin="wrong type"),
             "'origin' must be of class 'nanotime'")

## minutes:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("00:05:00")),
                 as.nanotime("2010-10-10T12:25:00+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("00:01:00")),
                 as.nanotime("2010-10-10T12:24:00+00:00"))

expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"),
                              as.nanoduration("00:07:00"),
                              origin = as.nanotime("2010-10-10 12:22:12 UTC")),
                 as.nanotime("2010-10-10T12:29:12+00:00"))

## seconds:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("00:00:05")),
                 as.nanotime("2010-10-10T12:23:25+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123 UTC"), as.nanoduration("00:00:01")),
                 as.nanotime("2010-10-10T12:23:24+00:00"))

## milliseconds
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.001")),
                 as.nanotime("2010-10-10T12:23:23.124+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.010")),
                 as.nanotime("2010-10-10T12:23:23.13+00:00"))

## microseconds
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000001")),
                 as.nanotime("2010-10-10T12:23:23.123457+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000010")),
                 as.nanotime("2010-10-10T12:23:23.12346+00:00"))

## nanoseconds
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000000001")),
                 as.nanotime("2010-10-10T12:23:23.123456789+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000000010")),
                 as.nanotime("2010-10-10T12:23:23.123456790+00:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000000033")),
                 as.nanotime("2010-10-10T12:23:23.123456814+00:00"))


## nano_floor

## hours:
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("06:00:00")),
                 as.nanotime("2010-10-10T12:00:00+00:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("07:00:00")),
                 as.nanotime("2010-10-10T12:00:00+00:00"))
expect_identical(nano_floor(as.nanotime("1970-01-01 UTC"), as.nanoduration("06:00:00")),
                 as.nanotime("1970-01-01T00:00:00+00:00"))
expect_identical(nano_floor(as.nanotime("1970-01-01 UTC"), as.nanoduration("01:30:00")),
                 as.nanotime("1970-01-01T00:00:00+00:00"))
expect_identical(nano_floor(as.nanotime("1970-01-01 06:00:00 UTC"), as.nanoduration("06:00:00")),
                 as.nanotime("1970-01-01T06:00:00+00:00"))
expect_identical(nano_floor(as.nanotime("1970-01-01 07:00:00 UTC"), as.nanoduration("07:00:00")),
                 as.nanotime("1970-01-01T07:00:00+00:00"))

expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"),
                            as.nanoduration("07:00:00"),
                            origin = as.nanotime("2010-10-10 10:23:12 UTC")),
                 as.nanotime("2010-10-10T10:23:12+00:00"))

expect_error(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("07:00:00"), origin = as.nanotime(1:10)),
             "'origin' must be scalar")

expect_error(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("-06:00:00")),
             "'precision' must be strictly positive")

expect_error(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("-06:00:00"), origin="wrong type"),
             "'origin' must be of class 'nanotime'")

## minutes:
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("00:05:00")),
                 as.nanotime("2010-10-10T12:20:00+00:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("00:01:00")),
                 as.nanotime("2010-10-10T12:23:00+00:00"))

## seconds:
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoduration("00:00:05")),
                 as.nanotime("2010-10-10T12:23:20+00:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123 UTC"), as.nanoduration("00:00:01")),
                 as.nanotime("2010-10-10T12:23:23+00:00"))

## milliseconds
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.001")),
                 as.nanotime("2010-10-10T12:23:23.123+00:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.010")),
                 as.nanotime("2010-10-10T12:23:23.12+00:00"))

## microseconds
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000001")),
                 as.nanotime("2010-10-10T12:23:23.123456+00:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000010")),
                 as.nanotime("2010-10-10T12:23:23.12345+00:00"))

## nanoseconds
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000000001")),
                 as.nanotime("2010-10-10T12:23:23.123456789+00:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000000010")),
                 as.nanotime("2010-10-10T12:23:23.12345678+00:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 UTC"), as.nanoduration("00:00:00.000000033")),
                 as.nanotime("2010-10-10T12:23:23.123456781+00:00"))

## rep
expect_identical(rep(as.nanoduration(1), 2), as.nanoduration(rep(1,2)))
expect_identical(rep(as.nanoduration(1:2), each=2), as.nanoduration(rep(1:2, each=2)))
