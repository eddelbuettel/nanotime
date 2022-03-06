
suppressMessages({
    library(nanotime)
    library(bit64)
})

isSolaris <- Sys.info()[["sysname"]] == "SunOS"

## constructors
##test_as.nanoperiod_character <- function() {
p1 <- as.nanoperiod("1m1d")
expect_identical(nanoperiod.day(p1), 1)
expect_identical(nanoperiod.month(p1), 1)
expect_identical(nanoperiod.nanoduration(p1), as.nanoduration(0))

expect_identical(as.nanoperiod("1y"), as.nanoperiod("12m"))
expect_identical(as.nanoperiod("2y"), as.nanoperiod("24m"))
expect_identical(as.nanoperiod("1w"), as.nanoperiod("7d"))
expect_identical(as.nanoperiod("2w"), as.nanoperiod("14d"))

p2 <- as.nanoperiod("2m1d/00:01:01.1")
expect_identical(nanoperiod.month(p2), 2)
expect_identical(nanoperiod.day(p2), 1)
expect_identical(nanoperiod.nanoduration(p2), as.nanoduration("00:01:01.1"))

p3 <- as.nanoperiod(c("1d","2d","3d"))
expect_identical(nanoperiod.day(p3[1]), 1)
expect_identical(nanoperiod.day(p3[2]), 2)
expect_identical(nanoperiod.day(p3[3]), 3)    

expect_identical(nanoperiod(), as.nanoperiod(NULL))
expect_identical(nanoperiod(), as.nanoperiod())
expect_identical(length(nanoperiod()), 0L)
expect_identical(length(as.nanoperiod(NULL)), 0L)

## check year and week
expect_identical(as.character(as.nanoperiod("1y/00:01:01")), "12m0d/00:01:01")
expect_identical(as.character(as.nanoperiod("1w/00:01:01")), "0m7d/00:01:01")
expect_identical(names(as.nanoperiod(c(a="1m"))), "a")

## check negative duration
expect_identical(as.nanoperiod("-00:00:01"), as.nanoperiod(as.nanoduration("-00:00:01")))
expect_identical(as.nanoperiod("-0:00:01"), as.nanoperiod(as.nanoduration("-00:00:01")))

##test_as.nanoperiod_character_error <- function() {
expect_error(as.nanoperiod("1wm00:01:01"), "cannot parse nanoperiod")
expect_error(as.nanoperiod("1ym00:01:01"), "cannot parse nanoperiod")
expect_error(as.nanoperiod("1dm00:01:01"), "cannot parse nanoperiod")

##test_as.nanoperiod_integer64 <- function() {
p1 <- as.nanoperiod(as.integer64(1:10))
expect_identical(nanoperiod.nanoduration(p1), as.nanoduration(1:10))
p1 <- as.nanoperiod(c(a=as.integer64(1)))
expect_identical(nanoperiod.nanoduration(p1), as.nanoduration(c(a=1)))

##test_as.nanoperiod_integer <- function() {
p1 <- as.nanoperiod(1:10)
expect_identical(nanoperiod.nanoduration(p1), as.nanoduration(1:10))
p1 <- as.nanoperiod(c(a=1L))
expect_identical(nanoperiod.nanoduration(p1), as.nanoduration(c(a=1)))

##test_as.nanoperiod_numeric <- function() {
p1 <- as.nanoperiod(as.numeric(1:10))
expect_identical(nanoperiod.nanoduration(p1), as.nanoduration(1:10))
p1 <- as.nanoperiod(c(a=1))
expect_identical(nanoperiod.nanoduration(p1), c(a=as.nanoduration(1)))

##test_as.nanoperiod_nanoduration <- function() {
p1 <- as.nanoperiod(c(a=as.nanoduration(1)))
expect_identical(p1, c(a=as.nanoperiod("00:00:00.000_000_001")))

##test_nanoperiod <- function() {
expect_identical(nanoperiod(0,0,1), as.nanoperiod(1))
expect_identical(nanoperiod(0,0,1:10), as.nanoperiod(1:10))
expect_identical(nanoperiod(1,1,0:9), as.nanoperiod(paste0("1m1d/00:00:00.000_000_00", 0:9)))

expect_error(nanoperiod("a"), "argument 'months' must be numeric")
expect_error(nanoperiod(1, "a"), "argument 'days' must be numeric")


## accessors:
##test_nanoperiod.day <- function() {
p1 <- as.nanoperiod(paste0(1:10, "d"))
names(p1) <- 1:10
expected  <- as.numeric(1:10)
names(expected)  <- 1:10
expect_identical(nanoperiod.day(p1), expected)

##test_nanoperiod.month <- function() {
p1 <- as.nanoperiod(paste0(1:10, "m"))
names(p1) <- 1:10
expected  <- as.numeric(1:10)
names(expected)  <- 1:10
expect_identical(nanoperiod.month(p1), expected)

##test_nanoperiod.nanoduration <- function() {
p1 <- as.nanoperiod(1:10)
names(p1) <- 1:10
expected  <- as.numeric(1:10)
names(expected)  <- 1:10
expect_identical(nanoperiod.nanoduration(p1), as.nanoduration(expected))


## show/print/as.character/format
##test_show <- function() {
p1 <- format(as.nanoperiod("1m1d/1:00:00.1"))
expect_identical(p1, "1m1d/01:00:00.100")
expect_stdout(show(as.nanoperiod("1m1d/1:00:00.1")))

##test_print <- function() {
p1 <- format(as.nanoperiod("2m2d/2:02:02.2"))
expect_identical(p1, "2m2d/02:02:02.200")
expect_stdout(print(nanoperiod()))

##test_as.character <- function() {
p1 <- as.character(as.nanoperiod("2m2d/2:02:02.20001"))
expect_identical(p1, "2m2d/02:02:02.200_010")
expect_identical(as.character(NA_nanoperiod_), NA_character_)

##test_as.character_named <- function() {
p1 <- as.character(c(a=as.nanoperiod("2m2d/2:02:02.20001"),
                     b=as.nanoperiod("2m2d/2:02:02.20002")))
expect_identical(p1, c(a="2m2d/02:02:02.200_010", b="2m2d/02:02:02.200_020"))

##test_format <- function() {
p1 <- as.nanoperiod("2m2d/2:02:02.20001")
expect_identical(format(p1), "2m2d/02:02:02.200_010")
expect_identical(as.character(p1), "2m2d/02:02:02.200_010")


## subset:
##test_subset_int <- function() {
p1 <- as.nanoperiod("1m1d/00:00:01")
p2 <- as.nanoperiod("2m2d/00:00:02") 
p3 <- as.nanoperiod("3m3d/00:00:03") 
p4 <- as.nanoperiod("4m4d/00:00:04") 
pp <- c(p1, p2, p3, p4)
expect_identical(pp[1], p1)
expect_identical(pp[2], p2)
expect_identical(pp[3], p3)
expect_identical(pp[4], p4)
expect_identical(pp[1:4], pp)
expect_identical(pp[2:3], c(p2,p3))
expect_identical(pp[-1:-2], c(p3,p4))
expect_warning(pp[1, 2], "unused indices or arguments in 'nanoperiod' subsetting")

##test_subset_logical <- function() {
p1 <- as.nanoperiod("1m1d/00:00:01")
p2 <- as.nanoperiod("2m2d/00:00:02") 
p3 <- as.nanoperiod("3m3d/00:00:03") 
p4 <- as.nanoperiod("4m4d/00:00:04") 
pp <- c(p1, p2, p3, p4)
expect_identical(pp[c(T,F,F,F)], p1)
expect_identical(pp[c(F,T,F,F)], p2)
expect_identical(pp[c(F,F,T,F)], p3)
expect_identical(pp[c(F,F,F,T)], p4)
expect_identical(pp[TRUE], pp)
expect_identical(pp[c(F,T,T,F)], c(p2,p3))
expect_identical(pp[c(NA, F, F, F)], as.nanoperiod(NA_integer_))
expect_identical(pp[c(NA, F, F, F)], NA_nanoperiod_)
expect_warning(pp[T, F], "unused indices or arguments in 'nanoperiod' subsetting")

##test_subset_character <- function() {
pp <- c(x=as.nanoperiod(1), y=as.nanoperiod(2))
expect_identical(pp["x"], c(x=as.nanoperiod(1)))
expect_identical(pp["y"], c(y=as.nanoperiod(2)))
res  <- NA_nanoperiod_
names(res)  <- NA_character_
expect_identical(pp["a"], res)
expect_warning(pp["a", "b"], "unused indices or arguments in 'nanoperiod' subsetting")

## test subset incorrect type
expect_error(pp[as.integer64(1)], "']' not defined on 'nanoperiod' for index of type 'ANY'")

## subassign
##test_subassign_logical <- function() {
x <- as.nanoperiod(1:10)
x[c(T,T,T,F,F,F,F,F,F,F)] <- as.nanoperiod(2:4)
expect_identical(x, as.nanoperiod(c(2:4, 4:10)))
x <- as.nanoperiod(paste0(1:10, "d"))
x[c(T,T,T,F,F,F,F,F,F,F)] <- as.nanoperiod(paste0(2:4, "d"))
expect_identical(x, as.nanoperiod(paste0(c(2:4, 4:10), "d")))

##test_subassign_numeric <- function() {
x <- as.nanoperiod(1:10)
x[1:3] <- as.nanoperiod(2:4)
x[4:10] <- as.nanoperiod(5:11)
expect_identical(x, as.nanoperiod(2:11))
x <- as.nanoperiod(paste0(1:10, "d"))
x[1:3] <- as.nanoperiod(paste0(2:4, "d"))
x[4:10] <- as.nanoperiod(paste0(5:11, "d"))
expect_identical(x, as.nanoperiod(paste0(2:11, "d")))

##test_subsassign_character <- function() {
pp <- c(a=as.nanoperiod(1), b=as.nanoperiod(2), c=as.nanoperiod(3), d=as.nanoperiod(4))
pp[c("b", "c")] <- as.nanoperiod(20:21)
expected <- c(a=as.nanoperiod(1), b=as.nanoperiod(20), c=as.nanoperiod(21), d=as.nanoperiod(4))
expect_identical(pp, expected)

##test_square_bracket <- function() {
pp <- c(a=as.nanoperiod(1), b=as.nanoperiod(2), c=as.nanoperiod(3), d=as.nanoperiod(4))
pp_nonames <- as.nanoperiod(1:4)
expect_identical(pp_nonames[1], pp[[1]])
expect_identical(pp_nonames[2], pp[[2]])
expect_identical(pp_nonames[3], pp[[3]])


## ops
## -
##test_nanoperiod_minus_nanoperiod <- function() {
expect_identical(-as.nanoperiod("2m2d/00:00:02"), as.nanoperiod("-2m-2d/-00:00:02"))
expect_identical(as.nanoperiod("2m2d") - as.nanoperiod("1m1d"), as.nanoperiod("1m1d"))
expect_identical(as.nanoperiod("-1m-1d/-00:00:01") - as.nanoperiod("1m1d/00:00:01"),
               as.nanoperiod("-2m-2d/-00:00:02"))

##test_nanoperiod_minus_nanoperiod_vector <- function() {
expect_identical(as.nanoperiod("1d") - as.nanoperiod(paste0(1:10, "d")),
               as.nanoperiod(paste0(0:-9,"d")))
expect_identical(as.nanoperiod(paste0(1:10, "d")) - as.nanoperiod("1d"),
               as.nanoperiod(paste0(0:9,"d")))
expect_identical(as.nanoperiod(paste0(1:10, "d")) - as.nanoperiod(paste0(0:9, "d")),
               as.nanoperiod(paste0(rep(1,10),"d")))

##test_nanoperiod_minus_numeric <- function() {
expect_identical(as.nanoperiod("2m2d") - 1,
               as.nanoperiod("2m2d/-00:00:00.000_000_001"))
expect_identical(as.nanoperiod(paste0(1:10,"m2d")) - 1,
               as.nanoperiod(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.nanoperiod("12m2d") - 1:9.0,
               as.nanoperiod(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_nanoperiod_minus_nanoduration <- function() {
expect_identical(as.nanoperiod("2m2d") - as.nanoduration(1), as.nanoperiod("2m2d/-00:00:00.000_000_001"))
expect_identical(as.nanoperiod(paste0(1:10,"m2d")) - as.nanoduration(1),
               as.nanoperiod(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.nanoperiod("12m2d") - as.nanoduration(1:9),
               as.nanoperiod(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_nanoperiod_minus_integer <- function() {
expect_identical(as.nanoperiod("2m2d") - as.integer(1), as.nanoperiod("2m2d/-00:00:00.000_000_001"))
expect_identical(as.nanoperiod(paste0(1:10,"m2d")) - as.integer(1),
               as.nanoperiod(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.nanoperiod("12m2d") - as.integer(1:9),
               as.nanoperiod(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_nanoperiod_minus_integer64 <- function() {
expect_identical(as.nanoperiod("2m2d") - as.integer64(1), as.nanoperiod("2m2d/-00:00:00.000_000_001"))
expect_identical(as.nanoperiod(paste0(1:10,"m2d")) - as.integer64(1),
               as.nanoperiod(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.nanoperiod("12m2d") - as.integer64(1:9),
               as.nanoperiod(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_numeric_minus_nanoperiod <- function() {
expect_identical(1 - as.nanoperiod("1m1d"), as.nanoperiod("-1m-1d/00:00:00.000_000_001"))
expect_identical(1:10 - as.nanoperiod("1m1d"), nanoperiod(-1,-1,1:10))
expect_identical(1 - nanoperiod(1:10,1,1), nanoperiod(-1:-10, -1, 0))

##test_integer64_minus_nanoperiod <- function() {
expect_identical(as.integer64(1) - as.nanoperiod("1m1d"),
               as.nanoperiod("-1m-1d/00:00:00.000_000_001"))
expect_identical(as.integer64(1:10) - as.nanoperiod("1m1d"), nanoperiod(-1,-1,1:10))
expect_identical(as.integer64(1) - nanoperiod(1:10,1,1), nanoperiod(-1:-10, -1, 0))

##test_nanoduration_minus_nanoperiod <- function() {
expect_identical(as.nanoduration(1) - as.nanoperiod("1m1d"),
               as.nanoperiod("-1m-1d/00:00:00.000_000_001"))
expect_identical(as.nanoduration(1:10) - as.nanoperiod("1m1d"), nanoperiod(-1,-1,1:10))
expect_identical(as.nanoduration(1) - nanoperiod(1:10,1,1), nanoperiod(-1:-10, -1, 0))

##test_nanoperiod_minus_any <- function() {
expect_error(as.nanoperiod(1) - "a", "invalid operand types")

##test_any_minus_nanoperiod <- function() {
expect_error("a" - as.nanoperiod(1), "invalid operand types")
expect_error(as.nanoperiod(1) - nanotime(1), "invalid operand types")

## +
##test_nanoperiod_plus_nanoperiod <- function() {
expect_identical(+as.nanoperiod("2m"), as.nanoperiod("2m"))
expect_identical(as.nanoperiod("2m2d") + as.nanoperiod("1m1d"), as.nanoperiod("3m3d"))
expect_identical(as.nanoperiod("-1m-1d/00:00:01") + as.nanoperiod("1m1d/00:00:01"),
               as.nanoperiod("0m0d/00:00:02"))
expect_identical(nanoperiod(1,1,1:10) + nanoperiod(1,1,0), nanoperiod(2,2,1:10))
expect_identical(nanoperiod(1,1,0) + nanoperiod(1,1,1:10), nanoperiod(2,2,1:10))

##test_nanoperiod_plus_nanoperiod_names <- function() {
non_scalar <- as.nanoperiod(c(a="2m2d", b="3m3d"))
scalar <- as.nanoperiod(c(c="1m1d"))
expected <- as.nanoperiod(c(a="3m3d", b="4m4d"))
expect_identical(non_scalar + scalar, expected)
expect_identical(scalar + non_scalar, expected)

##test_integer64_plus_nanoperiod <- function() {
expect_identical(as.integer64(1) + as.nanoperiod(1), as.nanoperiod(2))
expect_identical(as.integer64(0:9) + as.nanoperiod(1), as.nanoperiod(1:10))
expect_identical(as.integer64(1) + nanoperiod(1:10,1,0), nanoperiod(1:10,1,1))

##test_nanoperiod_plus_integer64 <- function() {
expect_identical(as.nanoperiod(1) + as.integer64(1), as.nanoperiod(2))
expect_identical(as.nanoperiod("2m2d") + as.integer64(1), as.nanoperiod("2m2d/00:00:00.000_000_001"))
expect_identical(as.nanoperiod(1) + as.integer64(0:9), as.nanoperiod(1:10))
expect_identical(nanoperiod(1:10,1,0) + as.integer64(1), nanoperiod(1:10,1,1))

##test_nanoperiod_plus_nanoduration <- function() {
expect_identical(as.nanoperiod(1) + as.nanoduration(1), as.nanoperiod(2))
expect_identical(as.nanoperiod("2m2d") + as.nanoduration(1), as.nanoperiod("2m2d/00:00:00.000_000_001"))
expect_identical(as.nanoperiod(1) + as.nanoduration(0:9), as.nanoperiod(1:10))
expect_identical(nanoperiod(1:10,1,0) + as.nanoduration(1), nanoperiod(1:10,1,1))

##test_nanoduration_plus_nanoperiod <- function() {
expect_identical(as.nanoduration(1) + as.nanoperiod(1), as.nanoperiod(2))
expect_identical(as.nanoduration(1) + as.nanoperiod("2m2d"), as.nanoperiod("2m2d/00:00:00.000_000_001"))
expect_identical(as.nanoduration(0:9) + as.nanoperiod(1), as.nanoperiod(1:10))
expect_identical(as.nanoduration(1) + nanoperiod(1:10,1,0), nanoperiod(1:10,1,1))

##test_numeric_plus_nanoperiod <- function() {
expect_identical(as.nanoperiod(1) + 1, as.nanoperiod(2))
expect_identical(as.nanoperiod("2m2d") + 1, as.nanoperiod("2m2d/00:00:00.000_000_001"))
expect_identical(0.0:9.0 + as.nanoperiod(1), as.nanoperiod(1:10))
expect_identical(1.0 + nanoperiod(1:10,1,0), nanoperiod(1:10,1,1))

##test_any_plus_nanoperiod <- function() {
expect_error("a" + as.nanoperiod(1), "invalid operand types")

##test_nanoperiod_plus_any <- function() {
expect_error(as.nanoperiod(1) + any, "invalid operand types")


## *
##test_nanoperiod_times_numeric <- function() {
expect_identical(as.nanoperiod(1) * 3, as.nanoperiod(3))
expect_identical(as.nanoperiod("1m1d") * 3, as.nanoperiod("3m3d"))
expect_identical(nanoperiod(1,1,1) * 1:10, nanoperiod(1:10,1:10,1:10))
expect_identical(nanoperiod(1:10,1,1) * 3, nanoperiod(1:10 * 3, 3, 3))

##test_nanoperiod_times_integer64 <- function() {
expect_identical(as.nanoperiod(1) * as.integer64(3), as.nanoperiod(3))
expect_identical(nanoperiod(1,1,1) * as.integer64(1:10), nanoperiod(1:10,1:10,1:10))
expect_identical(nanoperiod(1:10,1,1) * as.integer64(3), nanoperiod(1:10 * 3, 3, 3))

##test_numeric_times_nanoperiod <- function() {
expect_identical(3 * as.nanoperiod(1), as.nanoperiod(3))
expect_identical(4.5 * as.nanoperiod("10d"), as.nanoperiod("45d"))
expect_identical(1:10.0 * as.nanoperiod(1), as.nanoperiod(1:10))
expect_identical(1 * as.nanoperiod(1:10), as.nanoperiod(1:10))

##test_integer64_times_nanoperiod <- function() {
expect_identical(as.integer64(3) * as.nanoperiod(1), as.nanoperiod(3))
expect_identical(as.integer64(3) * as.nanoperiod("1m1d"), as.nanoperiod("3m3d"))
expect_identical(as.integer64(1:10) * as.nanoperiod(1), as.nanoperiod(1:10))
expect_identical(as.integer64(1) * as.nanoperiod(1:10), as.nanoperiod(1:10))

##test_character_times_nanoperiod <- function() {
expect_error("a"   * as.nanoperiod(1), "invalid operand types")
expect_error("123" * as.nanoperiod(1), "invalid operand types")

##test_nanoperiod_times_character <- function() {
expect_error(as.nanoperiod(1) * "a",   "invalid operand types")
expect_error(as.nanoperiod(1) * "123", "invalid operand types")


## /
##test_nanoperiod_div_numeric <- function() {
expect_identical(as.nanoperiod(4) / 3, as.nanoperiod(1))
expect_identical(as.nanoperiod("5m5d") / 2.5, as.nanoperiod("2m2d"))
expect_identical(as.nanoperiod(4) / c(4,2,1), as.nanoperiod(c(1,2,4)))
expect_identical(as.nanoperiod(4:2) / c(4,2,1), as.nanoperiod(c(1,1,2)))
expect_error(as.nanoperiod("2m") / 0, "divide by zero")

##test_nanoperiod_div_integer64 <- function() {
expect_identical(as.nanoperiod(4) / as.integer64(3), as.nanoperiod(1))
expect_identical(as.nanoperiod("5m5d") / as.integer64(2), as.nanoperiod("2m2d"))
expect_identical(as.nanoperiod(4) / as.integer64(c(4,2,1)), as.nanoperiod(c(1,2,4)))
expect_identical(as.nanoperiod(4:2) / as.integer64(c(4,2,1)), as.nanoperiod(c(1,1,2)))

##test_nanoperiod_div_integer <- function() {
expect_identical(as.nanoperiod(4) / as.integer(3), as.nanoperiod(1))
expect_identical(as.nanoperiod("5m5d") / as.integer(2), as.nanoperiod("2m2d"))
expect_identical(as.nanoperiod(4) / as.integer(c(4,2,1)), as.nanoperiod(c(1,2,4)))
expect_identical(as.nanoperiod(4:2) / as.integer(c(4,2,1)), as.nanoperiod(c(1,1,2)))

##test_nanoperiod_div_any <- function() {
expect_error(as.nanoperiod(1) / "a", "invalid operand types")

##test_any_div_nanoperiod <- function() {
expect_error("a" / as.nanoperiod(1), "invalid operand types")

##test_Logic_nanoperiod_any <- function() {
expect_error(as.nanoperiod(1) | "a", "operation not defined for 'nanoperiod' objects")

##test_Logic_any_nanoperiod <- function() {
expect_error("a" | as.nanoperiod(1), "operation not defined for 'nanoperiod' objects")


## Math/Math2/Summary/Complex
##test_nanoperiod_Math <- function() {
## is that right? LLL
expect_error(abs(as.nanoperiod(1)), "operation not defined for 'nanoperiod' objects")  

##test_nanoperiod_Math2 <- function() {
expect_error(round(as.nanoperiod(1)), "operation not defined for 'nanoperiod' objects")  

##test_nanoperiod_Summary  <- function() {
expect_error(min(as.nanoperiod(1)), "invalid 'type' \\(nanoperiod\\) of argument")
expect_error(max(as.nanoperiod(1)), "invalid 'type' \\(nanoperiod\\) of argument")

##test_nanoperiod_Complex  <- function() {
expect_error(Arg(as.nanoperiod(1)), "operation not defined for 'nanoperiod' objects")  

##test_binary_plus_nanoperiod_nanotime <- function() {
expect_error(as.nanoperiod(1) + nanotime(1),
             "binary '\\+' is not defined for 'nanoperiod' and 'nanotime' objects; instead use 'plus\\(e1, e2, tz\\)'")

##test_binary_plus_nanotime_nanoperiod <- function() {
expect_error(nanotime(1) + as.nanoperiod(1),
             "binary '\\+' is not defined for 'nanotime' and 'nanoperiod' objects; instead use 'plus\\(e1, e2, tz\\)'")

##test_binary_plus_nanoperiod_nanoival <- function() {
expect_error(as.nanoperiod(1) + nanoival(nanotime(1), nanotime(2)),
             "binary '\\+' is not defined for 'nanoperiod' and 'nanoival' objects; instead use 'plus\\(e1, e2, tz\\)'")

##test_binary_plus_nanoival_nanoperiod <- function() {
expect_error(nanoival(nanotime(1), nanotime(2)) + as.nanoperiod(1),
             "binary '\\+' is not defined for 'nanoival' and 'nanoperiod' objects; instead use 'plus\\(e1, e2, tz\\)'")


## Compare
## ----------

##test_nanoperiod_eq_nanoperiod <- function() {
expect_true(as.nanoperiod(1) == as.nanoperiod(1))
expect_true(as.nanoperiod("1d") == as.nanoperiod("1d"))
expect_true(!(as.nanoperiod(1) == as.nanoperiod(2)))
expect_identical(as.nanoperiod(1:10) == as.nanoperiod(1:10), rep(TRUE, 10))

##test_nanoperiod_ne_nanoperiod <- function() {
expect_true(as.nanoperiod(1) != as.nanoperiod(2))
expect_true(as.nanoperiod("1d") != as.nanoperiod("2d"))
expect_true(!(as.nanoperiod(1) != as.nanoperiod(1)))
expect_identical(as.nanoperiod(1:10) != as.nanoperiod(1:10), rep(FALSE, 10))

##test_nanoperiod_eq_any <- function() {
expect_error(as.nanoperiod(1) == "a", "operation not defined for 'nanoperiod' objects")  

##test_any_eq_nanoperiod <- function() {
expect_error("a" == as.nanoperiod(1), "operation not defined for 'nanoperiod' objects")  

##test_all.equal <- function() {
expect_true(all.equal(as.nanoperiod(1), as.nanoperiod(1)))
expect_true(all.equal(as.nanoperiod(1:10), as.nanoperiod(1:10)))
expect_true(all.equal(as.nanoperiod("1m1d"), as.nanoperiod("1d") + as.nanoperiod("1m")))


## names (in general)
##test_nanoperiod_get_names <- function() {
a <- as.nanoperiod(1:10)
names(a) <- "b"
expect_identical(names(a), c("b", rep(as.character(NA), 9)))
    
##test_nanoperiod_set_names <- function() {
names <- c("a","b","c","d")
pp <- as.nanoperiod(1:4)
names(pp) <- names
expect_identical(names(pp), names)
names(pp)[1] <- "x"
expect_identical(names(pp), c("x","b","c","d"))
    

##test_nanoperiod_c <- function() {
pp <- c(x=as.nanoperiod(1), y=as.nanoperiod(2))
expect_identical(names(pp), c("x","y"))
expect_identical(pp[1], c(x=as.nanoperiod(1)))
expect_identical(pp[2], c(y=as.nanoperiod(2)))

pp <- c(as.nanoperiod(1:10), as.nanoperiod(11:20))
expect_identical(pp, as.nanoperiod(1:20))


if (!isSolaris) {

    ## plus/minus with 'nanotime':

    ##test_plus_nanotime_nanoperiod <- function() {
    nt <- nanotime("2018-01-01T05:00:00.000000000+00")
    p  <- c(p=as.nanoperiod("4m"))
    tz <- "America/New_York"
    expected <- c(p=nanotime("2018-05-01T00:00:00.000000000-04:00"))
    expect_identical(plus(nt, p, tz), expected)

    ##test_plus_nanotime_nanoperiod_vector1 <- function() {
    nt <- nanotime("2018-01-01T05:00:00.000000000+00") + 1:10
    p  <- as.nanoperiod("4m")
    tz <- "America/New_York"
    expected <- nanotime("2018-05-01T00:00:00.000000000-04:00") + 1:10
    expect_identical(plus(nt, p, tz), expected)

    ##test_plus_nanotime_nanoperiod_vector2 <- function() {
    nt <- nanotime("2018-01-01T05:00:00.000000000+00")
    p  <- as.nanoperiod("4m") + 1:10
    tz <- "America/New_York"
    expected <- nanotime("2018-05-01T00:00:00.000000000-04:00") + 1:10
    expect_identical(plus(nt, p, tz), expected)

    ##test_plus_nanotime_nanoperiod_vector3 <- function() {
    nt <- nanotime("2018-01-01T05:00:00.000000000+00")
    p  <- as.nanoperiod("4m") + 1:10
    tz <- rep("America/New_York", 10)
    expected <- nanotime("2018-05-01T00:00:00.000000000-04:00") + 1:10
    expect_identical(plus(nt, p, tz), expected)

    ##test_plus_nanoperiod_nanotime <- function() {
    nt <- nanotime("2018-01-01T05:00:00.000000000+00")
    p  <- c(p=as.nanoperiod("4m"))
    tz <- "America/New_York"
    expected <- c(p=nanotime("2018-05-01T00:00:00.000000000-04:00"))
    expect_identical(plus(p, nt, tz), expected)

    ##test_minus_nanotime_nanoperiod <- function() {
    nt <- c(p1=nanotime("2018-05-01T00:00:00.000000000-04:00"))
    p  <- c(p2=as.nanoperiod("4m"))
    tz <- "America/New_York"
    expected <- c(p1=nanotime("2018-01-01T00:00:00.000000000-05:00"))
    expect_identical(minus(nt, p, tz), expected)

    ##test_minus_nanoperiod_nanotime <- function() {
    nt <- nanotime("2018-05-01T00:00:00.000000000-04:00")
    p  <- as.nanoperiod("4m")
    tz <- "America/New_York"
    expect_error(minus(p, nt, tz), "operation not defined for 'nanoperiod' objects")

    ## test the crossing of daylight saving time in both directions:

    ## adding/subtracting a nanoperiod should not realign if doing so
    ## crosses again a DST boundary:

    ## look at cases over the Spring boundary in America:
    nt <- as.nanotime("2020-03-08 01:45:30 America/New_York")
    p <- as.nanoperiod("00:30:00")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-03-08 03:15:30 America/New_York")
    expect_identical(plus(nt, p, tz), expected)

    nt <- as.nanotime("2020-03-08 01:00:00 America/New_York")
    p <- as.nanoperiod("01:00:00")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-03-08 03:00:00 America/New_York")
    expect_identical(plus(nt, p, tz), expected)

    nt <- as.nanotime("2020-03-08 03:15:30 America/New_York")
    p <- as.nanoperiod("00:30:00")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-03-08 01:45:30 America/New_York")
    expect_identical(minus(nt, p, tz), expected)

    nt <- as.nanotime("2020-03-08 03:00:00 America/New_York")
    p <- as.nanoperiod("01:00:00")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-03-08 01:00:00 America/New_York")
    expect_identical(minus(nt, p, tz), expected)

    ## look at the cases over the Autumn boundary in America:
    nt <- as.nanotime("2020-11-01 01:45:30 America/New_York")
    p <- as.nanoperiod("00:30:00")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-11-01 02:15:30 America/New_York")
    expect_identical(plus(nt, p, tz), expected)

    nt <- as.nanotime("2020-11-01 01:00:00 America/New_York")
    p <- as.nanoperiod("01:00:00")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-11-01 02:00:00 America/New_York")
    expect_identical(plus(nt, p, tz), expected)

    nt <- as.nanotime("2020-11-01 02:15:30 America/New_York")
    p <- as.nanoperiod("00:30:00")
    tz <- "America/New_York"
    ##expected <- as.nanotime("2020-11-01 01:45:30 America/New_York")  # ambiguous
    expected <- as.nanotime("2020-11-01 06:45:30+00:00")
    expect_identical(minus(nt, p, tz), expected)

    nt <- as.nanotime("2020-11-01 02:00:00 America/New_York")
    p <- as.nanoperiod("01:00:00")
    tz <- "America/New_York"
    ## expected <- as.nanotime("2020-11-01 01:00:00 America/New_York")    # ambiguous
    expected <- as.nanotime("2020-11-01 06:00:00+00:00")
    expect_identical(minus(nt, p, tz), expected)
    
    
    ## adding/subtracting a nanoperiod should realign if doing so does
    ## not cross again a DST boundary:    

    ## look at cases over the Spring boundary in America:
    nt <- as.nanotime("2020-03-08 01:00:00 America/New_York")
    p <- as.nanoperiod("02:00:01")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-03-08 03:00:01 America/New_York")
    expect_identical(plus(nt, p, tz), expected)

    nt <- as.nanotime("2020-03-08 03:00:00 America/New_York")
    p <- as.nanoperiod("02:00:01")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-03-08 00:59:59 America/New_York")
    expect_identical(minus(nt, p, tz), expected)

    ## look at the cases over the Autumn boundary in America:
    nt <- as.nanotime("2020-11-01 01:00:00 America/New_York")
    p <- as.nanoperiod("02:00:01")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-11-01 03:00:01 America/New_York")
    expect_identical(plus(nt, p, tz), expected)

    nt <- as.nanotime("2020-11-01 03:00:00 America/New_York")
    p <- as.nanoperiod("02:00:01")
    tz <- "America/New_York"
    expected <- as.nanotime("2020-11-01 00:59:59 America/New_York")
    expect_identical(minus(nt, p, tz), expected)
    
    
    ## plus/minus with 'nanoival':

    ##test_plus_nanoival_nanoperiod <- function() {
    start <- nanotime("2018-01-01T05:00:00.000000000+00")
    end <- nanotime("2018-01-01T23:00:00.000000000+00")
    ni <- nanoival(start, end)
    p  <- as.nanoperiod("4m")
    tz <- "America/New_York"
    expected <- as.nanoival("+2018-05-01T00:00:00.000000000-04:00 -> 2018-05-01T18:00:00.000000000-04:00-")
    expect_identical(plus(ni, p, tz), expected)

    ##test_plus_nanoival_nanoperiod_pre_1970 <- function() {
    start <- nanotime("1969-01-01T05:00:00.000000000+00")
    end <- nanotime("1969-01-01T23:00:00.000000000+00")
    ni <- nanoival(start, end)
    p  <- as.nanoperiod("4m")
    tz <- "America/New_York"
    expected <- as.nanoival("+1969-05-01T00:00:00.000000000-04:00 -> 1969-05-01T18:00:00.000000000-04:00-")
    expect_identical(plus(ni, p, tz), expected)

    ##test_plus_nanoperiod_nanoival <- function() {
    start <- nanotime("2018-01-01T05:00:00.000000000+00")
    end <- nanotime("2018-01-01T23:00:00.000000000+00")
    ni <- nanoival(start, end)
    p  <- c(a=as.nanoperiod("4m"))
    tz <- "America/New_York"
    expected <- c(a=as.nanoival("+2018-05-01T00:00:00.000000000-04:00 -> 2018-05-01T18:00:00.000000000-04:00-"))
    expect_identical(plus(p, ni, tz), expected)

    ##test_minus_nanoival_nanoperiod <- function() {
    start <- nanotime("2018-05-01T05:00:00.000000000-04")
    end <- nanotime("2018-05-01T23:00:00.000000000-04")
    ni <- c(a=nanoival(start, end))
    p  <- as.nanoperiod("4m")
    tz <- "America/New_York"
    expected <- c(a=as.nanoival("+2018-01-01T05:00:00.000000000-05:00 -> 2018-01-01T23:00:00.000000000-05:00-"))
    expect_identical(minus(ni, p, tz), expected)

    ##test_minus_nanoperiod_nanoival <- function() {
    start <- nanotime("2018-05-01T05:00:00.000000000-04")
    end <- nanotime("2018-05-01T23:00:00.000000000-04")
    ni <- nanoival(start, end)
    p  <- as.nanoperiod("4m")
    tz <- "America/New_York"
    expect_error(minus(p, ni, tz), "operation not defined for 'nanoperiod' objects")

}

## NA stuff
expect_true(is.na(as.nanoperiod(NA_integer_)))
expect_true(is.na(as.nanoperiod(NA_integer64_)))
expect_true(is.na(as.nanoperiod(NA_real_)))
expect_true(is.na(as.nanoperiod(as.integer(NaN))))
p <- as.nanoperiod(1:10)
is.na(p) <- 1:3
expect_true(all(is.na(p[1:3])))
expect_true(!any(is.na(p[4:10])))
expect_true(is.na(NA_nanoperiod_))
expect_true(is.na(nanoperiod.nanoduration(NA_nanoperiod_)))
expect_true(is.na(nanoperiod.month(NA_nanoperiod_)))
expect_true(is.na(nanoperiod.day(NA_nanoperiod_)))

## test S4 conversions:
expect_identical(nanoperiod(1,1,1), as("1m1d/00:00:00.000_000_001", "nanoperiod"))
hour <- 3600*1e9
expect_identical(as.nanoperiod(as.integer64(hour)), as(hour, "nanoperiod"))
expect_identical(as.nanoperiod(hour), as(hour, "nanoperiod"))
expect_identical(as.nanoperiod(hour), as(as.nanoduration(hour), "nanoperiod"))

## 0-length ops:
## ------------

## constructor:

expect_identical(nanoperiod(integer(), integer(), nanoduration()), nanoperiod())
expect_identical(nanoperiod(1, 1, nanoduration()), nanoperiod())
expect_identical(nanoperiod(1,integer(), as.nanoduration(1)), nanoperiod())
expect_identical(nanoperiod(numeric(), integer(), as.nanoduration(1)), nanoperiod())

## Comp:
expect_identical(as.nanoperiod() == as.nanoperiod(), logical())
expect_identical(as.nanoperiod(1) == as.nanoperiod(), logical())
expect_identical(as.nanoperiod(1) != as.nanoperiod(), logical())


## ops
expect_identical(as.nanoperiod() + as.nanoperiod(), as.nanoperiod())
expect_identical(nanoperiod(1)   + as.nanoperiod(), as.nanoperiod())
expect_identical(nanoperiod()    + as.nanoperiod(1:10), as.nanoperiod())
expect_identical(nanoperiod()    + as.nanoduration(1:10), as.nanoperiod())
expect_identical(nanoperiod()    + as.integer64(1:10), as.nanoperiod())
expect_identical(nanoperiod(1)   + as.integer(), as.nanoperiod())
expect_identical(as.nanoperiod() - as.nanoperiod(), as.nanoperiod())
expect_identical(nanoperiod(1)   - as.nanoperiod(), as.nanoperiod())
expect_identical(nanoperiod()    - as.nanoperiod(1:10), as.nanoperiod())
expect_identical(nanoperiod()    * 3, as.nanoperiod())
expect_identical(nanoperiod(1)   * integer(), as.nanoperiod())

## accessors
expect_identical(nanoperiod.month(nanoperiod()), numeric())
expect_identical(nanoperiod.day(nanoperiod()), numeric())
expect_identical(nanoperiod.nanoduration(nanoperiod()), as.nanoduration())

## all.equal:
expect_identical(all.equal(as.nanoperiod("1m"), as.nanoperiod("1m")), TRUE)
expect_false(isTRUE(all.equal(as.nanoperiod("1d"), "A")))
expect_identical(all.equal(as.nanoperiod("1d"), NA_nanoperiod_), "'is.NA' value mismatch: 1 in current 0 in target")


## test rounding functions:

## nano_ceiling:

if (isSolaris) exit_file("skip remainder on Solaris")

## years:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("12m"), tz="America/New_York"),
                 as.nanotime("2011-01-01T00:00:00-05:00"))
expect_identical(nano_ceiling(as.nanotime("1970-01-01 America/New_York"), as.nanoperiod("12m"), tz="America/New_York"),
                 as.nanotime("1970-01-01 America/New_York"))
expect_identical(nano_ceiling(as.nanotime("1970-01-01 06:00:00 America/New_York"), as.nanoperiod("12m"), tz="America/New_York"),
                 as.nanotime("1971-01-01 America/New_York"))

expect_error(nano_ceiling(c(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanotime("2010-10-10 11:00:00 America/New_York")),
                          as.nanoperiod("12m"), tz="America/New_York"),
             "'x' must be sorted")

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("12m"), tz=c("America/New_York", "UTC")),
             "'tz' must be scalar")

expect_identical(nano_ceiling(as.nanotime("1940-10-10 12:00:00 America/New_York"), as.nanoperiod("12m"), tz="America/New_York"),
                 as.nanotime("1941-01-01T00:00:00-05:00"))

expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"),
                              as.nanoperiod("12m"),
                              origin=as.nanotime("2010-10-10 00:00:01 America/New_York"),
                              tz="America/New_York"),
                 as.nanotime("2011-10-10T04:00:01+00:00"))

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"),
                          as.nanoperiod("12m"),
                          origin=as.nanotime(1:10),
                          tz="America/New_York"),
             "'origin' must be scalar")

expect_identical(nano_ceiling(as.nanotime("1904-10-10 12:00:00 America/New_York"),
                              as.nanoperiod("12m"),
                              origin=as.nanotime("1904-10-10 00:00:01 America/New_York"),
                              tz="America/New_York"),
                 as.nanotime("1905-10-10T05:00:01+00:00"))

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"),
                          as.nanoperiod("12m"),
                          origin=as.nanotime("2004-10-10 00:00:01 America/New_York"),
                          tz="America/New_York"),
             "when specifying 'origin', the first interval must contain at least one observation")

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoperiod("-12m"), tz="America/New_York"),
             "'precision' must be strictly positive")

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoperiod("-12m"), tz="America/New_York", origin="wrong type"),
             "'origin' must be of class 'nanotime'")

expect_error(nano_ceiling(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoperiod("-12m"), tz=12),
             "'tz' must be of type 'character'")

## months:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("13m"), tz="America/New_York"),
                 as.nanotime("2011-11-01T00:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-08-10 12:00:00 America/New_York"), as.nanoperiod("3m"), tz="America/New_York"),
                 as.nanotime("2010-10-01T00:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-08-10 12:00:00 America/New_York"), as.nanoperiod("1m"), tz="America/New_York"),
                 as.nanotime("2010-09-01T00:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("1940-08-10 12:00:00 America/New_York"), as.nanoperiod("1m"), tz="America/New_York"),
                 as.nanotime("1940-09-01T00:00:00-04:00"))

## days:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("1d"), tz="America/New_York"),
                 as.nanotime("2010-10-11T00:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("2d"), tz="America/New_York"),
                 as.nanotime("2010-10-12T00:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("1940-10-10 12:00:00 America/New_York"), as.nanoperiod("2d"), tz="America/New_York"),
                 as.nanotime("1940-10-12T00:00:00-05:00"))

## hours:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("06:00:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("1927-10-10 12:00:00 America/New_York"), as.nanoperiod("06:00:00"), tz="America/New_York"),
                 as.nanotime("1927-10-10T12:00:00-05:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("01:00:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("06:00:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T18:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("07:00:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T19:00:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("1899-10-10 12:23:23 America/New_York"), as.nanoperiod("07:00:00"), tz="America/New_York"),
                 as.nanotime("1899-10-10T19:00:00-05:00"))

## minutes:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("00:05:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:25:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:25:00 America/New_York"), as.nanoperiod("00:05:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:25:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("1922-10-10 12:23:23 America/New_York"), as.nanoperiod("00:05:00"), tz="America/New_York"),
                 as.nanotime("1922-10-10T12:25:00-05:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("00:01:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:24:00-04:00"))
expect_identical(nano_ceiling(as.nanotime("1922-10-10 12:25:00 America/New_York"), as.nanoperiod("00:05:00"), tz="America/New_York"),
                 as.nanotime("1922-10-10T12:25:00-05:00"))

## seconds:
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("00:00:05"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:25-04:00"))
expect_identical(nano_ceiling(as.nanotime("1969-10-10 12:23:23 America/New_York"), as.nanoperiod("00:00:05"), tz="America/New_York"),
                 as.nanotime("1969-10-10T12:23:25-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123 America/New_York"), as.nanoperiod("00:00:01"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:24-04:00"))
expect_identical(nano_ceiling(as.nanotime("1888-10-10 12:23:23.123 America/New_York"), as.nanoperiod("00:00:11"), tz="America/New_York"),
                 as.nanotime("1888-10-10T12:23:34-05:00"))

## milliseconds
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.001"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.124-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.010"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.13-04:00"))

## microseconds
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000001"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123457-04:00"))
expect_identical(nano_ceiling(as.nanotime("1933-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000001"), tz="America/New_York"),
                 as.nanotime("1933-10-10T12:23:23.123457-05:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000010"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.12346-04:00"))

## nanoseconds
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000001"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123456789-04:00"))
expect_identical(nano_ceiling(as.nanotime("1931-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000001"), tz="America/New_York"),
                 as.nanotime("1931-10-10T12:23:23.123456789-05:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000010"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123456790-04:00"))
expect_identical(nano_ceiling(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000033"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123456789-04:00"))


## nano_floor


## years:
expect_identical(nano_floor(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("12m"), tz="America/New_York"),
                 as.nanotime("2010-01-01T05:00:00-00:00"))
expect_identical(nano_floor(as.nanotime("1970-01-01 06:00:00 America/New_York"), as.nanoperiod("12m"), tz="America/New_York"),
                 as.nanotime("1970-01-01 America/New_York"))

expect_error(nano_floor(c(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanotime("2010-10-10 11:00:00 America/New_York")),
                        as.nanoperiod("12m"), tz="America/New_York"),
             "'x' must be sorted")

expect_error(nano_floor(as.nanotime("2010-10-10 12:00:00 America/New_York"), as.nanoperiod("12m"), tz=c("America/New_York", "UTC")),
             "'tz' must be scalar")

expect_identical(nano_floor(as.nanotime("2010-10-10 12:00:00 America/New_York"),
                              as.nanoperiod("12m"),
                              origin=as.nanotime("2010-10-10 00:00:01 America/New_York"),
                              tz="America/New_York"),
                 as.nanotime("2010-10-10T04:00:01+00:00"))

expect_error(nano_floor(as.nanotime("2010-10-10 12:00:00 America/New_York"),
                          as.nanoperiod("12m"),
                          origin=as.nanotime("2004-10-10 00:00:01 America/New_York"),
                          tz="America/New_York"),
             "when specifying 'origin', the first interval must contain at least one observation")


expect_error(nano_floor(as.nanotime("2010-10-10 12:00:00 America/New_York"),
                          as.nanoperiod("12m"),
                          origin=as.nanotime(1:10),
                          tz="America/New_York"),
             "'origin' must be scalar")

expect_error(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoperiod("-12m"), tz="America/New_York"),
             "'precision' must be strictly positive")

expect_error(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoperiod("-12m"), tz="America/New_York", origin="wrong type"),
             "'origin' must be of class 'nanotime'")

expect_error(nano_floor(as.nanotime("2010-10-10 12:23:23 UTC"), as.nanoperiod("-12m"), tz=12),
             "'tz' must be of type 'character'")



## hours:
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("06:00:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:00:00-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("07:00:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:00:00-04:00"))
## minutes:
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("00:05:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:20:00-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:20:00 America/New_York"), as.nanoperiod("00:05:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:20:00-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("00:01:00"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:00-04:00"))
expect_identical(nano_floor(as.nanotime("1911-10-10 12:20:00 America/New_York"), as.nanoperiod("00:01:00"), tz="America/New_York"),
                 as.nanotime("1911-10-10T12:20:00-05:00"))

## seconds:
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23 America/New_York"), as.nanoperiod("00:00:05"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:20-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123 America/New_York"), as.nanoperiod("00:00:01"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23-04:00"))

## milliseconds
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.001"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.010"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.12-04:00"))

## microseconds
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000001"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123456-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000010"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.12345-04:00"))

## nanoseconds
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000001"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123456789-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000010"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.12345678-04:00"))
expect_identical(nano_floor(as.nanotime("2010-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000033"), tz="America/New_York"),
                 as.nanotime("2010-10-10T12:23:23.123456789-04:00"))
expect_identical(nano_floor(as.nanotime("1965-10-10 12:23:23.123456789 America/New_York"), as.nanoperiod("00:00:00.000000033"), tz="America/New_York"),
                 as.nanotime("1965-10-10T12:23:23.123456789-04:00"))

## rep
expect_identical(rep(as.nanoperiod(1), 2), as.nanoperiod(rep(1,2)))
expect_identical(rep(as.nanoperiod(1:2), each=2), as.nanoperiod(rep(1:2, each=2)))
