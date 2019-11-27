
library(nanotime)

## constructors
##test_as.period_character <- function() {
p1 <- as.period("1m1d")
expect_identical(period.day(p1), 1)
expect_identical(period.month(p1), 1)
expect_identical(period.duration(p1), as.duration(0))

expect_identical(as.period("1y"), as.period("12m"))
expect_identical(as.period("2y"), as.period("24m"))
expect_identical(as.period("1w"), as.period("7d"))
expect_identical(as.period("2w"), as.period("14d"))

p2 <- as.period("2m1d/00:01:01.1")
expect_identical(period.month(p2), 2)
expect_identical(period.day(p2), 1)
expect_identical(period.duration(p2), as.duration("00:01:01.1"))

p3 <- as.period(c("1d","2d","3d"))
expect_identical(period.day(p3[1]), 1)
expect_identical(period.day(p3[2]), 2)
expect_identical(period.day(p3[3]), 3)    

## check year and week
expect_identical(as.character(as.period("1y/00:01:01")), "12m0d/00:01:01")
expect_identical(as.character(as.period("1w/00:01:01")), "0m7d/00:01:01")
expect_identical(names(as.period(c(a="1m"))), "a")

##test_as.period_character_error <- function() {
expect_error(as.period("1wm00:01:01"), "cannot parse period")
expect_error(as.period("1ym00:01:01"), "cannot parse period")
expect_error(as.period("1dm00:01:01"), "cannot parse period")

##test_as.period_integer64 <- function() {
p1 <- as.period(as.integer64(1:10))
expect_identical(period.duration(p1), as.duration(1:10))
p1 <- as.period(c(a=as.integer64(1)))
expect_identical(period.duration(p1), as.duration(c(a=1)))

##test_as.period_integer <- function() {
p1 <- as.period(1:10)
expect_identical(period.duration(p1), as.duration(1:10))
p1 <- as.period(c(a=1L))
expect_identical(period.duration(p1), as.duration(c(a=1)))

##test_as.period_numeric <- function() {
p1 <- as.period(as.numeric(1:10))
expect_identical(period.duration(p1), as.duration(1:10))
p1 <- as.period(c(a=1))
expect_identical(period.duration(p1), c(a=as.duration(1)))

##test_as.period_duration <- function() {
p1 <- as.period(c(a=as.duration(1)))
expect_identical(p1, c(a=as.period("00:00:00.000_000_001")))

##test_period <- function() {
expect_identical(period(0,0,1), as.period(1))
expect_identical(period(0,0,1:10), as.period(1:10))
expect_identical(period(1,1,0:9), as.period(paste0("1m1d/00:00:00.000_000_00", 0:9)))


## accessors:
##test_period.day <- function() {
p1 <- as.period(paste0(1:10, "d"))
names(p1) <- 1:10
expected  <- as.numeric(1:10)
names(expected)  <- 1:10
expect_identical(period.day(p1), expected)

##test_period.month <- function() {
p1 <- as.period(paste0(1:10, "m"))
names(p1) <- 1:10
expected  <- as.numeric(1:10)
names(expected)  <- 1:10
expect_identical(period.month(p1), expected)

##test_period.duration <- function() {
p1 <- as.period(1:10)
names(p1) <- 1:10
expected  <- as.numeric(1:10)
names(expected)  <- 1:10
expect_identical(period.duration(p1), as.duration(expected))


## show/print/as.character/format
##test_show <- function() {
p1 <- show(as.period("1m1d/1:00:00.1"))
expect_identical(p1, "1m1d/01:00:00.100")

##test_print <- function() {
p1 <- print(as.period("2m2d/2:02:02.2"))
expect_identical(p1, "2m2d/02:02:02.200")

##test_as.character <- function() {
p1 <- as.character(as.period("2m2d/2:02:02.20001"))
expect_identical(p1, "2m2d/02:02:02.200_010")

##test_as.character_named <- function() {
p1 <- as.character(c(a=as.period("2m2d/2:02:02.20001"),
                     b=as.period("2m2d/2:02:02.20002")))
expect_identical(p1, c(a="2m2d/02:02:02.200_010", b="2m2d/02:02:02.200_020"))

##test_format <- function() {
p1 <- as.period("2m2d/2:02:02.20001")
expect_identical(format(p1), "2m2d/02:02:02.200_010")


## subset:
##test_subset_int <- function() {
p1 <- as.period("1m1d/00:00:01")
p2 <- as.period("2m2d/00:00:02") 
p3 <- as.period("3m3d/00:00:03") 
p4 <- as.period("4m4d/00:00:04") 
pp <- c(p1, p2, p3, p4)
expect_identical(pp[1], p1)
expect_identical(pp[2], p2)
expect_identical(pp[3], p3)
expect_identical(pp[4], p4)
expect_identical(pp[1:4], pp)
expect_identical(pp[2:3], c(p2,p3))

##test_subset_logical <- function() {
p1 <- as.period("1m1d/00:00:01")
p2 <- as.period("2m2d/00:00:02") 
p3 <- as.period("3m3d/00:00:03") 
p4 <- as.period("4m4d/00:00:04") 
pp <- c(p1, p2, p3, p4)
expect_identical(pp[c(T,F,F,F)], p1)
expect_identical(pp[c(F,T,F,F)], p2)
expect_identical(pp[c(F,F,T,F)], p3)
expect_identical(pp[c(F,F,F,T)], p4)
expect_identical(pp[TRUE], pp)
expect_identical(pp[c(F,T,T,F)], c(p2,p3))

##test_subset_character <- function() {
pp <- c(x=as.period(1), y=as.period(2))
expect_identical(pp["x"], c(x=as.period(1)))
expect_identical(pp["y"], c(y=as.period(2)))
## expect_identical(pp["a"], as.period(as.integer64(NA)))    LLL


## subassign
##test_subassign_logical <- function() {
x <- as.period(1:10)
x[c(T,T,T,F,F,F,F,F,F,F)] <- as.period(2:4)
expect_identical(x, as.period(c(2:4, 4:10)))
x <- as.period(paste0(1:10, "d"))
x[c(T,T,T,F,F,F,F,F,F,F)] <- as.period(paste0(2:4, "d"))
expect_identical(x, as.period(paste0(c(2:4, 4:10), "d")))

##test_subassign_numeric <- function() {
x <- as.period(1:10)
x[1:3] <- as.period(2:4)
x[4:10] <- as.period(5:11)
expect_identical(x, as.period(2:11))
x <- as.period(paste0(1:10, "d"))
x[1:3] <- as.period(paste0(2:4, "d"))
x[4:10] <- as.period(paste0(5:11, "d"))
expect_identical(x, as.period(paste0(2:11, "d")))

##test_subsassign_character <- function() {
pp <- c(a=as.period(1), b=as.period(2), c=as.period(3), d=as.period(4))
pp[c("b", "c")] <- as.period(20:21)
expected <- c(a=as.period(1), b=as.period(20), c=as.period(21), d=as.period(4))
expect_identical(pp, expected)

##test_square_bracket <- function() {
pp <- c(a=as.period(1), b=as.period(2), c=as.period(3), d=as.period(4))
pp_nonames <- as.period(1:4)
expect_identical(pp_nonames[1], pp[[1]])
expect_identical(pp_nonames[2], pp[[2]])
expect_identical(pp_nonames[3], pp[[3]])


## ops
## -
##test_period_minus_period <- function() {
expect_identical(as.period("2m2d") - as.period("1m1d"), as.period("1m1d"))
expect_identical(as.period("-1m-1d/-00:00:01") - as.period("1m1d/00:00:01"),
               as.period("-2m-2d/-00:00:02"))

##test_period_minus_period_vector <- function() {
expect_identical(as.period("1d") - as.period(paste0(1:10, "d")),
               as.period(paste0(0:-9,"d")))
expect_identical(as.period(paste0(1:10, "d")) - as.period("1d"),
               as.period(paste0(0:9,"d")))
expect_identical(as.period(paste0(1:10, "d")) - as.period(paste0(0:9, "d")),
               as.period(paste0(rep(1,10),"d")))

##test_period_minus_numeric <- function() {
expect_identical(as.period("2m2d") - 1,
               as.period("2m2d/-00:00:00.000_000_001"))
expect_identical(as.period(paste0(1:10,"m2d")) - 1,
               as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.period("12m2d") - 1:9.0,
               as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_period_minus_duration <- function() {
expect_identical(as.period("2m2d") - as.duration(1), as.period("2m2d/-00:00:00.000_000_001"))
expect_identical(as.period(paste0(1:10,"m2d")) - as.duration(1),
               as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.period("12m2d") - as.duration(1:9),
               as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_period_minus_integer <- function() {
expect_identical(as.period("2m2d") - as.integer(1), as.period("2m2d/-00:00:00.000_000_001"))
expect_identical(as.period(paste0(1:10,"m2d")) - as.integer(1),
               as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.period("12m2d") - as.integer(1:9),
               as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_period_minus_integer64 <- function() {
expect_identical(as.period("2m2d") - as.integer64(1), as.period("2m2d/-00:00:00.000_000_001"))
expect_identical(as.period(paste0(1:10,"m2d")) - as.integer64(1),
               as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")))
expect_identical(as.period("12m2d") - as.integer64(1:9),
               as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)))

##test_numeric_minus_period <- function() {
expect_identical(1 - as.period("1m1d"), as.period("-1m-1d/00:00:00.000_000_001"))
expect_identical(1:10 - as.period("1m1d"), period(-1,-1,1:10))
expect_identical(1 - period(1:10,1,1), period(-1:-10, -1, 0))

##test_integer64_minus_period <- function() {
expect_identical(as.integer64(1) - as.period("1m1d"),
               as.period("-1m-1d/00:00:00.000_000_001"))
expect_identical(as.integer64(1:10) - as.period("1m1d"), period(-1,-1,1:10))
expect_identical(as.integer64(1) - period(1:10,1,1), period(-1:-10, -1, 0))

##test_duration_minus_period <- function() {
expect_identical(as.duration(1) - as.period("1m1d"),
               as.period("-1m-1d/00:00:00.000_000_001"))
expect_identical(as.duration(1:10) - as.period("1m1d"), period(-1,-1,1:10))
expect_identical(as.duration(1) - period(1:10,1,1), period(-1:-10, -1, 0))

##test_period_minus_any <- function() {
expect_error(as.period(1) - "a", "invalid operand types")

##test_any_minus_period <- function() {
expect_error("a" - as.period(1), "invalid operand types")


## +
##test_period_plus_period <- function() {
expect_identical(as.period("2m2d") + as.period("1m1d"), as.period("3m3d"))
expect_identical(as.period("-1m-1d/00:00:01") + as.period("1m1d/00:00:01"),
               as.period("0m0d/00:00:02"))
expect_identical(period(1,1,1:10) + period(1,1,0), period(2,2,1:10))
expect_identical(period(1,1,0) + period(1,1,1:10), period(2,2,1:10))

##test_period_plus_period_names <- function() {
non_scalar <- as.period(c(a="2m2d", b="3m3d"))
scalar <- as.period(c(c="1m1d"))
expected <- as.period(c(a="3m3d", b="4m4d"))
expect_identical(non_scalar + scalar, expected)
expect_identical(scalar + non_scalar, expected)

##test_integer64_plus_period <- function() {
expect_identical(as.integer64(1) + as.period(1), as.period(2))
expect_identical(as.integer64(0:9) + as.period(1), as.period(1:10))
expect_identical(as.integer64(1) + period(1:10,1,0), period(1:10,1,1))

##test_period_plus_integer64 <- function() {
expect_identical(as.period(1) + as.integer64(1), as.period(2))
expect_identical(as.period("2m2d") + as.integer64(1), as.period("2m2d/00:00:00.000_000_001"))
expect_identical(as.period(1) + as.integer64(0:9), as.period(1:10))
expect_identical(period(1:10,1,0) + as.integer64(1), period(1:10,1,1))

##test_period_plus_duration <- function() {
expect_identical(as.period(1) + as.duration(1), as.period(2))
expect_identical(as.period("2m2d") + as.duration(1), as.period("2m2d/00:00:00.000_000_001"))
expect_identical(as.period(1) + as.duration(0:9), as.period(1:10))
expect_identical(period(1:10,1,0) + as.duration(1), period(1:10,1,1))

##test_duration_plus_period <- function() {
expect_identical(as.duration(1) + as.period(1), as.period(2))
expect_identical(as.duration(1) + as.period("2m2d"), as.period("2m2d/00:00:00.000_000_001"))
expect_identical(as.duration(0:9) + as.period(1), as.period(1:10))
expect_identical(as.duration(1) + period(1:10,1,0), period(1:10,1,1))

##test_numeric_plus_period <- function() {
expect_identical(as.period(1) + 1, as.period(2))
expect_identical(as.period("2m2d") + 1, as.period("2m2d/00:00:00.000_000_001"))
expect_identical(0.0:9.0 + as.period(1), as.period(1:10))
expect_identical(1.0 + period(1:10,1,0), period(1:10,1,1))

##test_any_plus_period <- function() {
expect_error("a" + as.period(1), "invalid operand types")

##test_period_plus_any <- function() {
expect_error(as.period(1) + any, "invalid operand types")


## *
##test_period_times_numeric <- function() {
expect_identical(as.period(1) * 3, as.period(3))
expect_identical(as.period("1m1d") * 3, as.period("3m3d"))
expect_identical(period(1,1,1) * 1:10, period(1:10,1:10,1:10))
expect_identical(period(1:10,1,1) * 3, period(1:10 * 3, 3, 3))

##test_period_times_integer64 <- function() {
expect_identical(as.period(1) * as.integer64(3), as.period(3))
expect_identical(period(1,1,1) * as.integer64(1:10), period(1:10,1:10,1:10))
expect_identical(period(1:10,1,1) * as.integer64(3), period(1:10 * 3, 3, 3))

##test_numeric_times_period <- function() {
expect_identical(3 * as.period(1), as.period(3))
expect_identical(4.5 * as.period("10d"), as.period("45d"))
expect_identical(1:10.0 * as.period(1), as.period(1:10))
expect_identical(1 * as.period(1:10), as.period(1:10))

##test_integer64_times_period <- function() {
expect_identical(as.integer64(3) * as.period(1), as.period(3))
expect_identical(as.integer64(3) * as.period("1m1d"), as.period("3m3d"))
expect_identical(as.integer64(1:10) * as.period(1), as.period(1:10))
expect_identical(as.integer64(1) * as.period(1:10), as.period(1:10))

##test_character_times_period <- function() {
expect_error("a"   * as.period(1), "invalid operand types")
expect_error("123" * as.period(1), "invalid operand types")

##test_period_times_character <- function() {
expect_error(as.period(1) * "a",   "invalid operand types")
expect_error(as.period(1) * "123", "invalid operand types")


## /
##test_period_div_numeric <- function() {
expect_identical(as.period(4) / 3, as.period(1))
expect_identical(as.period("5m5d") / 2.5, as.period("2m2d"))
expect_identical(as.period(4) / c(4,2,1), as.period(c(1,2,4)))
expect_identical(as.period(4:2) / c(4,2,1), as.period(c(1,1,2)))
expect_error(as.period("2m") / 0, "divide by zero")

##test_period_div_integer64 <- function() {
expect_identical(as.period(4) / as.integer64(3), as.period(1))
expect_identical(as.period("5m5d") / as.integer64(2), as.period("2m2d"))
expect_identical(as.period(4) / as.integer64(c(4,2,1)), as.period(c(1,2,4)))
expect_identical(as.period(4:2) / as.integer64(c(4,2,1)), as.period(c(1,1,2)))

##test_period_div_integer <- function() {
expect_identical(as.period(4) / as.integer(3), as.period(1))
expect_identical(as.period("5m5d") / as.integer(2), as.period("2m2d"))
expect_identical(as.period(4) / as.integer(c(4,2,1)), as.period(c(1,2,4)))
expect_identical(as.period(4:2) / as.integer(c(4,2,1)), as.period(c(1,1,2)))

##test_period_div_any <- function() {
expect_error(as.period(1) / "a", "invalid operand types")

##test_any_div_period <- function() {
expect_error("a" / as.period(1), "invalid operand types")

##test_Logic_period_any <- function() {
expect_error(as.period(1) | "a", "operation not defined for \\\"period\\\" objects")

##test_Logic_any_period <- function() {
expect_error("a" | as.period(1), "operation not defined for \\\"period\\\" objects")


## Math/Math2/Summary/Complex
##test_period_Math <- function() {
## is that right? LLL
expect_error(abs(as.period(1)), "operation not defined for \"period\" objects")  

##test_period_Math2 <- function() {
expect_error(round(as.period(1)), "operation not defined for \"period\" objects")  

##test_period_Summary  <- function() {
expect_error(min(as.period(1)), "invalid 'type' \\(period\\) of argument")
expect_error(max(as.period(1)), "invalid 'type' \\(period\\) of argument")

##test_period_Complex  <- function() {
expect_error(Arg(as.period(1)), "operation not defined for \"period\" objects")  

##test_binary_plus_period_nanotime <- function() {
expect_error(as.period(1) + nanotime(1), "invalid operand types")

##test_binary_plus_nanotime_period <- function() {
expect_error(nanotime(1) + as.period(1), "binary '\\+' is not defined for \\\"nanotime\\\" and \\\"period\\\" objects; instead use \\\"plus\\(e1, e2, tz\\)\\\"")


## Compare
## ----------

##test_period_eq_period <- function() {
expect_true(as.period(1) == as.period(1))
expect_true(as.period("1d") == as.period("1d"))
expect_true(!(as.period(1) == as.period(2)))
expect_identical(as.period(1:10) == as.period(1:10), rep(TRUE, 10))

##test_period_ne_period <- function() {
expect_true(as.period(1) != as.period(2))
expect_true(as.period("1d") != as.period("2d"))
expect_true(!(as.period(1) != as.period(1)))
expect_identical(as.period(1:10) != as.period(1:10), rep(FALSE, 10))

##test_period_eq_any <- function() {
expect_error(as.period(1) == "a", "operation not defined for \"period\" objects")  

##test_any_eq_period <- function() {
expect_error("a" == as.period(1), "operation not defined for \"period\" objects")  

##test_all.equal <- function() {
expect_true(all.equal(as.period(1), as.period(1)))
expect_true(all.equal(as.period(1:10), as.period(1:10)))
expect_true(all.equal(as.period("1m1d"), as.period("1d") + as.period("1m")))


## names (in general)
##test_period_get_names <- function() {
a <- as.period(1:10)
names(a) <- "b"
expect_identical(names(a), c("b", rep(as.character(NA), 9)))
    
##test_period_set_names <- function() {
names <- c("a","b","c","d")
pp <- as.period(1:4)
names(pp) <- names
expect_identical(names(pp), names)
names(pp)[1] <- "x"
expect_identical(names(pp), c("x","b","c","d"))
    

##test_period_c <- function() {
pp <- c(x=as.period(1), y=as.period(2))
expect_identical(names(pp), c("x","y"))
expect_identical(pp[1], c(x=as.period(1)))
expect_identical(pp[2], c(y=as.period(2)))

pp <- c(as.period(1:10), as.period(11:20))
expect_identical(pp, as.period(1:20))



## plus/minus with 'nanotime':

##test_plus_nanotime_period <- function() {
nt <- nanotime("2018-01-01T05:00:00.000000000+00")
p  <- c(p=as.period("4m"))
tz <- "America/New_York"
expected <- c(p=nanotime("2018-05-01T00:00:00.000000000-04:00"))
expect_identical(plus(nt, p, tz), expected)

##test_plus_period_nanotime <- function() {
nt <- nanotime("2018-01-01T05:00:00.000000000+00")
p  <- c(p=as.period("4m"))
tz <- "America/New_York"
expected <- c(p=nanotime("2018-05-01T00:00:00.000000000-04:00"))
expect_identical(plus(p, nt, tz), expected)

##test_minus_nanotime_period <- function() {
nt <- c(p1=nanotime("2018-05-01T00:00:00.000000000-04:00"))
p  <- c(p2=as.period("4m"))
tz <- "America/New_York"
expected <- c(p1=nanotime("2018-01-01T00:00:00.000000000-05:00"))
expect_identical(minus(nt, p, tz), expected)

##test_minus_period_nanotime <- function() {
nt <- nanotime("2018-05-01T00:00:00.000000000-04:00")
p  <- as.period("4m")
tz <- "America/New_York"
expect_error(minus(p, nt, tz), "operation not defined for \"period\" objects")



## plus/minus with 'nanoival':

##test_plus_nanoival_period <- function() {
start <- nanotime("2018-01-01T05:00:00.000000000+00")
end <- nanotime("2018-01-01T23:00:00.000000000+00")
ni <- nanoival(start, end)
p  <- as.period("4m")
tz <- "America/New_York"
expected <- as.nanoival("+2018-05-01T00:00:00.000000000-04:00 -> 2018-05-01T18:00:00.000000000-04:00-")
expect_identical(plus(ni, p, tz), expected)

##test_plus_nanoival_period_pre_1970 <- function() {
start <- nanotime("1969-01-01T05:00:00.000000000+00")
end <- nanotime("1969-01-01T23:00:00.000000000+00")
ni <- nanoival(start, end)
p  <- as.period("4m")
tz <- "America/New_York"
expected <- as.nanoival("+1969-05-01T00:00:00.000000000-04:00 -> 1969-05-01T18:00:00.000000000-04:00-")
expect_identical(plus(ni, p, tz), expected)

##test_plus_period_nanoival <- function() {
start <- nanotime("2018-01-01T05:00:00.000000000+00")
end <- nanotime("2018-01-01T23:00:00.000000000+00")
ni <- nanoival(start, end)
p  <- c(a=as.period("4m"))
tz <- "America/New_York"
expected <- c(a=as.nanoival("+2018-05-01T00:00:00.000000000-04:00 -> 2018-05-01T18:00:00.000000000-04:00-"))
expect_identical(plus(p, ni, tz), expected)

##test_minus_nanoival_period <- function() {
start <- nanotime("2018-05-01T05:00:00.000000000-04")
end <- nanotime("2018-05-01T23:00:00.000000000-04")
ni <- c(a=nanoival(start, end))
p  <- as.period("4m")
tz <- "America/New_York"
expected <- c(a=as.nanoival("+2018-01-01T05:00:00.000000000-05:00 -> 2018-01-01T23:00:00.000000000-05:00-"))
expect_identical(minus(ni, p, tz), expected)

##test_minus_period_nanoival <- function() {
start <- nanotime("2018-05-01T05:00:00.000000000-04")
end <- nanotime("2018-05-01T23:00:00.000000000-04")
ni <- nanoival(start, end)
p  <- as.period("4m")
tz <- "America/New_York"
expect_error(minus(p, ni, tz), "operation not defined for \"period\" objects")

