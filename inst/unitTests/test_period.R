## constructors
test_as.period_character <- function() {
    p1 <- as.period("1m1d")
    checkEquals(period.day(p1), 1)
    checkEquals(period.month(p1), 1)
    checkEquals(period.duration(p1), as.duration(0))

    checkEquals(as.period("1y"), as.period("12m"))
    checkEquals(as.period("2y"), as.period("24m"))
    checkEquals(as.period("1w"), as.period("7d"))
    checkEquals(as.period("2w"), as.period("14d"))
    
    p2 <- as.period("2m1d/00:01:01.1")
    checkEquals(period.month(p2), 2)
    checkEquals(period.day(p2), 1)
    checkEquals(period.duration(p2), as.duration("00:01:01.1"))

    p3 <- as.period(c("1d","2d","3d"))
    checkEquals(period.day(p3[1]), 1)
    checkEquals(period.day(p3[2]), 2)
    checkEquals(period.day(p3[3]), 3)    
}
test_as.period_integer64 <- function() {
    p1 <- as.period(as.integer64(1:10))
    checkEquals(period.duration(p1), as.duration(1:10))
}
test_as.period_integer <- function() {
    p1 <- as.period(1:10)
    checkEquals(period.duration(p1), as.duration(1:10))
}
test_as.period_numeric <- function() {
    p1 <- as.period(1.0:10)
    checkEquals(period.duration(p1), as.duration(1:10))
}

## accessors:

test_period.day <- function() {
    p1 <- as.period(paste0(1:10, "d"))
    checkEquals(period.day(p1), 1:10)
}
test_period.month <- function() {
    p1 <- as.period(paste0(1:10, "m"))
    checkEquals(period.month(p1), 1:10)
}
test_period.day <- function() {
    p1 <- as.period(1:10)
    checkEquals(period.duration(p1), as.duration(1:10))
}

## subset:

test_subset_int <- function() {
    p1 <- as.period("1m1d/00:00:01")
    p2 <- as.period("2m2d/00:00:02") 
    p3 <- as.period("3m3d/00:00:03") 
    p4 <- as.period("4m4d/00:00:04") 
    pp <- c(p1, p2, p3, p4)
    checkEquals(pp[1], p1)
    checkEquals(pp[2], p2)
    checkEquals(pp[3], p3)
    checkEquals(pp[4], p4)
    checkEquals(pp[1:4], pp)
    checkEquals(pp[2:3], c(p2,p3))
}
test_subset_logical <- function() {
    p1 <- as.period("1m1d/00:00:01")
    p2 <- as.period("2m2d/00:00:02") 
    p3 <- as.period("3m3d/00:00:03") 
    p4 <- as.period("4m4d/00:00:04") 
    pp <- c(p1, p2, p3, p4)
    checkEquals(pp[c(T,F,F,F)], p1)
    checkEquals(pp[c(F,T,F,F)], p2)
    checkEquals(pp[c(F,F,T,F)], p3)
    checkEquals(pp[c(F,F,F,T)], p4)
    checkEquals(pp[TRUE], pp)
    checkEquals(pp[c(F,T,T,F)], c(p2,p3))
}
test_subset_character <- function() {
    pp <- c(x=as.period(1), y=as.period(2))
    checkEquals(pp["x"], c(x=as.period(1)))
    checkEquals(pp["y"], c(y=as.period(2)))
    ## checkEquals(pp["a"], as.period(as.integer64(NA)))    
}

## subassign
test_subassign_logical <- function() {
    x <- as.period(1:10)
    x[c(T,T,T,F,F,F,F,F,F,F)] <- as.period(2:4)
    checkEquals(x, as.period(c(2:4, 4:10)))
    x <- as.period(paste0(1:10, "d"))
    x[c(T,T,T,F,F,F,F,F,F,F)] <- as.period(paste0(2:4, "d"))
    checkEquals(x, as.period(paste0(c(2:4, 4:10), "d")))
}
test_subassign_numeric <- function() {
    x <- as.period(1:10)
    x[1:3] <- as.period(2:4)
    x[4:10] <- as.period(5:11)
    checkEquals(x, as.period(2:11))
    x <- as.period(paste0(1:10, "d"))
    x[1:3] <- as.period(paste0(2:4, "d"))
    x[4:10] <- as.period(paste0(5:11, "d"))
    checkEquals(x, as.period(paste0(2:11, "d")))
}
test_subsassign_character <- function() {
    pp <- c(a=as.period(1), b=as.period(2), c=as.period(3), d=as.period(4))
    pp[c("b", "c")] <- as.period(20:21)
    expected <- c(a=as.period(1), b=as.period(20), c=as.period(21), d=as.period(4))
    checkEquals(pp, expected)
}

## ops
## -
test_period_minus_period <- function() {
    checkEquals(as.period("2m2d") - as.period("1m1d"), as.period("1m1d"))
    checkEquals(as.period("-1m-1d/-00:00:01") - as.period("1m1d/00:00:01"),
                as.period("-2m-2d/-00:00:02"))
}
test_period_minus_period_vector <- function() {
    checkEquals(as.period("1d") - as.period(paste0(1:10, "d")), as.period(paste0(0:-9,"d")))
    checkEquals(as.period(paste0(1:10, "d")) - as.period("1d"), as.period(paste0(0:9,"d")))
    checkEquals(as.period(paste0(1:10, "d")) - as.period(paste0(0:9, "d")),
                as.period(paste0(rep(1,10),"d")))
}

test_period_minus_numeric <- function() {
    checkEquals(as.period("2m2d") - 1, as.period("2m2d/-00:00:00.000_000_001"))
}
test_period_minus_integer64 <- function() {
    checkEquals(as.period("2m2d") - as.integer(1), as.period("2m2d/-00:00:00.000_000_001"))
}
test_numeric_minus_period <- function() {
    checkEquals(1 - as.period("1m1d"), as.period("1m1d/00:00:00.000_000_001"))
}
## +
test_period_plus_period <- function() {
    checkEquals(as.period("2m2d") +as.period("1m1d"), as.period("3m3d"))
    checkEquals(as.period("-1m-1d/00:00:01") + as.period("1m1d/00:00:01"),
                as.period("0m0d/00:00:02"))
}
test_integer64_plus_period <- function() {
    checkEquals(as.integer64(1) + as.period(1), as.period(2))
}
test_period_plus_integer64 <- function() {
    checkEquals(as.period(1) + as.integer64(1), as.period(2))
    checkEquals(as.period("2m2d") + as.integer(1), as.period("2m2d/00:00:00.000_000_001"))
}
test_numeric_plus_period <- function() {
    checkEquals(as.period(1) + 1, as.period(2))
    checkEquals(as.period("2m2d") + 1, as.period("2m2d/00:00:00.000_000_001"))
}
test_character_plus_period <- function() {
    checkException("a" + as.period(1), "invalid operand types")
}

## *
test_period_times_numeric <- function() {
    checkEquals(as.period(1) * 3, as.period(3))
    checkEquals(as.period("1m1d") * 3, as.period("3m3d"))
}
test_period_times_integer64 <- function() {
    checkEquals(as.period(1) * as.integer64(3), as.period(3))
}
test_numeric_times_period <- function() {
    checkEquals(3 * as.period(1), as.period(3))
    checkEquals(4.5 * as.period("10d"), as.period("45d"))
}
test_integer64_times_period <- function() {
    checkEquals(as.integer64(3) * as.period(1), as.period(3))
    checkEquals(as.integer64(3) * as.period("1m1d"), as.period("3m3d"))
}
test_character_times_period <- function() {
    checkException("a"   * as.period(1), "invalid operand types")
    checkException("123" * as.period(1), "invalid operand types")
}
test_period_times_character <- function() {
    checkException(as.period(1) * "a",   "invalid operand types")
    checkException(as.period(1) * "123", "invalid operand types")
}
## /
test_period_div_numeric <- function() {
    checkEquals(as.period(4) / 3, as.period(1))
    checkEquals(as.period("5m5d") / 2.5, as.period("2m2d"))
}
test_period_div_integer64 <- function() {
    checkEquals(as.period(4) / as.integer64(3), as.period(1))
    checkEquals(as.period("5m5d") / as.integer64(2), as.period("2m2d"))
}
test_period_div_integer <- function() {
    checkEquals(as.period(4) / as.integer(3), as.period(1))
    checkEquals(as.period("5m5d") / as.integer(2), as.period("2m2d"))
}
test_numeric_div_period <- function() {
    checkException(as.period(1) * "a", "operation not defined for \"period\" objects")
}

## Compare
## ----------

test_period_eq_period <- function() {
    checkTrue(as.period(1) == as.period(1))
    checkTrue(as.period("1d") == as.period("1d"))
    checkTrue(!(as.period(1) == as.period(2)))
    checkEquals(as.period(1:10) == as.period(1:10), rep(TRUE, 10))
}

test_period_ne_period <- function() {
    checkTrue(as.period(1) != as.period(2))
    checkTrue(as.period("1d") != as.period("2d"))
    checkTrue(!(as.period(1) != as.period(1)))
    checkEquals(as.period(1:10) != as.period(1:10), rep(FALSE, 10))
}

## names (in general)
test_period_names <- function() {
    a <- as.period(1:10)
    names(a) <- "b"
    checkEquals(names(a), c("b", rep(as.character(NA), 9)))
}    

test_period_c <- function() {
    pp <- c(x=as.period(1), y=as.period(2))
    checkEquals(names(pp), c("x","y"))
    checkEquals(pp[1], c(x=as.period(1)))
    checkEquals(pp[2], c(y=as.period(1)))

    pp <- c(as.period(1:10), as.period(11:20))
    checkEquals(pp, as.period(1:20))
}
