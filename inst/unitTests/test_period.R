## constructors
test_as.period_character <- function() {
    p1 <- as.period("1d")
    checkEquals(period.day(p1), 1)
    checkEquals(period.month(p1), 0)
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
}
test_period_minus_integer64 <- function() {
}
test_numeric_minus_period <- function() {
}
## +
test_period_plus_period <- function() {
}
test_integer64_plus_period <- function() {
}
test_period_plus_integer64 <- function() {
}
test_numeric_plus_period <- function() {
}
test_character_plus_period <- function() {
}
## *
test_period_times_numeric <- function() {
}
test_period_times_integer64 <- function() {
}
test_numeric_times_period <- function() {
}
test_integer64_times_period <- function() {
}
test_character_times_period <- function() {
}
test_period_times_character <- function() {
}
## /
test_period_div_numeric <- function() {
}
test_period_div_integer64 <- function() {
}
test_period_div_integer <- function() {
}
test_numeric_div_period <- function() {
}
