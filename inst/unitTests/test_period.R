## constructors
test_as.period_character <- function() {
    p1 <- as.period("1m1d")
    checkEquals(period.day(p1), 1, tolerance=0)
    checkEquals(period.month(p1), 1, tolerance=0)
    checkEquals(period.duration(p1), as.duration(0), tolerance=0)

    checkEquals(as.period("1y"), as.period("12m"), tolerance=0)
    checkEquals(as.period("2y"), as.period("24m"), tolerance=0)
    checkEquals(as.period("1w"), as.period("7d"), tolerance=0)
    checkEquals(as.period("2w"), as.period("14d"), tolerance=0)
    
    p2 <- as.period("2m1d/00:01:01.1")
    checkEquals(period.month(p2), 2, tolerance=0)
    checkEquals(period.day(p2), 1, tolerance=0)
    checkEquals(period.duration(p2), as.duration("00:01:01.1"), tolerance=0)

    p3 <- as.period(c("1d","2d","3d"))
    checkEquals(period.day(p3[1]), 1, tolerance=0)
    checkEquals(period.day(p3[2]), 2, tolerance=0)
    checkEquals(period.day(p3[3]), 3, tolerance=0)    
}
test_as.period_integer64 <- function() {
    p1 <- as.period(as.integer64(1:10))
    checkEquals(period.duration(p1), as.duration(1:10), tolerance=0)
}
test_as.period_integer <- function() {
    p1 <- as.period(1:10)
    checkEquals(period.duration(p1), as.duration(1:10), tolerance=0)
}
test_as.period_numeric <- function() {
    p1 <- as.period(1.0:10)
    checkEquals(period.duration(p1), as.duration(1:10), tolerance=0)
}
test_as.period_duration <- function() {
    p1 <- as.period(as.duration(1))
    checkEquals(p1, as.period("00:00:00.1"))
}
test_period <- function() {
    checkEquals(period(0,0,1), as.period(1), tolerance=0)
    checkEquals(period(0,0,1:10), as.period(1:10), tolerance=0)
    checkEquals(period(1,1,0:9), as.period(paste0("1m1d/00:00:00.000_000_00", 0:9)), tolerance=0)
}

## accessors:
test_period.day <- function() {
    p1 <- as.period(paste0(1:10, "d"))
    checkEquals(period.day(p1), 1:10, tolerance=0)
}
test_period.month <- function() {
    p1 <- as.period(paste0(1:10, "m"))
    checkEquals(period.month(p1), 1:10, tolerance=0)
}
test_period.day <- function() {
    p1 <- as.period(1:10)
    checkEquals(period.duration(p1), as.duration(1:10), tolerance=0)
}

## show/print/as.character
test_show <- function() {
  p1 <- show(as.period("1m1d/1:00:00.1"))
  checkEquals(p1, "1m1d/01:00:00.100")
}
test_print <- function() {
  p1 <- print(as.period("2m2d/2:02:02.2"))
  checkEquals(p1, "2m2d/02:02:02.200")
}
test_as.character <- function() {
  p1 <- as.character(as.period("2m2d/2:02:02.20001"))
  checkEquals(p1, "2m2d/02:02:02.200_010")
}

## subset:
test_subset_int <- function() {
    p1 <- as.period("1m1d/00:00:01")
    p2 <- as.period("2m2d/00:00:02") 
    p3 <- as.period("3m3d/00:00:03") 
    p4 <- as.period("4m4d/00:00:04") 
    pp <- c(p1, p2, p3, p4)
    checkEquals(pp[1], p1, tolerance=0)
    checkEquals(pp[2], p2, tolerance=0)
    checkEquals(pp[3], p3, tolerance=0)
    checkEquals(pp[4], p4, tolerance=0)
    checkEquals(pp[1:4], pp, tolerance=0)
    checkEquals(pp[2:3], c(p2,p3), tolerance=0)
}
test_subset_logical <- function() {
    p1 <- as.period("1m1d/00:00:01")
    p2 <- as.period("2m2d/00:00:02") 
    p3 <- as.period("3m3d/00:00:03") 
    p4 <- as.period("4m4d/00:00:04") 
    pp <- c(p1, p2, p3, p4)
    checkEquals(pp[c(T,F,F,F)], p1, tolerance=0)
    checkEquals(pp[c(F,T,F,F)], p2, tolerance=0)
    checkEquals(pp[c(F,F,T,F)], p3, tolerance=0)
    checkEquals(pp[c(F,F,F,T)], p4, tolerance=0)
    checkEquals(pp[TRUE], pp, tolerance=0)
    checkEquals(pp[c(F,T,T,F)], c(p2,p3), tolerance=0)
}
test_subset_character <- function() {
    pp <- c(x=as.period(1), y=as.period(2), tolerance=0)
    checkEquals(pp["x"], c(x=as.period(1)), tolerance=0)
    checkEquals(pp["y"], c(y=as.period(2)), tolerance=0)
    ## checkEquals(pp["a"], as.period(as.integer64(NA)))    LLL
}

## subassign
test_subassign_logical <- function() {
    x <- as.period(1:10)
    x[c(T,T,T,F,F,F,F,F,F,F)] <- as.period(2:4)
    checkEquals(x, as.period(c(2:4, 4:10)), tolerance=0)
    x <- as.period(paste0(1:10, "d"))
    x[c(T,T,T,F,F,F,F,F,F,F)] <- as.period(paste0(2:4, "d"))
    checkEquals(x, as.period(paste0(c(2:4, 4:10), "d")), tolerance=0)
}
test_subassign_numeric <- function() {
    x <- as.period(1:10)
    x[1:3] <- as.period(2:4)
    x[4:10] <- as.period(5:11)
    checkEquals(x, as.period(2:11), tolerance=0)
    x <- as.period(paste0(1:10, "d"))
    x[1:3] <- as.period(paste0(2:4, "d"))
    x[4:10] <- as.period(paste0(5:11, "d"))
    checkEquals(x, as.period(paste0(2:11, "d")), tolerance=0)
}
test_subsassign_character <- function() {
    pp <- c(a=as.period(1), b=as.period(2), c=as.period(3), d=as.period(4))
    pp[c("b", "c")] <- as.period(20:21)
    expected <- c(a=as.period(1), b=as.period(20), c=as.period(21), d=as.period(4))
    checkEquals(pp, expected, tolerance=0)
}

## ops
## -
test_period_minus_period <- function() {
    checkEquals(as.period("2m2d") - as.period("1m1d"), as.period("1m1d"), tolerance=0)
    checkEquals(as.period("-1m-1d/-00:00:01") - as.period("1m1d/00:00:01"),
                as.period("-2m-2d/-00:00:02"), tolerance=0)
}
test_period_minus_period_vector <- function() {
    checkEquals(as.period("1d") - as.period(paste0(1:10, "d")),
                as.period(paste0(0:-9,"d")), tolerance=0)
    checkEquals(as.period(paste0(1:10, "d")) - as.period("1d"),
                as.period(paste0(0:9,"d")), tolerance=0)
    checkEquals(as.period(paste0(1:10, "d")) - as.period(paste0(0:9, "d")),
                as.period(paste0(rep(1,10),"d")), tolerance=0)
}
test_period_minus_numeric <- function() {
    checkEquals(as.period("2m2d") - 1,
                as.period("2m2d/-00:00:00.000_000_001"), tolerance=0)
    checkEquals(as.period(paste0(1:10,"m2d")) - 1,
                as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")), tolerance=0)
    checkEquals(as.period("12m2d") - 1:9.0,
                as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)), tolerance=0)
}
test_period_minus_duration <- function() {
    checkEquals(as.period("2m2d") - as.duration(1), as.period("2m2d/-00:00:00.000_000_001"))
    checkEquals(as.period(paste0(1:10,"m2d")) - as.duration(1),
                as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")), tolerance=0)
    checkEquals(as.period("12m2d") - as.duration(1:9),
                as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)), tolerance=0)
}
test_period_minus_integer <- function() {
    checkEquals(as.period("2m2d") - as.integer(1), as.period("2m2d/-00:00:00.000_000_001"))
    checkEquals(as.period(paste0(1:10,"m2d")) - as.integer(1),
                as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")), tolerance=0)
    checkEquals(as.period("12m2d") - as.integer(1:9),
                as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)), tolerance=0)
}
test_period_minus_integer64 <- function() {
    checkEquals(as.period("2m2d") - as.integer64(1), as.period("2m2d/-00:00:00.000_000_001"))
    checkEquals(as.period(paste0(1:10,"m2d")) - as.integer64(1),
                as.period(paste0(1:10, "m2d/-00:00:00.000_000_001")), tolerance=0)
    checkEquals(as.period("12m2d") - as.integer64(1:9),
                as.period(paste0("12m2d/-00:00:00.000_000_00", 1:9)), tolerance=0)
}
test_numeric_minus_period <- function() {
    checkEquals(1 - as.period("1m1d"), as.period("-1m-1d/00:00:00.000_000_001"), tolerance=0)
    checkEquals(1:10 - as.period("1m1d"), period(-1,-1,1:10), tolerance=0)
    checkEquals(1 - period(1:10,1,1), period(-1:-10, -1, 0), tolerance=0)
}
test_integer64_minus_period <- function() {
    checkEquals(as.integer64(1) - as.period("1m1d"),
                as.period("-1m-1d/00:00:00.000_000_001"), tolerance=0)
    checkEquals(as.integer64(1:10) - as.period("1m1d"), period(-1,-1,1:10), tolerance=0)
    checkEquals(as.integer64(1) - period(1:10,1,1), period(-1:-10, -1, 0), tolerance=0)
}
test_duration_minus_period <- function() {
    checkEquals(as.duration(1) - as.period("1m1d"),
                as.period("-1m-1d/00:00:00.000_000_001"), tolerance=0)
    checkEquals(as.duration(1:10) - as.period("1m1d"), period(-1,-1,1:10), tolerance=0)
    checkEquals(as.duration(1) - period(1:10,1,1), period(-1:-10, -1, 0), tolerance=0)
}
test_period_minus_any <- function() {
    checkException(as.period(1) - "a", "invalid operand types")
}
test_any_minus_period <- function() {
    checkException("a" - as.period(1), "invalid operand types")
}

## +
test_period_plus_period <- function() {
    checkEquals(as.period("2m2d") + as.period("1m1d"), as.period("3m3d"), tolerance=0)
    checkEquals(as.period("-1m-1d/00:00:01") + as.period("1m1d/00:00:01"),
                as.period("0m0d/00:00:02"), tolerance=0)
    checkEquals(period(1,1,1:10) + period(1,1,0), period(1,1,1:10))
    checkEquals(period(1,1,0) + period(1,1,1:10), period(1,1,1:10))
}
test_integer64_plus_period <- function() {
    checkEquals(as.integer64(1) + as.period(1), as.period(2), tolerance=0)
    checkEquals(as.integer64(0:9) + as.period(1), as.period(1:10), tolerance=0)
    checkEquals(as.integer64(1) + period(1:10,1,0), period(1:10,1,1), tolerance=0)
}
test_period_plus_integer64 <- function() {
    checkEquals(as.period(1) + as.integer64(1), as.period(2), tolerance=0)
    checkEquals(as.period("2m2d") + as.integer64(1), as.period("2m2d/00:00:00.000_000_001"), tolerance=0)
    checkEquals(as.period(1) + as.integer64(0:9), as.period(1:10), tolerance=0)
    checkEquals(period(1:10,1,0) + as.integer64(1), period(1:10,1,1), tolerance=0)
}
test_period_plus_duration <- function() {
    checkEquals(as.period(1) + as.duration(1), as.period(2), tolerance=0)
    checkIdentical(as.period("2m2d") + as.duration(1), as.period("2m2d/00:00:00.000_000_001"))
    checkEquals(as.period(1) + as.duration(0:9), as.period(1:10), tolerance=0)
    checkEquals(period(1:10,1,0) + as.duration(1), period(1:10,1,1), tolerance=0)
}
test_duration_plus_period <- function() {
    checkEquals(as.duration(1) + as.period(1), as.period(2), tolerance=0)
    checkIdentical(as.duration(1) + as.period("2m2d"), as.period("2m2d/00:00:00.000_000_001"))
    checkEquals(as.duration(0:9) + as.period(1), as.period(1:10), tolerance=0)
    checkEquals(as.duration(1) + period(1:10,1,0), period(1:10,1,1), tolerance=0)
}
test_numeric_plus_period <- function() {
    checkEquals(as.period(1) + 1, as.period(2), tolerance=0)
    checkEquals(as.period("2m2d") + 1, as.period("2m2d/00:00:00.000_000_001"), tolerance=0)
    checkEquals(0.0:9.0 + as.period(1), as.period(1:10), tolerance=0)
    checkEquals(1.0 + period(1:10,1,0), period(1:10,1,1), tolerance=0)
}
test_any_plus_period <- function() {
    checkException("a" + as.period(1), "invalid operand types")
}
test_period_plus_any <- function() {
    checkException(as.period(1) + any, "invalid operand types")
}

## *
test_period_times_numeric <- function() {
    checkEquals(as.period(1) * 3, as.period(3), tolerance=0)
    checkEquals(as.period("1m1d") * 3, as.period("3m3d"), tolerance=0)
    checkEquals(period(1,1,1) * 1:10, period(1:10,1:10,1:10), tolerance=0)
    checkEquals(period(1:10,1,1) * 3, period(1:10 * 3, 3, 3), tolerance=0)
}
test_period_times_integer64 <- function() {
    checkEquals(as.period(1) * as.integer64(3), as.period(3), tolerance=0)
    checkEquals(period(1,1,1) * as.integer64(1:10), period(1:10,1:10,1:10), tolerance=0)
    checkEquals(period(1:10,1,1) * as.integer64(3), period(1:10 * 3, 3, 3), tolerance=0)
}
test_numeric_times_period <- function() {
    checkEquals(3 * as.period(1), as.period(3), tolerance=0)
    checkEquals(4.5 * as.period("10d"), as.period("45d"), tolerance=0)
    checkEquals(1:10.0 * as.period(1), as.period(1:10), tolerance=0)
    checkEquals(1 * as.period(1:10), as.period(1:10), tolerance=0)
}
test_integer64_times_period <- function() {
    checkEquals(as.integer64(3) * as.period(1), as.period(3), tolerance=0)
    checkEquals(as.integer64(3) * as.period("1m1d"), as.period("3m3d"), tolerance=0)
    checkEquals(as.integer64(1:10) * as.period(1), as.period(1:10), tolerance=0)
    checkEquals(as.integer64(1) * as.period(1:10), as.period(1:10), tolerance=0)
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
    checkEquals(as.period(4) / 3, as.period(1), tolerance=0)
    checkEquals(as.period("5m5d") / 2.5, as.period("2m2d"), tolerance=0)
    checkEquals(as.period(4) / c(4,2,1), as.period(c(1,2,4)), tolerance=0)
    checkEquals(as.period(4:2) / c(4,2,1), as.period(c(1,1,2)), tolerance=0)
}
test_period_div_integer64 <- function() {
    checkEquals(as.period(4) / as.integer64(3), as.period(1), tolerance=0)
    checkEquals(as.period("5m5d") / as.integer64(2), as.period("2m2d"), tolerance=0)
    checkEquals(as.period(4) / as.integer64(c(4,2,1)), as.period(c(1,2,4)), tolerance=0)
    checkEquals(as.period(4:2) / as.integer64(c(4,2,1)), as.period(c(1,1,2)), tolerance=0)
}
test_period_div_integer <- function() {
    checkEquals(as.period(4) / as.integer(3), as.period(1), tolerance=0)
    checkEquals(as.period("5m5d") / as.integer(2), as.period("2m2d"), tolerance=0)
    checkEquals(as.period(4) / as.integer(c(4,2,1)), as.period(c(1,2,4)), tolerance=0)
    checkEquals(as.period(4:2) / as.integer(c(4,2,1)), as.period(c(1,1,2)), tolerance=0)
}
test_period_div_any <- function() {
    checkException(as.period(1) / "a", "operation not defined for \"period\" objects")
}
test_any_div_period <- function() {
    checkException("a" / as.period(1), "operation not defined for \"period\" objects")
}

## Math/Math2/Summary/Complex
test_period_Math <- function() {
    ## is that right? LLL
    checkException(abs(as.period(1)), "operation not defined for \"period\" objects")  
}
test_period_Math2 <- function() {
    checkException(round(as.period(1)), "operation not defined for \"period\" objects")  
}
test_period_Summary  <- function() {
    checkException(min(as.period(1)), "operation not defined for \"period\" objects")  
    checkException(max(as.period(1)), "operation not defined for \"period\" objects")  
}
test_period_Complex  <- function() {
    checkException(Arg(as.period(1)), "operation not defined for \"period\" objects")  
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
    checkEquals(as.period(1:10) != as.period(1:10), rep(FALSE, 10), tolerance=0)
}
test_period_eq_any <- function() {
    checkException(as.period(1) == "a", "operation not defined for \"period\" objects")  
}
test_any_eq_period <- function() {
    checkException("a" == as.period(1), "operation not defined for \"period\" objects")  
}
test_all.equal <- function() {
    checkTrue(all.equal(as.period(1), as.period(1)))
    checkTrue(all.equal(as.period(1:10), as.period(1:10)))
    checkTrue(all.equal(as.period("1m1d"), as.period("1d") + as.period("1m")))
}

## names (in general)
test_period_get_names <- function() {
    a <- as.period(1:10)
    names(a) <- "b"
    checkEquals(names(a), c("b", rep(as.character(NA), 9)))
}    
test_period_set_names <- function() {
    names <- c("a","b","c","d")
    pp <- as.period(1:4)
    names(pp) <- names
    checkEquals(names(pp), names)
    names(pp)[1] <- "x"
    checkEquals(names(pp), c("x","b","c","d"))
}    

test_period_c <- function() {
    pp <- c(x=as.period(1), y=as.period(2))
    checkEquals(names(pp), c("x","y"))
    checkEquals(pp[1], c(x=as.period(1)), tolerance=0)
    checkEquals(pp[2], c(y=as.period(2)), tolerance=0)

    pp <- c(as.period(1:10), as.period(11:20))
    checkEquals(pp, as.period(1:20), tolerance=0)
}
