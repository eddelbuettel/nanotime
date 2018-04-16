milli <- 1e6
sec <- 1e9
minute <- 60 * sec
hour <- 60 * minute

## constructors
test_as.duration_character_and_numeric <- function() {
    checkIdentical(as.duration("1:00:00"), as.duration(hour))
    checkIdentical(as.duration("0:01:00"), as.duration(minute))
    checkIdentical(as.duration("0:00:01"), as.duration(sec))
    checkIdentical(as.duration("0:00:00.001"), as.duration(milli))
    checkIdentical(c(as.duration("1:00:00"), c(as.duration("2:00:00"))),
                c(as.duration(hour), as.duration(2*hour)))
}
test_as.duration_integer64 <- function() {
    checkIdentical(as.duration(as.integer64(hour)), as.duration(hour))
    checkIdentical(as.duration(c(a=as.integer64(hour), b=as.integer64(2*hour))),
                as.duration(c(a=hour, b=2*hour)))
}
test_as.duration_integer <- function() {
    checkIdentical(as.duration(as.integer(sec)), as.duration(sec))
    checkIdentical(as.duration(c(a=as.integer(milli), b=as.integer(2*milli))),
                as.duration(c(a=milli, b=2*milli)))
}
test_as.integer64 <- function() {
    checkIdentical(as.integer64(as.duration(hour)), as.integer64(hour))
    checkIdentical(as.integer64(as.duration(1:1000)), as.integer64(1:1000))
}

## subset:
test_subset_int <- function() {
    dd <- as.duration(1:10)
    checkIdentical(dd[1], as.duration(1))
}
test_subset_logical <- function() {
    dd <- as.duration(1:4)
    checkIdentical(dd[c(T,F,F,F)], dd[1])
    checkIdentical(dd[c(F,T,F,F)], dd[2])
    checkIdentical(dd[c(F,F,T,F)], dd[3])
    checkIdentical(dd[c(F,F,F,T)], dd[4])
    checkIdentical(dd[TRUE], dd)
    checkIdentical(dd[c(F,T,T,F)], dd[2:3])
}
test_subset_character <- function() {
    dd <- c(x=as.duration(1), y=as.duration(2))
    checkIdentical(dd["x"], c(x=as.duration(1)))
    checkIdentical(dd["y"], c(y=as.duration(2)))
    ## also check out of bound subset
    res <- as.duration(as.integer64(NA))
    names(res) <- NA
    checkIdentical(dd["a"], res)
}

## subassign:
test_subassign_logical <- function() {
    x <- as.duration(1:10)
    x[c(T,T,T,F,F,F,F,F,F,F)] <- as.duration(2:4)
    checkIdentical(x, as.duration(c(2:4, 4:10)))
}
test_subassign_numeric <- function() {
    x <- as.duration(1:10)
    x[1:3] <- as.duration(2:4)
    x[4:10] <- as.duration(5:11)
    checkIdentical(x, as.duration(2:11))
}
test_subsassign_character <- function() {
    dd <- c(a=as.duration(1), b=as.duration(2), c=as.duration(3), d=as.duration(4))
    dd[c("b", "c")] <- as.duration(20:21)
    expected <- c(a=as.duration(1), b=as.duration(20), c=as.duration(21), d=as.duration(4))
    checkIdentical(dd, expected)
}

## test names
test_get_names <- function() {
    dd <- c(a=as.duration(1), b=as.duration(2), c=as.duration(3), d=as.duration(4))
    checkIdentical(names(dd), c("a","b","c","d"))
}
test_set_names <- function() {
    names <- c("a","b","c","d")
    dd <- as.duration(1:4)
    names(dd) <- names
    checkIdentical(names(dd), names)
    names(dd)[1] <- "x"
    checkIdentical(names(dd), c("x","b","c","d"))
}

## test scalar/vector mix LLL

## ops
## -
test_duration_minus_duration <- function() {
    checkIdentical(as.duration("1:00:00") - as.duration("00:01:00"), as.duration("00:59:00"))
    checkIdentical(as.duration("-1:00:00") - as.duration("00:01:00"), as.duration("-01:01:00"))
    checkIdentical(as.duration("1:00:00.1") - as.duration("1:00:00"), as.duration("0.1"))
    checkIdentical(as.duration(1:10) - as.duration(0:9), as.duration(rep(1, 10)))
    checkIdentical(as.duration(1:10) - as.duration(1), as.duration(0:9))
    checkIdentical(c(a=as.duration(1), b=as.duration(1)) - as.duration(1),
                   as.duration(c(a=0, b=0)))
}
test_duration_minus_numeric <- function() {
    checkIdentical(as.duration("1:00:00") - minute, as.duration("00:59:00"))
    checkIdentical(as.duration(1:10) - 0.0:9, as.duration(rep(1, 10)))
    checkIdentical(as.duration(1:10) - 1, as.duration(0:9))
    checkIdentical(c(a=as.duration(1), b=as.duration(1)) - 1, as.duration(c(a=0, b=0)))
}
test_duration_minus_integer <- function() {
    checkIdentical(as.duration("1:00:00") - as.integer(milli), as.duration("00:59:59.999"))
    checkIdentical(as.duration(1:10) - 0:9, as.duration(rep(1, 10)))
    checkIdentical(as.duration(1:10) - as.integer(1), as.duration(0:9))
    checkIdentical(c(a=as.duration(1), b=as.duration(1)) - as.integer(1),
                   as.duration(c(a=0, b=0)))
}
test_duration_minus_integer64 <- function() {
    checkIdentical(as.duration("1:00:00") - as.integer64(minute), as.duration("00:59:00"))
    checkIdentical(as.duration(1:10) - as.integer64(0:9), as.duration(rep(1, 10)))
    checkIdentical(as.duration(1:10) - as.integer64(1), as.duration(0:9))
    checkIdentical(c(a=as.duration(1), b=as.duration(1)) - as.integer64(1),
                   as.duration(c(a=0, b=0)))
}
test_numeric_minus_duration <- function() {
    checkIdentical(2 - c(a=as.duration(1), b=as.duration(2)),
                   c(a=as.duration(1), b=as.duration(0)))
    checkIdentical(2 - c(a=as.integer64(1), b=as.integer64(2)),
                   c(a=as.integer64(1), b=as.integer64(0)))

}
test_character_minus_duration <- function() {
    checkException("a" - as.duration("1:00:00"), "invalid operand types")
}

## +
test_duration_plus_duration <- function() {
    checkIdentical(as.duration("1:00:00") + as.duration("00:01:00"), as.duration("01:01:00"))
    checkIdentical(as.duration("-1:00:00") + as.duration("00:01:00"), as.duration("-00:59:00"))
    checkIdentical(as.duration("-1:00:00.1") + as.duration("1:00:00"), as.duration("-0.1"))
    checkIdentical(-as.duration(1:10) + as.duration(0:9), as.duration(rep(-1, 10)))
    checkIdentical(as.duration(1:10) + as.duration(1), as.duration(2:11))
    checkIdentical(c(a=as.duration(1), b=as.duration(1)) + as.duration(1),
                   as.duration(c(a=2, b=2)))
}
test_integer64_plus_duration <- function() {
    checkIdentical(as.integer64(hour) + as.duration("1:00:00"), as.duration("2:00:00"))
}
test_duration_plus_integer64 <- function() {
    checkIdentical(as.duration("1:00:00") + as.integer64(hour), as.duration("2:00:00"))
}
test_numeric_plus_duration <- function() {
    checkIdentical(hour + as.duration("1:00:00"), as.duration("2:00:00"))
}
test_character_plus_duration <- function() {
    checkException("hello" + as.duration("1:00:00"), "invalid operand types")
}
## *
test_duration_times_numeric <- function() {
    checkIdentical(as.duration("00:01:00") * 3, as.duration("00:03:00"))
}
test_duration_times_integer64 <- function() {
    checkIdentical(as.duration("00:01:00") * as.integer64(3), as.duration("00:03:00"))
}
test_numeric_times_duration <- function() {
    checkIdentical(1.5 * as.duration("00:01:00"), as.duration("00:01:30"))
}
test_integer64_times_duration <- function() {
    checkIdentical(as.integer64(3) * as.duration("00:01:00"), as.duration("00:03:00"))
}
test_character_times_duration <- function() {
    checkException("hello" / as.duration("00:01:00"), "invalid operand types")
}
test_duration_times_character <- function() {
    checkException(as.duration("00:01:00") *  "hello", "invalid operand types")
}
## /
test_duration_div_numeric <- function() {
    checkIdentical(as.duration("00:03:00") / 3, as.duration("00:01:00"))
}
test_duration_div_integer64 <- function() {
    checkIdentical(as.duration("00:03:00") / as.integer64(3), as.duration("00:01:00"))
}
test_duration_div_integer <- function() {
    checkIdentical(as.duration("00:03:00") / as.integer(3), as.duration("00:01:00"))
}
test_numeric_div_duration <- function() {
    checkException(1.5 / as.duration("00:01:00"), "invalid operand types")
}

## unary
test_unary_minus <- function() {
    checkIdentical(-as.duration("00:01:00"), as.duration("-00:01:00"))
}

test_unary_plus <- function() {
    checkIdentical(+as.duration("00:01:00"), as.duration("00:01:00"))
}

