milli <- 1e6
sec <- 1e9
min <- 60 * sec
hour <- 60 * min

## constructors
test_as.duration_character_and_numeric <- function() {
    checkEquals(as.duration("1:00:00"), as.duration(hour))
    checkEquals(as.duration("0:01:00"), as.duration(min))
    checkEquals(as.duration("0:00:01"), as.duration(sec))
    checkEquals(as.duration("0:00:01.001"), as.duration(milli))
    checkEquals(c(as.duration("1:00:00"), c(as.duration("2:00:00"))),
                c(as.duration(hour), as.duration(2*hour)))
}
test_as.duration_integer64 <- function() {
    checkEquals(as.duration(as.integer64(hour)), as.duration(hour))
}
test_as.duration_integer <- function() {
    checkEquals(as.duration(as.integer(sec)), as.duration(sec))
}
test_as.integer64 <- function() {
    checkEquals(as.integer64(as.duration(hour)), as.integer64(hour))
}

## subset:
test_subset_int <- function() {
    dd <- as.duration(1:10)
    checkEquals(dd[1], as.duration(1))
}
test_subset_logical <- function() {
    dd <- as.duration(1:4)
    checkEquals(dd[c(T,F,F,F)], dd[1])
    checkEquals(dd[c(F,T,F,F)], dd[2])
    checkEquals(dd[c(F,F,T,F)], dd[3])
    checkEquals(dd[c(F,F,F,T)], dd[4])
    checkEquals(dd[TRUE], dd)
    checkEquals(dd[c(F,T,T,F)], dd[2:3])
}
test_subset_character <- function() {
    dd <- c(x=as.duration(1), y=as.duration(2))
    checkEquals(dd["x"], c(x=as.duration(1)))
    checkEquals(dd["y"], c(y=as.duration(2)))
    ## checkEquals(pp["a"], as.duration(as.integer64(NA)))    LLL
}

## subassign:
test_subassign_logical <- function() {
    x <- as.duration(1:10)
    x[c(T,T,T,F,F,F,F,F,F,F)] <- as.duration(2:4)
    checkEquals(x, as.duration(c(2:4, 4:10)))
}
test_subassign_numeric <- function() {
    x <- as.duration(1:10)
    x[1:3] <- as.duration(2:4)
    x[4:10] <- as.duration(5:11)
    checkEquals(x, as.duration(2:11))
}
test_subsassign_character <- function() {
    dd <- c(a=as.duration(1), b=as.duration(2), c=as.duration(3), d=as.duration(4))
    dd[c("b", "c")] <- as.duration(20:21)
    expected <- c(a=as.duration(1), b=as.duration(20), c=as.duration(21), d=as.duration(4))
    checkEquals(dd, expected)
}

## test names
test_get_names <- function() {
    dd <- c(a=as.duration(1), b=as.duration(2), c=as.duration(3), d=as.duration(4))
    checkEquals(names(dd), c("a","b","c","d"))
}
test_set_names <- function() {
    names <- c("a","b","c","d")
    dd <- as.duration(1:4)
    names(dd) <- names
    checkEquals(names(dd), names)
    names(dd)[1] <- "x"
    checkEquals(names(dd), c("x","b","c","d"))
}

## test scalar/vector mix LLL

## ops
## -
test_duration_minus_duration <- function() {
    checkEquals(as.duration("1:00:00") - as.duration("00:01:00"), as.duration("00:59:00"))
    checkEquals(as.duration("-1:00:00") - as.duration("00:01:00"), as.duration("-01:01:00"))
    checkEquals(as.duration("1:00:00.1") - as.duration("1:00:00"), as.duration("0.1"))
}
test_duration_minus_numeric <- function() {
    checkEquals(as.duration("1:00:00") - min, as.duration("00:59:00"))
}
test_duration_minus_integer64 <- function() {
    checkEquals(as.duration("1:00:00") - as.integer64(min), as.duration("00:59:00"))
}
test_numeric_minus_duration <- function() {
    checkException(hour - as.duration("1:00:00"), "invalid operand types")
}
## +
test_duration_plus_duration <- function() {
    checkEquals(as.duration("1:00:00") + as.duration("00:01:00"), as.duration("01:01:00"))
    checkEquals(as.duration("-1:00:00") + as.duration("00:01:00"), as.duration("-00:59:00"))
    checkEquals(as.duration("-1:00:00.1") + as.duration("1:00:00"), as.duration("-0.1"))
}
test_integer64_plus_duration <- function() {
    checkEquals(as.integer64(hour) + as.duration("1:00:00"), as.duration("2:00:00"))
}
test_duration_plus_integer64 <- function() {
    checkEquals(as.duration("1:00:00") + as.integer64(hour), as.duration("2:00:00"))
}
test_numeric_plus_duration <- function() {
    checkEquals(hour + as.duration("1:00:00"), as.duration("2:00:00"))
}
test_character_plus_duration <- function() {
    checkException("hello" + as.duration("1:00:00"), "invalid operand types")
}
## *
test_duration_times_numeric <- function() {
    checkEquals(as.duration("00:01:00") * 3, as.duration("00:03:00"))
}
test_duration_times_integer64 <- function() {
    checkEquals(as.duration("00:01:00") * as.integer64(3), as.duration("00:03:00"))
}
test_numeric_times_duration <- function() {
    checkEquals(1.5 * as.duration("00:01:00"), as.duration("00:01:30"))
}
test_integer64_times_duration <- function() {
    checkEquals(as.integer64(3) * as.duration("00:01:00"), as.duration("00:03:00"))
}
test_character_times_duration <- function() {
    checkException("hello" / as.duration("00:01:00"), "invalid operand types")
}
test_duration_times_character <- function() {
    checkException(as.duration("00:01:00") *  "hello", "invalid operand types")
}
## /
test_duration_div_numeric <- function() {
    checkEquals(as.duration("00:03:00") / 3, as.duration("00:01:00"))
}
test_duration_div_integer64 <- function() {
    checkEquals(as.duration("00:03:00") / as.integer64(3), as.duration("00:01:00"))
}
test_duration_div_integer <- function() {
    checkEquals(as.duration("00:03:00") / as.integer(3), as.duration("00:01:00"))
}
test_numeric_div_duration <- function() {
    checkException(1.5 / as.duration("00:01:00"), "invalid operand types")
}

## unary -

## conversion functions

