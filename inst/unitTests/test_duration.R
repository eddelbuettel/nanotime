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

