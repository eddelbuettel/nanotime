## ------------ `-`
"test_nanotime-nanotime" <- function() {
  checkEquals(nanotime(2)  - nanotime(1),  as.integer64(1))
  checkEquals(nanotime(-1) - nanotime(-2), as.integer64(1))
}
"test_nanotime-integer64" <- function() {
  checkEquals(nanotime(2)  - as.integer64(1),  nanotime(1))
  checkEquals(nanotime(-1) - as.integer64(-2), nanotime(1))
}
"test_nanotime-integer" <- function() {
  checkEquals(nanotime(2)  - 1L,  nanotime(1))
  checkEquals(nanotime(-1) - -2L, nanotime(1))
}
"test_nanotime-character" <- function() {
  checkException(nanotime(0) - "A", "invalid operand types")
}
"test_character-nanotime" <- function() {
  checkException("A" - nanotime(0), "invalid operand types")
}
"test_numeric-nanotime" <- function() {
  checkException(1 - nanotime(0), "invalid operand types")
}


## ----------- `+`
"test_nanotime+numeric" <- function() {
  checkEquals(nanotime(0) +  1, nanotime(1))
  checkEquals(nanotime(0) + -1, nanotime(-1))
}

"test_nanotime+integer" <- function() {
  checkEquals(nanotime(0) +  1L, nanotime(1))
  checkEquals(nanotime(0) + -1L, nanotime(-1))
}
"test_nanotime+integer64" <- function() {
  checkEquals(nanotime(0) + as.integer64(1), nanotime(1))
  checkEquals(nanotime(0) + as.integer64(-1), nanotime(-1))
}
"test_nanotime+character" <- function() {
  checkException(nanotime(0) + "A", "invalid operand types")
}
"test_character+nanotime" <- function() {
  checkException("A" + nanotime(0), "invalid operand types")
}
"test_nanotime+nanotime" <- function() {
  checkException(nanotime(1) + nanotime(0), "invalid operand types")
}
"test_nanotime+integer64" <- function() {
  checkEquals(nanotime(0) + as.integer64(1), nanotime(1))
  checkEquals(nanotime(0) + as.integer64(-1), nanotime(-1))
}
"test_numeric+nanotime" <- function() {
  checkEquals(nanotime(0) +  1, nanotime( 1))
  checkEquals(nanotime(0) + -1, nanotime(-1))
}
"test_integer+nanotime" <- function() {
  checkEquals( 1L + nanotime(0), nanotime(1))
  checkEquals(-1L + nanotime(0), nanotime(-1))
}


## ---------- other ops
test_compare_nanotime_ANY <- function() {
  checkTrue(nanotime(1) == nanotime(1))
  checkTrue(nanotime(1) == as.integer64(1))
  checkTrue(nanotime(1) == 1L)
  checkTrue(nanotime(1) == 1)
  checkTrue(is.na(nanotime(1) == "a"))  # same as 'integer64'... TODO: check
  
  checkTrue(!(nanotime(1) < nanotime(1)))
  checkTrue(!(nanotime(1) < as.integer64(1)))
  checkTrue(!(nanotime(1) < 1L))
  checkTrue(!(nanotime(1) < 1))
}
test_compare_ANY_nanotime <- function() {
  checkTrue(as.integer64(1) == nanotime(1))
  checkTrue(1L == nanotime(1))
  checkTrue(1 == nanotime(1))
  checkTrue(!("a" == is.na(nanotime(1))))

  checkTrue(!(as.integer64(1) < nanotime(1)))
  checkTrue(!(1L < nanotime(1)))
  checkTrue(!(1 < nanotime(1)))
}
test_Logic <- function() {
  exc <- "operations are possible only for numeric, logical or complex types"
  checkException(nanotime(1) & nanotime(1), exc)
  checkException(nanotime(1) & as.integer64(1), exc)
  checkException(nanotime(1) & TRUE, exc)

  checkException(as.integer64(1) & nanotime(1), exc)
  checkException(1L & nanotime(1), exc)
  checkException(FALSE & nanotime(1), exc)
}
test_Math <- function() {
  checkException(abs(nanotime(1)), "non-numeric argument to mathematical function")
}
test_Math2 <- function() {
  checkException(round(nanotime(1), 2), "non-numeric argument to mathematical function")
}
test_Summary <- function() {
  checkException(sum(nanotime(1)), "invalid 'type' (nanotime) of argument")

  checkEquals(min(nanotime(c(1,8,4,2,0,3,10))), nanotime(0))
  checkEquals(max(nanotime(c(1,8,4,2,0,3,10))), nanotime(10))
  checkEquals(range(nanotime(c(1,8,4,2,0,3,10))), nanotime(c(0, 10)))
}
test_Complex <- function() {
  checkException(Arg(nanotime(1)), "non-numeric argument to function")
}
