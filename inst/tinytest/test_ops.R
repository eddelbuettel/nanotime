
suppressMessages({
    library(nanotime)
    library(bit64)
})

## ------------ `-`
#"test_nanotime-nanotime" <- function() {
expect_identical(nanotime(2)  - nanotime(1),  as.nanoduration(1))
expect_identical(nanotime(-1) - nanotime(-2), as.nanoduration(1))

#"test_nanotime-integer64" <- function() {
expect_identical(nanotime(2)  - as.integer64(1),  nanotime(1))
expect_identical(nanotime(-1) - as.integer64(-2), nanotime(1))

#"test_nanotime-integer" <- function() {
expect_identical(nanotime(2)  - 1L,  nanotime(1))
expect_identical(nanotime(-1) - -2L, nanotime(1))

#"test_nanotime-character" <- function() {
expect_error(nanotime(0) - "A", "invalid operand types")

#"test_character-nanotime" <- function() {
expect_error("A" - nanotime(0), "invalid operand types")

#"test_numeric-nanotime" <- function() {
expect_error(1 - nanotime(0), "invalid operand types")



## ----------- `+`
#"test_nanotime+numeric" <- function() {
expect_identical(nanotime(0) +  1, nanotime(1))
expect_identical(nanotime(0) + -1, nanotime(-1))


#"test_nanotime+integer" <- function() {
expect_identical(nanotime(0) +  1L, nanotime(1))
expect_identical(nanotime(0) + -1L, nanotime(-1))

#"test_nanotime+integer64" <- function() {
expect_identical(nanotime(0) + as.integer64(1), nanotime(1))
expect_identical(nanotime(0) + as.integer64(-1), nanotime(-1))

#"test_nanotime+character" <- function() {
expect_error(nanotime(0) + "A", "invalid operand types")

#"test_character+nanotime" <- function() {
expect_error("A" + nanotime(0), "invalid operand types")

#"test_nanotime+nanotime" <- function() {
expect_error(nanotime(1) + nanotime(0), "invalid operand types")

#"test_nanotime+integer64" <- function() {
expect_identical(nanotime(0) + as.integer64(1), nanotime(1))
expect_identical(nanotime(0) + as.integer64(-1), nanotime(-1))

#"test_numeric+nanotime" <- function() {
expect_identical(nanotime(0) +  1, nanotime( 1))
expect_identical(nanotime(0) + -1, nanotime(-1))

#"test_integer+nanotime" <- function() {
expect_identical( 1L + nanotime(0), nanotime(1))
expect_identical(-1L + nanotime(0), nanotime(-1))



## ---------- other ops
#test_compare_nanotime_ANY <- function() {
expect_true(nanotime(1) == nanotime(1))
expect_true(nanotime(1) == as.integer64(1))
expect_true(nanotime(1) == 1L)
expect_true(nanotime(1) == 1)
expect_error(is.na(nanotime(1) == "a"))  # now passes on and gets parse error

expect_true(!(nanotime(1) < nanotime(1)))
expect_true(!(nanotime(1) < as.integer64(1)))
expect_true(!(nanotime(1) < 1L))
expect_true(!(nanotime(1) < 1))

#test_compare_ANY_nanotime <- function() {
expect_true(as.integer64(1) == nanotime(1))
expect_true(1L == nanotime(1))
expect_true(1 == nanotime(1))
expect_true(!("a" == is.na(nanotime(1))))

expect_true(!(as.integer64(1) < nanotime(1)))
expect_true(!(1L < nanotime(1)))
expect_true(!(1 < nanotime(1)))

#test_compare_character_nanotime <- function() {
expect_true(isFALSE("2018-12-28T16:34:59.649943448+00:00" < nanotime("2018-12-28T16:34:59.000000000+00:00")))
expect_true("2018-12-28T16:34:59.649943448+00:00" >  nanotime("2018-12-28T16:34:59.000000000+00:00"))
expect_true("2018-12-28T16:34:59.649943448+00:00" == nanotime("2018-12-28T16:34:59.649943448+00:00"))

#test_compare_nanotime_character <- function() {
expect_true(isFALSE(nanotime("2018-12-28T16:34:59.649943448+00:00") < "2018-12-28T16:34:59.000000000+00:00"))
expect_true(nanotime("2018-12-28T16:34:59.649943448+00:00") >  "2018-12-28T16:34:59.000000000+00:00")
expect_true(nanotime("2018-12-28T16:34:59.649943448+00:00") == "2018-12-28T16:34:59.649943448+00:00")

#test_Logic <- function() {
exc <- "operations are possible only for numeric, logical or complex types"
#expect_error(nanotime(1) & nanotime(1), exc)
expect_error(nanotime(1) & as.integer64(1), exc)
expect_error(nanotime(1) & TRUE, exc)

expect_error(as.integer64(1) & nanotime(1), exc)
expect_error(1L & nanotime(1), exc)
expect_error(FALSE & nanotime(1), exc)

#test_Math <- function() {
expect_error(abs(nanotime(1)), "non-numeric argument to mathematical function")

#test_Math2 <- function() {
expect_error(round(nanotime(1), 2), "non-numeric argument to mathematical function")

#test_Summary <- function() {
expect_error(sum(nanotime(1)))#, "invalid 'type' (nanotime) of argument")

expect_identical(min(nanotime(c(1,8,4,2,0,3,10))), nanotime(0))
expect_identical(max(nanotime(c(1,8,4,2,0,3,10))), nanotime(10))
expect_identical(range(nanotime(c(1,8,4,2,0,3,10))), nanotime(c(0, 10)))

#test_Complex <- function() {
expect_error(Arg(nanotime(1)), "non-numeric argument to function")
