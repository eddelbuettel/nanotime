library(nanotime)
suppressMessages(library(bit64))

isSolaris <- Sys.info()[["sysname"]] == "SunOS"

savedFormat <- NULL
one_second  <- 1e9
aa <- "+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"
bb <- "+2014-01-01 00:00:00 -> 2015-01-01 00:00:00+"
cc <- "-2015-01-01 00:00:00 -> 2016-01-01 00:00:00-"
dd <- "-2016-01-01 00:00:00 -> 2017-01-01 00:00:00+"


savedFormat <- options()$nanotimeFormat
options(nanotimeFormat="%Y-%m-%d %H:%M:%S")


## nanotime constructors/accessors
##test_as.nanoival <- function() {
ni <- as.nanoival(aa)
expect_identical(nanoival.start(ni),     nanotime("2013-01-01 00:00:00")) &
    expect_identical(nanoival.end(ni),   nanotime("2014-01-01 00:00:00")) &
    expect_identical(nanoival.sopen(ni), FALSE) &
    expect_identical(nanoival.eopen(ni), TRUE)

##test_as.nanoival_vector <- function() {
ni <- as.nanoival(c(a=aa, b=bb, c=cc, d=dd))
expect_identical(nanoival.start(ni),  c(a=nanotime("2013-01-01 00:00:00"),
                             b=nanotime("2014-01-01 00:00:00"),
                             c=nanotime("2015-01-01 00:00:00"),
                             d=nanotime("2016-01-01 00:00:00"))) &
    expect_identical(nanoival.end(ni),  c(a=nanotime("2014-01-01 00:00:00"),
                               b=nanotime("2015-01-01 00:00:00"),
                               c=nanotime("2016-01-01 00:00:00"),
                               d=nanotime("2017-01-01 00:00:00"))) &
    expect_identical(nanoival.sopen(ni), c(a=FALSE, b=FALSE, c=TRUE, d=TRUE)) &
    expect_identical(nanoival.eopen(ni), c(a=TRUE, b=FALSE, c=TRUE, d=FALSE))

if (!isSolaris) {
    expect_identical(length(as.nanoival(vector("character", 0))), 0L)
    expect_identical(as.nanoival("-2013-01-01 00:00:00 America/New_York -> 2014-01-01 00:00:00 America/New_York+"),
                     nanoival(nanotime("2013-01-01 00:00:00 America/New_York"),
                              nanotime("2014-01-01 00:00:00 America/New_York"), TRUE, FALSE))

    ## test warning when we double specify the timezone:
    expect_error(as.nanoival("-2013-01-01 00:00:00 America/New_York -> 2014-01-01 00:00:00+00:00+", tz="Europe/London"),
                 "timezone is specified twice: in the string and as an argument")

    ##test_as.nanoival_vector_fail <- function() {
    expect_error(as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00"), "`nanoival` must end with '\\+' or '-'")
    expect_error(as.nanoival("2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"), "`nanoival` must start with '\\+' or '-'") 
    expect_error(as.nanoival("+2013-01-01 00:00:00 $$ 2014-01-01 00:00:00-"), "Parse error on 2013-01-01 00:00:00 \\$\\$ 2014-01-01 00:00:00")
    expect_error(as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00- "), "`nanoival` must end with '\\+' or '-'")
    expect_error(as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00a"), "`nanoival` must end with '\\+' or '-'")
    expect_error(as.nanoival("+2013-01-01 00:00:00 America/New_York -> 2014-01-01 00:00:00 America/New_York %%"), "`nanoival` must end with '\\+' or '-'")
    expect_error(as.nanoival("+2013-01-01 00:00:00 America/New_York -> 2014-01-01 00:00:00 America/New_York + "), "`nanoival` must end with '\\+' or '-'")
    expect_error(as.nanoival("-2013-01-01 00:00:00 America/New_York -> 2014-01-01 00:00:00 America/New_YYork+"),
                 "Cannot retrieve timezone")
}

expect_error(as.nanoival(aa, tz=list(1)), "argument 'tz' must be of type 'character'")
              
##test_nanoival <- function() {
expect_identical(nanoival(nanotime("2013-01-01 00:00:00"),
                          nanotime("2014-01-01 00:00:00"), TRUE, TRUE),
                 as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"))
expect_error(nanoival(nanotime(2), nanotime(1)), "interval end \\(1\\) smaller than interval start \\(2\\)")
expect_identical(nanoival(), as.nanoival(NULL))
expect_identical(length(nanoival()), 0L)
expect_identical(nanoival(), as.nanoival())
expect_identical(length(as.nanoival()), 0L)

##test_nanoival_vector<- function() {
starts <- c(nanotime("2013-01-01 00:00:00"),
            nanotime("2014-01-01 00:00:00"),
            nanotime("2015-01-01 00:00:00"),
            nanotime("2016-01-01 00:00:00"))
ends   <- c(nanotime("2014-01-01 00:00:00"),
            nanotime("2015-01-01 00:00:00"),
            nanotime("2016-01-01 00:00:00"),
            nanotime("2017-01-01 00:00:00"))
sopens <- c(FALSE, FALSE, TRUE, TRUE)
eopens <- c(TRUE,  FALSE, TRUE, FALSE)
expect_identical(nanoival(starts, ends, sopens, eopens), as.nanoival(c(aa, bb, cc, dd)))

## different vector lengths:
starts <- nanotime(1:3)
ends   <- nanotime(4)
sopens <- c(TRUE,  TRUE,  TRUE)
eopens <- c(FALSE, FALSE, FALSE)
expect_identical(nanoival(starts, ends, sopens, eopens),
                 c(nanoival(nanotime(1), nanotime(4), TRUE, FALSE),
                   nanoival(nanotime(2), nanotime(4), TRUE, FALSE),
                   nanoival(nanotime(3), nanotime(4), TRUE, FALSE)))
expect_identical(nanoival(starts, ends, sopens[1], eopens),
                 c(nanoival(nanotime(1), nanotime(4), TRUE, FALSE),
                   nanoival(nanotime(2), nanotime(4), TRUE, FALSE),
                   nanoival(nanotime(3), nanotime(4), TRUE, FALSE)))
expect_identical(nanoival(starts[1], ends, sopens, eopens[1]),
                 c(nanoival(nanotime(1), nanotime(4), TRUE, FALSE),
                   nanoival(nanotime(1), nanotime(4), TRUE, FALSE),
                   nanoival(nanotime(1), nanotime(4), TRUE, FALSE)))


## show/print/format
##test_show <- function() {
ival_str <- "+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"
ival <- as.nanoival(ival_str)
expect_identical(format(ival), ival_str)
expect_stdout(show(ival))

ival_str <- "-2013-01-01 00:00:00 -> 2014-01-01 00:00:00+"
ival <- as.nanoival(ival_str)
expect_identical(format(ival), ival_str)
expect_stdout(show(ival))

ival_str <- c(a="+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
ival <- as.nanoival(ival_str)
expect_identical(format(ival), ival_str)
expect_stdout(show(ival))

expect_identical(format(as.nanoival(vector("character", 0))), "nanoival(0)")


##test_format <- function() {
ival_str <- "+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"
ival <- as.nanoival(ival_str)
expect_identical(format(ival), ival_str)
expect_identical(as.character(ival), ival_str)

ival_str <- "-2013-01-01 00:00:00 -> 2014-01-01 00:00:00+"
ival <- as.nanoival(ival_str)
expect_identical(format(ival), ival_str)
expect_identical(as.character(ival), ival_str)


## as.data.frame
##test_as.data.frame <- function() {
ni <- as.nanoival(c(aa, bb, cc, dd))
expect_identical(as.data.frame(ni), data.frame(ni=ni))

rownames <- c("aa","bb","cc","dd")
names(ni) <- rownames
as.data.frame(ni)
expect_identical(as.data.frame(ni), data.frame(ni=ni, row.names=rownames))


## equality and comparison operators

## eq/ne
##test_eq <- function() {
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_true(x==x)
y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_true(!(x==y))
z <- as.nanoival("+2013-01-01 00:00:01 -> 2014-01-01 00:00:00-")
expect_true(!(x==z))

                                        ##test_ne <- function() {
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_true(!(x!=x))
y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_true(x!=y)
z <- as.nanoival("+2013-01-01 00:00:01 -> 2014-01-01 00:00:00-")
expect_true(x!=z)

    
## lt
##test_lt_non_overlapping <- function() {
## x: c----------o
## y:              c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival(c(b="+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-"))
expect_identical(x < y, c(b=TRUE))
expect_identical(y < x, c(b=FALSE))

##test_lt_overlapping <- function() {
## x: c----------o
## y:       c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(x < y, TRUE)
expect_identical(y < x, FALSE)

##test_lt_same_end <- function() {
## x: c----------o
## y:       c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x < y, TRUE)
expect_identical(y < x, FALSE)

##test_lt_included <- function() {
## x: c----------o
## y:    c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
expect_identical(x < y, TRUE)
expect_identical(y < x, FALSE)

##test_lt_same_start<- function() {
## x: c----------o
## y: c--------------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(x < y, TRUE)
expect_identical(y < x, FALSE)

##test_lt_same_open_start <- function() {
## x: c----------o
## y: o----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x < y, TRUE)
expect_identical(y < x, FALSE)

##test_lt_same_open_end <- function() {
## x: c----------o
## y: c----------c
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
expect_identical(x < y, TRUE)
expect_identical(y < x, FALSE)

##test_lt_same <- function() {
## x: c----------o
## y: c----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x < y, FALSE)
expect_identical(y < x, FALSE)

##test_lt_same <- function() {
## x: c----------o
## y: c----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x < y, FALSE)
expect_identical(y < x, FALSE)

##test_lt_multiple <- function() {
## x: c----------o
## y:       c----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2014-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(c(x, y) < c(y, x), c(TRUE, FALSE))

## le
##test_le_non_overlapping <- function() {
## x: c----------o
## y:              c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, FALSE)

##test_le_overlapping <- function() {
## x: c----------o
## y:       c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, FALSE)

##test_le_same_end <- function() {
## x: c----------o
## y:       c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, FALSE)

##test_le_included <- function() {
## x: c----------o
## y:    c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, FALSE)

##test_le_same_start<- function() {
## x: c----------o
## y: c--------------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, FALSE)

##test_le_same_open_start <- function() {
## x: c----------o
## y: o----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, FALSE)

##test_le_same_open_end <- function() {
## x: c----------o
## y: c----------c
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, FALSE)

##test_le_same <- function() {
## x: c----------o
## y: c----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x <= y, TRUE)
expect_identical(y <= x, TRUE)


## gt
##test_gt_non_overlapping <- function() {
## x: c----------o
## y:              c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(x > y, FALSE)
expect_identical(y > x, TRUE)

##test_gt_overlapping <- function() {
## x: c----------o
## y:       c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(x > y, FALSE)
expect_identical(y > x, TRUE)

##test_gt_same_end <- function() {
## x: c----------o
## y:       c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x > y, FALSE)
expect_identical(y > x, TRUE)

##test_gt_included <- function() {
## x: c----------o
## y:    c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
expect_identical(x > y, FALSE)
expect_identical(y > x, TRUE)

##test_gt_same_start<- function() {
## x: c----------o
## y: c--------------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(x > y, FALSE)
expect_identical(y > x, TRUE)

##test_gt_same_open_start <- function() {
## x: c----------o
## y: o----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x > y, FALSE)
expect_identical(y > x, TRUE)

##test_gt_same_open_end <- function() {
## x: c----------o
## y: c----------c
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
expect_identical(x > y, FALSE)
expect_identical(y > x, TRUE)

##test_gt_same <- function() {
## x: c----------c
## y: c----------c
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
expect_identical(x > y, FALSE)
expect_identical(y > x, FALSE)


## ge
##test_ge_non_overlapping <- function() {
## x: c----------o
## y:              c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(x >= y, FALSE)
expect_identical(y >= x, TRUE)

##test_ge_overlapping <- function() {
## x: c----------o
## y:       c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(x >= y, FALSE)
expect_identical(y >= x, TRUE)

##test_ge_same_end <- function() {
## x: c----------o
## y:       c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x >= y, FALSE)
expect_identical(y >= x, TRUE)

##test_ge_included <- function() {
## x: c----------o
## y:    c----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
expect_identical(x >= y, FALSE)
expect_identical(y >= x, TRUE)

##test_ge_same_start<- function() {
## x: c----------o
## y: c--------------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(x >= y, FALSE)
expect_identical(y >= x, TRUE)

##test_ge_same_open_start <- function() {
## x: c----------o
## y: o----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x >= y, FALSE)
expect_identical(y >= x, TRUE)

##test_ge_same_open_end <- function() {
## x: c----------o
## y: c----------c
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
expect_identical(x >= y, FALSE)
expect_identical(y >= x, TRUE)

##test_ge_same <- function() {
## x: o----------o
## y: o----------o
x <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
expect_identical(x >= y, TRUE)
expect_identical(y >= x, TRUE)



## sorting/ordering
##test_is_unsorted_non_overlapping <- function() {
## x: c----------o
## y:              c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(is.unsorted(c(x, y)), FALSE)
expect_identical(is.unsorted(c(y, x)), TRUE)

##test_is_unsorted_overlapping <- function() {
## x: c----------o
## y:       c--------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
y <- as.nanoival("+2014-01-01 00:00:00 -> 2016-01-01 00:00:00-")
expect_identical(is.unsorted(c(x, y)), FALSE)
expect_identical(is.unsorted(c(y, x)), TRUE)

##test_is_unsorted_same_end <- function() {
## x: c----------o
## y:      c-----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
y <- as.nanoival("+2014-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(is.unsorted(c(x, y)), FALSE)
expect_identical(is.unsorted(c(y, x)), TRUE)

##test_is_unsorted_included <- function() {
## x: c----------o
## y:   c-----o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2016-01-01 00:00:00-")
y <- as.nanoival("+2014-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(is.unsorted(c(x, y)), FALSE)
expect_identical(is.unsorted(c(y, x)), TRUE)

##test_is_unsorted_same_start <- function() {
## x: c----o
## y: c----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(is.unsorted(c(x, y)), FALSE)
expect_identical(is.unsorted(c(y, x)), TRUE)

##test_is_unsorted_not_strictly <- function() {
## x: c----o
## y: c----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(is.unsorted(c(x, x, y, y)), FALSE)
expect_identical(is.unsorted(c(y, y, x, x)), TRUE)

##test_is_unsorted_strictly <- function() {
## x: c----o
## y: c----------o
x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
expect_identical(is.unsorted(c(x, x, y, y), strictly=TRUE), TRUE)
expect_identical(is.unsorted(c(y, y, x, x), strictly=TRUE), TRUE)
expect_error(is.unsorted(c(y, y, x, x), strictly="a"), "argument 'strictly' must be a logical")
expect_error(is.unsorted(c(y, y, x, x), strictly=as.logical(NULL)), "argument 'strictly' cannot have length 0")

## test 'na.rm':
expect_identical(is.unsorted(c(x, y, NA_nanoival_)), NA_nanoival_)
expect_identical(is.unsorted(c(x, y, NA_nanoival_), na.rm=TRUE), FALSE)

##test_sort <- function() {
v <- as.nanoival(c(aa, bb, cc, dd))
v_descending <- as.nanoival(c(dd, cc, bb, aa))
expect_identical(sort(v_descending), v)
expect_identical(sort(v, decreasing=TRUE), v_descending)
expect_error(sort(v, decreasing=as.logical(NULL)), "argument 'decreasing' cannot have length 0")
expect_error(sort(v, decreasing="not a logical"), "argument 'decreasing' must be logical")


## c
##test_c <- function() {                  # LLL
a <- c(nanotime(1), nanotime(2))
expect_identical(a, nanotime(1:2))

a <- c(nanotime(1:2), nanotime(3:4))
expect_identical(a, nanotime(1:4))

##test_c_name <- function() {
c_xy <- c(x=as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"),
          y=as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-"))
expect_identical(names(c_xy), c("x","y"))

##test_c_name_assign <- function() {
c_xy <- c(x=as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"),
          y=as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-"))
names(c_xy) <- c("a","b")
expect_identical(names(c_xy), c("a","b"))

##test_c_name_assign_null <- function() {
c_xy <- c(x=as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"),
          y=as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-"))
names(c_xy) <- NULL
expect_true(is.null(names(c_xy)))



## set operations

## naming convention for interval tests:
## cc : start closed, end closed
## co : start closed, end open
## oc : etc.
## oo

## intersection
## --------------------------------------------------------------------------

## time - interval

## test_intersect_time_interval_null_interval <- function() {
i1 <- as.nanoival(NULL)
s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
expect_identical(s1[i1], as.nanotime())
expect_identical(s1 %in% i1, rep(FALSE, 10))

##test_intersect.idx_unsorted <- function() {
a <- c(nanotime("2013-12-12 12:12:12"), nanotime("2012-12-12 12:12:12"))
idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
expect_error(intersect.idx(a, idx), "x must be sorted")
expect_error(a %in% idx, "x must be sorted")

##test_subset_unsorted <- function() {
a <- c(nanotime("2013-12-12 12:12:12"), nanotime("2012-12-12 12:12:12"))
idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
expect_error(a[idx], "x must be sorted")
expect_error(a %in% idx, "x must be sorted")

## test that nanotime %in% nanotime still works as it goes through the same S3
a <- nanotime(3:6)
idx <- nanotime(1:10)
expect_identical(a %in% idx, 3:6 %in% 1:10)


## time - interval
## 1: ..............
## 2:   c----c
## r:   ......
##test_intersect_idx_time_interval_cc <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
r <- list(x=c(3,4,5,6,7,8), y=c(1,1,1,1,1,1))
expect_identical(intersect.idx(a, idx), r)
expect_identical(a %in% idx, 1:10 %in% c(3,4,5,6,7,8))

## 1: ..............
## 2:   c----c
## r:   ......
##test_intersect_time_interval_cc  <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
r <- seq(nanotime("2012-12-12 12:12:14"), nanotime("2012-12-12 12:12:19"), by=one_second)
expect_identical(a[idx], r)
expect_identical(intersect(a, idx), r)

## 1: ..............
## 2:   o----o
## r:    ....
##test_intersect_time_interval_oo <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- as.nanoival("-2012-12-12 12:12:14 -> 2012-12-12 12:12:19-")
r <- seq(nanotime("2012-12-12 12:12:15"), nanotime("2012-12-12 12:12:18"), by=one_second)
expect_identical(a[idx], r)

## 1:   ......
## 2: o---------o
## r:   ......
##test_intersect_time_interval_overlapping <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:30-")
expect_identical(a[idx], a)

## 1:   .................
## 2: o-----o   c-----c
## r:  .....    .......
##test_intersect_time_interval_multiple <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- c(as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:14-"),
         as.nanoival("+2012-12-12 12:12:18 -> 2012-12-12 12:12:20+"))
r <- c(seq(nanotime("2012-12-12 12:12:12"), nanotime("2012-12-12 12:12:13"), by=one_second),
       seq(nanotime("2012-12-12 12:12:18"), nanotime("2012-12-12 12:12:20"), by=one_second))
expect_identical(a[idx], r)

##test_intersect_time_interval_multiple_direct_call  <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- c(as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:14-"),
         as.nanoival("+2012-12-12 12:12:18 -> 2012-12-12 12:12:20+"))
r <- c(seq(nanotime("2012-12-12 12:12:12"), nanotime("2012-12-12 12:12:13"), by=one_second),
       seq(nanotime("2012-12-12 12:12:18"), nanotime("2012-12-12 12:12:20"), by=one_second))
expect_identical(intersect(a, idx), r)


## interval - interval:
## 1: c-----------c
## 2: c-----------c
## r: c-----------c
##test_intersect_interval_interval_cc_cc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- i2
expect_true(intersect(i1, i2) == r)

## 1: c-----------c
## 2: o-----------c
## r: o-----------c
##test_intersect_interval_interval_cc_oc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- i2
expect_true(intersect(i1, i2) == r &
          intersect(i2, i1) == r)

## 1: c-----------c
## 2: c-----------o
## r: c-----------o
##test_intersect_interval_interval_cc_co__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- i2
expect_true(intersect(i1, i2) == r &
          intersect(i2, i1) == r)

## 1: c-----------o
## 2: o-----------c
## r: o-----------o
##test_intersect_interval_interval_co_oc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
expect_true(intersect(i1, i2) == r &
          intersect(i2, i1) == r)

## 1: o-----------o
## 2: o-----------o
## r: o-----------o
##test_intersect_interval_interval_oo_oo__2_eq_1 <- function() {
i1 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- i2
expect_true(intersect(i1, i2) == r)

## 1: c-----------c
## 2: o-----------o
## r: o-----------o
##test_intersect_interval_interval_cc_oo__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- i2
expect_true(intersect(i1, i2) == r)

## 1: c-----------o
## 2:             o-----------c
## r: 
##test_intersect_interval_interval_co_oc__2_lg_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
r  <- new("nanoival", as.complex(NULL))
expect_identical(intersect(i2, i1), r)

## 1: c-----------o
## 2:             c-----------c
## r: 
##test_intersect_interval_interval_co_cc__2_lg_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
r  <- as.nanoival(NULL)
expect_identical(intersect(i2, i1), r)

## 1: c-----------c
## 2:             o-----------c
## r: 
##test_intersect_interval_interval_cc_oc__2_lg_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
r  <- as.nanoival(NULL)
expect_identical(intersect(i2, i1), r)

## 1: c-----------c
## 2:             c-----------c
## r:             cc
##test_intersect_interval_interval_cc_cc__2_lg_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
r  <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:05+")
expect_identical(intersect(i2, i1), r)
expect_identical(intersect(i1, i2), r)

## 1: c-----------c                c-----------c 
## 2:               c-----------c
## r:
##test_intersect_interval_interval_distinct_more <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:10 -> 2015-01-01 12:00:12+"))
i2 <-   as.nanoival("+2015-01-01 12:00:06 -> 2015-01-01 12:00:07+")
r  <- as.nanoival(NULL)
expect_identical(intersect(i1, i2), r)
expect_identical(intersect(i2, i1), r)


## time-time
expect_identical(intersect(nanotime(1:10), nanotime(5:15)), nanotime(5:10))

## union
## --------------------------------------------------------------------------
## interval - interval:
## 1: c-----------c
## 2: c-----------c
## r: c-----------c
##test_union_interval_interval_cc_cc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- i2
expect_true(union(i1, i2) == r &
          union(i2, i1) == r)

## 1: c-----------c
## 2: o-----------c
## r: c-----------c
##test_union_interval_interval_cc_oc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- i1
expect_true(union(i1, i2) == r &
          union(i2, i1) == r)

## 1: c-----------c
## 2: c-----------o
## r: c-----------c
##test_union_interval_interval_cc_co__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- i1
expect_true(union(i1, i2) == r &
          union(i2, i1) == r)

## 1: c-----------o
## 2: o-----------c
## r: c-----------c
##test_union_interval_interval_co_oc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
expect_true(union(i1, i2) == r &
          union(i2, i1) == r)

## 1: o-----------o
## 2: o-----------o
## r: o-----------o
##test_union_interval_interval_oo_oo__2_eq_1 <- function() {
    i1 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- i2
    expect_true(union(i1, i2) == r &
              union(i2, i1) == r)

## 1: c-----------c
## 2: o-----------o
## r: c-----------c
##test_union_interval_interval_cc_oo__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- i1
expect_true(union(i1, i2) == r &
          union(i2, i1) == r)

## 1: c-----------c
## 2:             o-----------o
## 1: c-----------o
## 2:             c-----------o
## r: c-----------------------o
##test_union_interval_interval_no_overlap_co_oc <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:07-")
expect_identical(union(i1, i2), r)
expect_identical(union(i2, i1), r)

## 1: c-----------o
## 2:             o-----------o
## r: c-----------o-----------o
##test_union_interval_interval_no_overlap_oo <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
r  <- c(i1, i2)
expect_identical(union(i1, i2), r)
expect_identical(union(i2, i1), r)

## 1: c-----------c
## 2:             c-----------o
## r: c-----------------------o
##test_union_interval_interval_no_overlap_cc  <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:07-")
expect_identical(union(i1, i2), r)
expect_identical(union(i2, i1), r)

## 1: c-----------c              c----------o
## 2:             c-----------o      o----------c
## r: c-----------------------o
##test_union_interval_interval_no_overlap_cc_more  <- function() {
i1 <- as.nanoival(c("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+",
                    "+2016-01-01 12:00:03 -> 2016-01-01 12:00:05-"))
i2 <- as.nanoival(c("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07-",
                    "-2016-01-01 12:00:04 -> 2016-01-01 12:00:08+"))
r  <- as.nanoival(c("+2015-01-01 12:00:03 -> 2015-01-01 12:00:07-",
                    "+2016-01-01 12:00:03 -> 2016-01-01 12:00:08+"))
expect_identical(union(i1, i2), r)
expect_identical(union(i2, i1), r)

## time-time
expect_identical(union(nanotime(1:2), nanotime(8:10)), c(nanotime(1:2), nanotime(8:10)))


## setdiff
## --------------------------------------------------------------------------

## time - interval
## 1: ..........
## 2:   c----c
## r: ..      ..
##test_sediff_idx_time_interval_cc <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
r <- c(1, 2, 9, 10)
expect_identical(setdiff.idx(a, idx), r)

## time - interval
## 1: ..........
## 2:   o----o
## r: ...    ...
##test_sediff_idx_time_interval_oo <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
idx <- as.nanoival("-2012-12-12 12:12:14 -> 2012-12-12 12:12:19-")
r <- c(1, 2, 3, 8, 9, 10)
expect_identical(setdiff.idx(a, idx), r)

##test_sediff_idx_time_interval_unsorted <- function() {
a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
a[1]  <- a[10]                      # make it unsorted
idx <- as.nanoival("-2012-12-12 12:12:14 -> 2012-12-12 12:12:19-")
expect_error(setdiff.idx(a, idx), "x must be sorted")



## time - interval:
## 1: as.nanoival("-----------")
## 2:    as.nanoival("---")
## r: as.nanoival("--")   as.nanoival("----")
##test_setdiff_time_interval_cc__2_subset_of_1 <- function() {
s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=3, by=one_second),
        seq(nanotime("2015-01-01 12:00:06"), length.out=4, by=one_second))
expect_identical(setdiff(s1, i2), r)

##test_setdiff_time_interval_oc__2_subset_of_1 <- function() {
s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=4, by=one_second),
        seq(nanotime("2015-01-01 12:00:06"), length.out=4, by=one_second))
expect_identical(setdiff(s1, i2), r)

##test_setdiff_time_interval_co__2_subset_of_1 <- function() {
s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=3, by=one_second),
        seq(nanotime("2015-01-01 12:00:05"), length.out=5, by=one_second))
expect_identical(setdiff(s1, i2), r)

##test_setdiff_time_interval_oo__2_subset_of_1 <- function() {
s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=4, by=one_second),
        seq(nanotime("2015-01-01 12:00:05"), length.out=5, by=one_second))
expect_identical(setdiff(s1, i2), r)


## interval - interval:
## 1: c-----------c
## 2: c-----------c
## r:
##test_setdiff_interval_interval_cc_cc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- new("nanoival", as.complex(NULL))
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c
## 2: o-----------c
## r: c
##test_setdiff_interval_interval_cc_oc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+")
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c
## 2: c-----------o
## r:             c
##test_setdiff_interval_interval_cc_co__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:05+")
expect_identical(setdiff(i1, i2), r)

## 1: c-----------o
## 2: o-----------c
## r: c
##test_setdiff_interval_interval_co_oc__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+")
expect_identical(setdiff(i1, i2), r)

## 1: o-----------o
## 2: o-----------o
## r:
##test_setdiff_interval_interval_oo_oo__2_eq_1 <- function() {
i1 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- new("nanoival", as.complex(NULL))
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c
## 2: o-----------o
## r: c-----------c
##test_setdiff_interval_interval_cc_oo__2_eq_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+"),
        as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:05+"))
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c
## 2:             c-----------o
## r: c-----------o            
##test_setdiff_interval_interval_cc_co__2_gt_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c
## 2:             o-----------o
## r: c-----------c            
##test_setdiff_interval_interval_cc_oo__2_gt_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c
## 2:             o-----------c
## r: c-----------c            
##test_setdiff_interval_interval_cc_oc__2_gt_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
expect_identical(setdiff(i1, i2), r)

## 1:             c-----------c
## 2: o-----------c
## r:             o-----------c
##test_setdiff_interval_interval_cc_oc__2_lt_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
expect_identical(setdiff(i1, i2), r)

## 1:             c-----------c
## 2: o-----------o
## r:             c-----------c
##test_setdiff_interval_interval_cc_oo__2_lt_1 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
expect_identical(setdiff(i1, i2), r)

## various tests where we add a third interval
## 1:             c-----------c c--------c
## 2: o-----------o
## r:             c-----------c c--------c
##test_setdiff_interval_interval_cc_oo__2_lt_1_3rd <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10+"))
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
r  <- c(as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10+"))
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c        c--------o
## 2: o--------------------c
## r:                      o--------c
##test_setdiff_interval_interval_cc_co_oc_3rd <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
i2 <-   as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:08+")
r  <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+"),
        as.nanoival("-2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c
## 2:                o-----------o
## r: c-----------c            
##test_setdiff_interval_interval_non_overlapping__1_gt_2 <- function() {
i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
i2 <- as.nanoival("-2015-01-01 12:00:06 -> 2015-01-01 12:00:07-")
r  <- i1
expect_identical(setdiff(i1, i2), r)

## 1:                o-----------o
## 2: c-----------c
## r:                o-----------o
##test_setdiff_interval_interval_non_overlapping__2_gt_1 <- function() {
i1 <- as.nanoival("-2015-01-01 12:00:06 -> 2015-01-01 12:00:07-")
i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
r  <- i1
expect_identical(setdiff(i1, i2), r)

## 1: c-----------c                c--------c
## 2:                o-----------o
## r: c-----------c            
##test_setdiff_interval_interval_non_overlapping__1_gt_2_more <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:10 -> 2015-01-01 12:00:12+"))
i2 <- as.nanoival("-2015-01-01 12:00:06 -> 2015-01-01 12:00:07-")
r  <- i1
expect_identical(setdiff(i1, i2), r)

## interval - interval:
## 1: c-----------c  c-----------c
## 2:  o---------o
## r:
##test_setdiff_interval_interval_2_inside_1__more <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:02 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:10 -> 2015-01-01 12:00:12+"))
i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:04-")
r  <- c(as.nanoival("+2015-01-01 12:00:02 -> 2015-01-01 12:00:03+"),
        as.nanoival("+2015-01-01 12:00:04 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:10 -> 2015-01-01 12:00:12+"))
expect_identical(setdiff(i1, i2), r)

## interval - interval:
## 1:   c------c   c--------c
## 2: c---------c
## r:
##test_setdiff_interval_interval_1_inside_2__more <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:02 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:10 -> 2015-01-01 12:00:12+"))
i2 <- as.nanoival("+2015-01-01 12:00:01 -> 2015-01-01 12:00:06+")
r  <- as.nanoival("+2015-01-01 12:00:10 -> 2015-01-01 12:00:12+")
expect_identical(setdiff(i1, i2), r)

## time-time
expect_identical(setdiff(nanotime(1:10), nanotime(2:9)), c(nanotime(1), nanotime(10)))


## ops +, -
##test_minus_nanoival_any  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expect_error(i1 - "a", "invalid operand types")

##test_minus_nanoival_integer64  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(as.nanoival("+2015-01-01 12:00:02 -> 2015-01-01 12:00:04+"),
              as.nanoival("+2015-01-01 12:00:07 -> 2015-01-01 12:00:09-"))
expect_identical(i1 - as.integer64(one_second), expected)

##test_minus_nanoival_numeric  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(as.nanoival("+2015-01-01 12:00:02 -> 2015-01-01 12:00:04+"),
              as.nanoival("+2015-01-01 12:00:07 -> 2015-01-01 12:00:09-"))
expect_identical(i1 - one_second, expected)

##test_minus_nanoival_integer  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(as.nanoival("+2015-01-01 12:00:02 -> 2015-01-01 12:00:04+"),
              as.nanoival("+2015-01-01 12:00:07 -> 2015-01-01 12:00:09-"))
expect_identical(i1 - as.integer(one_second), expected)

##test_minus_any_nanoival  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expect_error(as.integer(one_second) - i1, "invalid operand types")

##test_minus_nanoival_nanoival  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expect_error(i1 - i1, "invalid operand types")


##test_plus_nanoival_any  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expect_error(i1 + "a", "invalid operand types")

##test_plus_nanoival_integer64  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(as.nanoival("+2015-01-01 12:00:04 -> 2015-01-01 12:00:06+"),
              as.nanoival("+2015-01-01 12:00:09 -> 2015-01-01 12:00:11-"))
expect_identical(i1 + as.integer64(one_second), expected)

##test_plus_nanoival_numeric  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(as.nanoival("+2015-01-01 12:00:04 -> 2015-01-01 12:00:06+"),
              as.nanoival("+2015-01-01 12:00:09 -> 2015-01-01 12:00:11-"))
expect_identical(i1 + one_second, expected)

##test_plus_nanoival_integer  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(as.nanoival("+2015-01-01 12:00:04 -> 2015-01-01 12:00:06+"),
              as.nanoival("+2015-01-01 12:00:09 -> 2015-01-01 12:00:11-"))
expect_identical(i1 + as.integer(one_second), expected)

##test_plus_any_nanoival  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expect_error(as.character("a") + i1, "invalid operand types")

##test_plus_nanoival_nanoival  <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expect_error(i1 + i1, "invalid operand types")

##test_plus_numeric_nanoival <- function() {
i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(as.nanoival("+2015-01-01 12:00:04 -> 2015-01-01 12:00:06+"),
              as.nanoival("+2015-01-01 12:00:09 -> 2015-01-01 12:00:11-"))
expect_identical(one_second + i1, expected)

##test_plus_integer64_nanoival <- function() {
i1 <- c(a=as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
        b=as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
expected <- c(a=as.nanoival("+2015-01-01 12:00:04 -> 2015-01-01 12:00:06+"),
              b=as.nanoival("+2015-01-01 12:00:09 -> 2015-01-01 12:00:11-"))
expect_identical(as.integer64(one_second) + i1, expected)


## Groups
##test_Arith_nanoival <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
expect_error(i1 ^ i2, "operation not defined for \"nanoival\" objects")

##test_Compare_nanoival <- function() {
i1 <- as.nanoival(aa)
expect_error(i1 < 2, "invalid operand types")

##test_Logic_nanoival <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
expect_error(i1 & i2, "operations are possible only for numeric, logical or complex types")
expect_error(i1 & 2,  "operations are possible only for numeric, logical or complex types")
expect_error(1  | i2,  "operations are possible only for numeric, logical or complex types")

##test_Math_nanoival <- function() {
i1 <- as.nanoival(aa)
expect_error(abs(i1), "non-numeric argument to mathematical function")

##test_Math2_nanoival <- function() {
i1 <- as.nanoival(aa)
expect_error(round(i1), "non-numeric argument to mathematical function")

##test_Summary_nanoival <- function() {
i1 <- as.nanoival(aa)
expect_error(prod(i1), "invalid 'type' \\(nanoival\\) of argument")  

##test_Complex_nanoival <- function() {
i1 <- as.nanoival(aa)
expect_error(Arg(i1), "non-numeric argument to function")  


## Subsetting
##test_subset_logical <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(i1, i2, i3)
expect_identical(ii[c(T,F,T)], c(i1, i3))
expect_identical(ii[c(T,F,NA)], c(i1, NA_nanoival_))
expect_identical(ii[-1], c(i2, i3))
expect_warning(ii[c(T,F,T), 3], "unused indices or arguments in 'nanoival' subsetting")
  
##test_subset_logical_named <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(a=i1, b=i2, c=i3)
expect_identical(ii[c(T,F,T)], c(a=i1, c=i3))
  
##test_subset_numeric <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(i1, i2, i3)
expect_identical(ii[c(1,3)], c(i1, i3))
expect_identical(ii[c(1,NA)], c(i1, NA_nanoival_))
expect_warning(ii[c(1,3), 3, 5], "unused indices or arguments in 'nanoival' subsetting")
  
##test_subset_numeric_named <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(a=i1, b=i2, c=i3)
expect_identical(ii[c(1,3)], c(a=i1, c=i3))

## test subset character
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(a=i1, b=i2, c=i3)
expect_identical(ii[c("a","b")], ii[1:2])
res  <- c(a=i1, NA_nanoival_)
names(res)[2] <- NA_character_
expect_identical(ii[c("a","x")], res)
expect_warning(ii["a", "b"], "unused indices or arguments in 'nanoival' subsetting")

## test subset error
expect_error(as.nanoival(aa)[as.integer64(1)], "']' not defined for on 'nanoival' for index of type 'ANY'")
    

##test_subassign_logical <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(i1, i2, i3)
ii[c(F,T,T)] <- i1
expect_identical(ii, c(i1, i1, i1))
  
##test_subassign_logical_named <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(a=i1, b=i2, c=i3)
ii[c(F,T,T)] <- i1
expect_identical(ii, c(a=i1, b=i1, c=i1))
  
##test_subassign_numeric <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(i1, i2, i3)
ii[2:3] <- i1
expect_identical(ii, c(i1, i1, i1))
  
##test_subassign_numeric_named <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(a=i1, b=i2, c=i3)
ii[2:3] <- i1
expect_identical(ii, c(a=i1, b=i1, c=i1))
  
##test_square_bracket <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
nn <- c(a=i1, b=i2, c=i3)
nn_nonames <- c(i1, i2, i3)
expect_identical(nn_nonames[1], nn[[1]])
expect_identical(nn_nonames[2], nn[[2]])
expect_identical(nn_nonames[3], nn[[3]])



## transposition, which is identity, as for 'POSIXct' for example:
##test_t_nanoival <- function() {
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
i3 <- as.nanoival(cc)
ii <- c(a=i1, b=i2, c=i3)
expect_identical(t(ii), ii)
  
## NA stuff
expect_true(is.na(nanoival(NA_nanotime_, NA_nanotime_, NA, NA)))
expect_true(is.na(nanoival(NA_nanotime_, nanotime(1), TRUE, TRUE)))
expect_true(is.na(nanoival(nanotime(1), NA_nanotime_, TRUE, TRUE)))
expect_true(is.na(nanoival(nanotime(1), nanotime(2), NA, TRUE)))
expect_true(is.na(nanoival(nanotime(1), nanotime(2), TRUE, NA)))
nn <- nanoival(nanotime(1:10), nanotime(2:11))
is.na(nn) <- 1:3
expect_true(all(is.na(nn[1:3])))
expect_true(!any(is.na(nn[4:10])))
expect_true(is.na(NA_nanoival_))
expect_true(is.na(nanoival.start(NA_nanoival_)))
expect_true(is.na(nanoival.end(NA_nanoival_)))
expect_true(is.na(nanoival.sopen(NA_nanoival_)))
expect_true(is.na(nanoival.eopen(NA_nanoival_)))

## overflow
n1 <- nanotime(1)
n2 <- nanotime(2)
ni <- nanoival(n1, n2)
prd <- as.nanoperiod("200y")
if (!isSolaris) {
    expect_warning(plus (ni, prd, "America/New_York"), "NAs produced by time overflow \\(remember that interval times are coded with 63 bits\\)")
    expect_warning(minus(ni, prd, "America/New_York"), "NAs produced by time overflow \\(remember that interval times are coded with 63 bits\\)")
}
dur <- as.nanoduration(200*365*24*3600*1e9)
expect_warning(ni + dur, "NAs produced by time overflow \\(remember that interval times are coded with 63 bits\\)")
expect_warning(ni + dur, "NAs produced by time overflow \\(remember that interval times are coded with 63 bits\\)")


## test warnings for ops where the vectors have length mismatch:

##test_lt_size_mismatch <- function() {
x <- nanoival(nanotime(1), nanotime(2))
expect_warning(c(x,x) < c(x,x,x), "longer object length is not a multiple of shorter object length")
d  <- as.nanoduration(1)
expect_warning(c(x,x,x) + c(d,d), "longer object length is not a multiple of shorter object length")
## warnings should also happen on 'nanoival' construction:
s <- nanotime(1)
e <- nanotime(2)
expect_warning(nanoival(c(s,s), c(e,e,e)), "longer object length is not a multiple of shorter object length")
expect_warning(nanoival(c(s,s), c(e,e), c(T,T,T)), "longer object length is not a multiple of shorter object length")
expect_warning(nanoival(c(s,s), c(e,e), c(T,T), c(F,F,F)), "longer object length is not a multiple of shorter object length")

## test that recycling is really correct, despite the warning:
s <- nanotime(1e9)
e <- nanotime(2e9)
ni  <- nanoival(s, e) + as.nanoduration(1:5*1e9)
d  <- as.nanoduration(1:2*1e9)
expect_identical(ni + d, ni + c(d, d, d[1]))

## test S4 conversions:
expect_identical(as.nanoival(aa), as(aa, "nanoival"))

## test seq:

x <- as.nanoival("+2013-01-01 15:00:00 -> 2013-01-01 17:00:00-")
if (!isSolaris) {
    expect_identical(seq(x, by=as.nanoperiod("1m"), length.out=4, tz="America/New_York"),
                     nanoival(seq(nanoival.start(x), by=as.nanoperiod("1m"), length.out=4, tz="America/New_York"),
                              seq(nanoival.end(x),   by=as.nanoperiod("1m"), length.out=4, tz="America/New_York"),
                              FALSE, TRUE))
}
y <- as.nanoival("+2013-01-04 15:00:00 -> 2013-01-04 17:00:00-")
expect_identical(seq(x, y, by=as.nanoduration("24:00:00")),
                 nanoival(seq(nanoival.start(x), by=as.nanoperiod("1d"), length.out=4, tz="UTC"),
                          seq(nanoival.end(x),   by=as.nanoperiod("1d"), length.out=4, tz="UTC"),
                          FALSE, TRUE))
expect_identical(seq(x, y, length.out=4),
                 nanoival(seq(nanoival.start(x), by=as.nanoperiod("1d"), length.out=4, tz="UTC"),
                          seq(nanoival.end(x),   by=as.nanoperiod("1d"), length.out=4, tz="UTC"),
                          FALSE, TRUE))

## 0-length ops:
## ------------

## constructor:
expect_identical(nanoival(nanotime(), nanotime(1)), nanoival())
expect_identical(nanoival(nanotime(), nanotime()), nanoival())
expect_identical(nanoival(nanotime(1), nanotime()), nanoival())
expect_identical(nanoival(nanotime(1), nanotime(2), logical()), nanoival())
expect_identical(nanoival(nanotime(1), nanotime(2), logical(), logical()), nanoival())
expect_identical(nanoival(nanotime(), nanotime(), logical(), logical()), nanoival())

expect_identical(as.nanoival(character()), nanoival())

## Comp:
expect_identical(nanoival(nanotime(1), nanotime(2)) > nanoival(), logical())
expect_identical(nanoival(nanotime(1), nanotime(2)) < nanoival(), logical())
expect_identical(nanoival() <= nanoival(nanotime(1), nanotime(2)) , logical())
expect_identical(nanoival() != nanoival(nanotime(1), nanotime(2)) , logical())

## ops
expect_identical(nanoival(nanotime(1), nanotime(2)) + integer(), as.nanoival())
expect_identical(nanoival(nanotime(1), nanotime(2)) - integer(), as.nanoival())
expect_identical(integer() + nanoival(), as.nanoival())

## accessors
expect_identical(nanoival.start(nanoival()), nanotime())
expect_identical(nanoival.end(nanoival()), nanotime())
expect_identical(nanoival.sopen(nanoival()), logical())
expect_identical(nanoival.eopen(nanoival()), logical())

## set ops:
expect_identical(union(nanoival(nanotime(1), nanotime(2)), nanoival()), nanoival(nanotime(1), nanotime(2)))
expect_identical(intersect(nanoival(nanotime(1), nanotime(2)), nanoival()), nanoival())
expect_identical(setdiff(nanoival(nanotime(1), nanotime(2)), nanoival()), nanoival(nanotime(1), nanotime(2)))
expect_identical(setdiff.idx(nanotime(), nanoival()), numeric())
expect_identical(setdiff.idx(nanotime(1), nanoival()), 1)
expect_identical(setdiff.idx(nanotime(), nanoival(nanotime(1), nanotime(2))), numeric())
expect_identical(intersect(nanotime(1), nanoival()), nanotime())
expect_identical(intersect(nanotime(), nanoival(nanotime(1), nanotime(2))), nanotime())
expect_identical(intersect.idx(nanotime(1), nanoival()), list(x=numeric(), y=numeric()))
expect_identical(intersect.idx(nanotime(), nanoival(nanotime(1), nanotime(2))), list(x=numeric(), y=numeric()))
expect_identical(nanotime() %in% nanoival(nanotime(1), nanotime(2)), logical(0))
expect_identical(nanotime(1:10) %in% nanoival(), rep(FALSE, 10))
expect_identical(is.na(as.nanoival()), logical())

## all.equal:
expect_identical(all.equal(nanoival(nanotime(1), nanotime(2)), nanoival(nanotime(1), nanotime(2))), TRUE)
expect_false(isTRUE(all.equal(nanoival(nanotime(1), nanotime(2)), "A")))
expect_identical(all.equal(nanoival(nanotime(1), nanotime(2)), NA_nanoival_), "'is.NA' value mismatch: 1 in current 0 in target")

## rep
i1 <- as.nanoival(aa)
i2 <- as.nanoival(bb)
expect_identical(rep(i1, 2), c(i1, i1))
expect_identical(rep(c(i1,i2), each=2), c(i1, i1, i2, i2))

options(nanotimeFormat=savedFormat)

