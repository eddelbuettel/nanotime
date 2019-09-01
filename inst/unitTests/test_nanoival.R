

savedFormat <- NULL
one_second  <- 1e9
aa <- "+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"
bb <- "+2014-01-01 00:00:00 -> 2015-01-01 00:00:00+"
cc <- "-2015-01-01 00:00:00 -> 2016-01-01 00:00:00-"
dd <- "-2016-01-01 00:00:00 -> 2017-01-01 00:00:00+"


.setUp <- function() {
    savedFormat <- options()$nanotimeFormat
    options(nanotimeFormat="%Y-%m-%d %H:%M:%S")
}

.tearDown <- function() {
    options(nanotimeFormat=savedFormat)
}

## nanotime constructors/accessors
test_as.nanoival <- function() {
    ni <- as.nanoival(aa)
    checkEquals(start(ni),     nanotime("2013-01-01 00:00:00")) &
        checkEquals(end(ni),   nanotime("2014-01-01 00:00:00")) &
        checkEquals(sopen(ni), FALSE) &
        checkEquals(eopen(ni), TRUE)
}
test_as.nanoival_vector <- function() {
    ni <- as.nanoival(c(aa, bb, cc, dd))
    checkEquals(start(ni),  c(nanotime("2013-01-01 00:00:00"),
                              nanotime("2014-01-01 00:00:00"),
                              nanotime("2015-01-01 00:00:00"),
                              nanotime("2016-01-01 00:00:00"))) &
      checkEquals(end(ni),  c(nanotime("2014-01-01 00:00:00"),
                              nanotime("2015-01-01 00:00:00"),
                              nanotime("2016-01-01 00:00:00"),
                              nanotime("2017-01-01 00:00:00"))) &
      checkEquals(sopen(ni), c(FALSE, FALSE, TRUE, TRUE)) &
      checkEquals(eopen(ni), c(TRUE, FALSE, TRUE, FALSE))
}
test_nanoival <- function() {
    checkEquals(nanoival(nanotime("2013-01-01 00:00:00"), nanotime("2014-01-01 00:00:00"), TRUE, TRUE),
                as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+"))
}
test_nanoival_vector<- function() {
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
    checkEquals(nanoival(starts, ends, sopens, eopens), as.nanoival(c(aa, bb, cc, dd)))
}


## show/print/format
test_show <- function() {
  ival_str <- "+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"
  ival <- as.nanoival(ival_str)
  checkEquals(show(ival), ival_str)
  
  ival_str <- "-2013-01-01 00:00:00 -> 2014-01-01 00:00:00+"
  ival <- as.nanoival(ival_str)
  checkEquals(show(ival), ival_str)
  
  savedFormat <- options()$nanotimeFormat
  options(nanotimeFormat=NULL)
  
  ival_str <- "+2013-01-01T00:00:00.000000000+00:00 -> 2014-01-01T00:00:00.000000000+00:00-"
  ival <- as.nanoival(ival_str)
  checkEquals(show(ival), ival_str)
  
  options(nanotimeFormat=savedFormat)
}

test_format <- function() {
  ival_str <- "+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"
  ival <- as.nanoival(ival_str)
  checkEquals(format(ival), ival_str)
  
  ival_str <- "-2013-01-01 00:00:00 -> 2014-01-01 00:00:00+"
  ival <- as.nanoival(ival_str)
  checkEquals(format(ival), ival_str)
  
  savedFormat <- options()$nanotimeFormat
  options(nanotimeFormat=NULL)
  
  ival_str <- "+2013-01-01T00:00:00.000000000+00:00 -> 2014-01-01T00:00:00.000000000+00:00-"
  ival <- as.nanoival(ival_str)
  checkEquals(format(ival), ival_str)
  
  options(nanotimeFormat=savedFormat)
}

## as.data.frame
test_as.data.frame <- function() {
  ni <- as.nanoival(c(aa, bb, cc, dd))
  checkEquals(as.data.frame(ni), data.frame(x=ni))

  rownames <- c("aa","bb","cc","dd")
  names(ni) <- rownames
  as.data.frame(ni)
  checkEquals(as.data.frame(ni), data.frame(x=ni, row.names=rownames))
}

## equality and comparison operators

## eq/ne
test_eq <- function() {
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkTrue(x==x)
    y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkTrue(!(x==y))
    z <- as.nanoival("+2013-01-01 00:00:01 -> 2014-01-01 00:00:00-")
    checkTrue(!(x==z))
}
test_ne <- function() {
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkTrue(!(x!=x))
    y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkTrue(x!=y)
    z <- as.nanoival("+2013-01-01 00:00:01 -> 2014-01-01 00:00:00-")
    checkTrue(x!=z)
}
    
## lt
test_lt_non_overlapping <- function() {
    ## x: c----------o
    ## y:              c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x < y, TRUE) &
        checkEquals(y < x, FALSE)
}
test_lt_overlapping <- function() {
    ## x: c----------o
    ## y:       c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x < y, TRUE) &
        checkEquals(y < x, FALSE)
}
test_lt_same_end <- function() {
    ## x: c----------o
    ## y:       c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x < y, TRUE) &
        checkEquals(y < x, FALSE)
}
test_lt_included <- function() {
    ## x: c----------o
    ## y:    c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
    checkEquals(x < y, TRUE) &
        checkEquals(y < x, FALSE)
}
test_lt_same_start<- function() {
    ## x: c----------o
    ## y: c--------------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(x < y, TRUE) &
        checkEquals(y < x, FALSE)
}
test_lt_same_open_start <- function() {
    ## x: c----------o
    ## y: o----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x < y, TRUE) &
        checkEquals(y < x, FALSE)
}
test_lt_same_open_end <- function() {
    ## x: c----------o
    ## y: c----------c
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
    checkEquals(x < y, TRUE) &
        checkEquals(y < x, FALSE)
}
test_lt_same <- function() {
    ## x: c----------o
    ## y: c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x < y, FALSE) &
        checkEquals(y < x, FALSE)
}
test_lt_same <- function() {
    ## x: c----------o
    ## y: c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x < y, FALSE) &
        checkEquals(y < x, FALSE)
}
test_lt_multiple <- function() {
    ## x: c----------o
    ## y:       c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2014-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(c(x, y) < c(y, x), c(TRUE, FALSE))
}
test_lt_size_mismatch <- function() {
    ## x: c----------o
    ## y: c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkException(x < c(x,x), "object lengths mismatch")
}

## le
test_le_non_overlapping <- function() {
    ## x: c----------o
    ## y:              c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, FALSE)
}
test_le_overlapping <- function() {
    ## x: c----------o
    ## y:       c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, FALSE)
}
test_le_same_end <- function() {
    ## x: c----------o
    ## y:       c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, FALSE)
}
test_le_included <- function() {
    ## x: c----------o
    ## y:    c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, FALSE)
}
test_le_same_start<- function() {
    ## x: c----------o
    ## y: c--------------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, FALSE)
}
test_le_same_open_start <- function() {
    ## x: c----------o
    ## y: o----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, FALSE)
}
test_le_same_open_end <- function() {
    ## x: c----------o
    ## y: c----------c
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, FALSE)
}
test_le_same <- function() {
    ## x: c----------o
    ## y: c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x <= y, TRUE) &
        checkEquals(y <= x, TRUE)
}

## gt
test_gt_non_overlapping <- function() {
    ## x: c----------o
    ## y:              c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, TRUE)
}
test_gt_overlapping <- function() {
    ## x: c----------o
    ## y:       c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, TRUE)
}
test_gt_same_end <- function() {
    ## x: c----------o
    ## y:       c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, TRUE)
}
test_gt_included <- function() {
    ## x: c----------o
    ## y:    c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, TRUE)
}
test_gt_same_start<- function() {
    ## x: c----------o
    ## y: c--------------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, TRUE)
}
test_gt_same_open_start <- function() {
    ## x: c----------o
    ## y: o----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, TRUE)
}
test_gt_same_open_end <- function() {
    ## x: c----------o
    ## y: c----------c
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, TRUE)
}
test_gt_same <- function() {
    ## x: c----------c
    ## y: c----------c
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
    checkEquals(x > y, FALSE) &
        checkEquals(y > x, FALSE)
}

## ge
test_ge_non_overlapping <- function() {
    ## x: c----------o
    ## y:              c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x >= y, FALSE) &
        checkEquals(y >= x, TRUE)
}
test_ge_overlapping <- function() {
    ## x: c----------o
    ## y:       c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(x >= y, FALSE) &
        checkEquals(y >= x, TRUE)
}
test_ge_same_end <- function() {
    ## x: c----------o
    ## y:       c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x >= y, FALSE) &
        checkEquals(y >= x, TRUE)
}
test_ge_included <- function() {
    ## x: c----------o
    ## y:    c----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-06-01 00:00:00 -> 2013-07-01 00:00:00-")
    checkEquals(x >= y, FALSE) &
        checkEquals(y >= x, TRUE)
}
test_ge_same_start<- function() {
    ## x: c----------o
    ## y: c--------------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(x >= y, FALSE) &
        checkEquals(y >= x, TRUE)
}
test_ge_same_open_start <- function() {
    ## x: c----------o
    ## y: o----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x >= y, FALSE) &
        checkEquals(y >= x, TRUE)
}
test_ge_same_open_end <- function() {
    ## x: c----------o
    ## y: c----------c
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00+")
    checkEquals(x >= y, FALSE) &
        checkEquals(y >= x, TRUE)
}
test_ge_same <- function() {
    ## x: o----------o
    ## y: o----------o
    x <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("-2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    checkEquals(x >= y, TRUE) &
        checkEquals(y >= x, TRUE)
}


## sorting/ordering
test_is_unsorted_non_overlapping <- function() {
    ## x: c----------o
    ## y:              c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2015-01-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(is.unsorted(c(x, y)), FALSE) &
        checkEquals(is.unsorted(c(y, x)), TRUE)
}
test_is_unsorted_overlapping <- function() {
    ## x: c----------o
    ## y:       c--------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    y <- as.nanoival("+2014-01-01 00:00:00 -> 2016-01-01 00:00:00-")
    checkEquals(is.unsorted(c(x, y)), FALSE) &
        checkEquals(is.unsorted(c(y, x)), TRUE)
}
test_is_unsorted_same_end <- function() {
    ## x: c----------o
    ## y:      c-----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    y <- as.nanoival("+2014-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(is.unsorted(c(x, y)), FALSE) &
        checkEquals(is.unsorted(c(y, x)), TRUE)
}
test_is_unsorted_included <- function() {
    ## x: c----------o
    ## y:   c-----o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2016-01-01 00:00:00-")
    y <- as.nanoival("+2014-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(is.unsorted(c(x, y)), FALSE) &
        checkEquals(is.unsorted(c(y, x)), TRUE)
}
test_is_unsorted_same_start <- function() {
    ## x: c----o
    ## y: c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(is.unsorted(c(x, y)), FALSE) &
        checkEquals(is.unsorted(c(y, x)), TRUE)
}
test_is_unsorted_not_strictly <- function() {
    ## x: c----o
    ## y: c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(is.unsorted(c(x, x, y, y)), FALSE) &
        checkEquals(is.unsorted(c(y, y, x, x)), TRUE)
}
test_is_unsorted_strictly <- function() {
    ## x: c----o
    ## y: c----------o
    x <- as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-")
    y <- as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-")
    checkEquals(is.unsorted(c(x, x, y, y), strictly=TRUE), TRUE) &
        checkEquals(is.unsorted(c(y, y, x, x), strictly=TRUE), TRUE)
}


## c, subset, subassign and binds
test_c <- function() {                  # LLL
  a <- c(nanotime(1), nanotime(2))
  checkEquals(a, nanotime(1:2))

  a <- c(nanotime(1:2), nanotime(3:4))
  checkEquals(a, nanotime(1:4))
}
test_c_name <- function() {
    c_xy <- c(x=as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"),
              y=as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-"))
    checkEquals(names(c_xy), c("x","y"))
}
test_c_name_assign <- function() {
    c_xy <- c(x=as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"),
              y=as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-"))
    names(c_xy) <- c("a","b")
    checkEquals(names(c_xy), c("a","b"))
}
test_c_name_assign_null <- function() {
    c_xy <- c(x=as.nanoival("+2013-01-01 00:00:00 -> 2014-01-01 00:00:00-"),
              y=as.nanoival("+2013-01-01 00:00:00 -> 2015-01-01 00:00:00-"))
    names(c_xy) <- NULL
    checkTrue(is.null(names(c_xy)))
}
test_subset <- function() {
  a <- nanotime(1:10)
  checkEquals(a[3], nanotime(3))
  checkEquals(a[1:10], a)
}
test_subsassign <- function() {
  a <- nanotime(1:10)
  a[3] <- nanotime(13)
  checkEquals(a[3], nanotime(13))
  a[1:10] <- nanotime(10:1)
  checkEquals(a[1:10], nanotime(10:1))
}
## test subassign numeric and character LLL


## set operations

## naming convention for interval tests:
## cc : start closed, end closed
## co : start closed, end open
## oc : etc.
## oo

## intersection
## --------------------------------------------------------------------------

## time - interval

## this should be legal, but for the moment as.nanoival(NULL) doesn't work LLL
## RUnit_intersect_time_interval_null_interval <- function() {
##     i1 <- as.nanoival(NULL)
##     s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
##     checkEquals(s1[i1], s1)
## }

## time - interval
## 1: ..............
## 2:   c----c
## r:   ......
test_intersect_idx_time_interval_cc <- function() {
    a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
    idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
    r <- list(x=c(3,4,5,6,7,8), y=c(1,1,1,1,1,1))
    checkEquals(intersect.idx(a, idx), r)
}
## 1: ..............
## 2:   c----c
## r:   ......
test_intersect_time_interval_cc <- function() {
    a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
    idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+")
    r <- seq(nanotime("2012-12-12 12:12:14"), nanotime("2012-12-12 12:12:19"), by=one_second)
    checkEquals(a[idx], r)
}
## 1: ..............
## 2:   o----o
## r:    ....
test_intersect_time_interval_oo <- function() {
    a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
    idx <- as.nanoival("-2012-12-12 12:12:14 -> 2012-12-12 12:12:19-")
    r <- seq(nanotime("2012-12-12 12:12:15"), nanotime("2012-12-12 12:12:18"), by=one_second)
    checkEquals(a[idx], r)
}
## 1:   ......
## 2: o---------o
## r:   ......
test_intersect_time_interval_overlapping <- function() {
    a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
    idx <- as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:30-")
    checkEquals(a[idx], a)
}
## 1:   .................
## 2: o-----o   c-----c
## r:  .....    .......
test_intersect_time_interval_multiple <- function() {
    a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
    idx <- c(as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:14-"),
             as.nanoival("+2012-12-12 12:12:18 -> 2012-12-12 12:12:20+"))
    r <- c(seq(nanotime("2012-12-12 12:12:12"), nanotime("2012-12-12 12:12:13"), by=one_second),
           seq(nanotime("2012-12-12 12:12:18"), nanotime("2012-12-12 12:12:20"), by=one_second))
    checkEquals(a[idx], r)
}
test_intersect_time_interval_multiple_direct_call  <- function() {
    a <- seq(nanotime("2012-12-12 12:12:12"), length.out=10, by=one_second)
    idx <- c(as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:14-"),
             as.nanoival("+2012-12-12 12:12:18 -> 2012-12-12 12:12:20+"))
    r <- c(seq(nanotime("2012-12-12 12:12:12"), nanotime("2012-12-12 12:12:13"), by=one_second),
           seq(nanotime("2012-12-12 12:12:18"), nanotime("2012-12-12 12:12:20"), by=one_second))
    checkEquals(intersect(a, idx), r)
}

## interval - interval:
## 1: c-----------c
## 2: c-----------c
## r: c-----------c
test_intersect_interval_interval_cc_cc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- i2
    checkTrue(intersect(i1, i2) == r)
}
## 1: c-----------c
## 2: o-----------c
## r: o-----------c
test_intersect_interval_interval_cc_oc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- i2
    checkTrue(intersect(i1, i2) == r &
              intersect(i2, i1) == r)
}
## 1: c-----------c
## 2: c-----------o
## r: c-----------o
test_intersect_interval_interval_cc_co__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- i2
    checkTrue(intersect(i1, i2) == r &
              intersect(i2, i1) == r)
}
## 1: c-----------o
## 2: o-----------c
## r: o-----------o
test_intersect_interval_interval_co_oc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    checkTrue(intersect(i1, i2) == r &
              intersect(i2, i1) == r)
}
## 1: o-----------o
## 2: o-----------o
## r: o-----------o
test_intersect_interval_interval_oo_oo__2_eq_1 <- function() {
    i1 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- i2
    checkTrue(intersect(i1, i2) == r)
}
## 1: c-----------c
## 2: o-----------o
## r: o-----------o
test_intersect_interval_interval_cc_oo__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- i2
    checkTrue(intersect(i1, i2) == r)
}

## union
## --------------------------------------------------------------------------
## interval - interval:
## 1: c-----------c
## 2: c-----------c
## r: c-----------c
test_union_interval_interval_cc_cc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- i2
    checkTrue(union(i1, i2) == r &
              union(i2, i1) == r)
}
## 1: c-----------c
## 2: o-----------c
## r: c-----------c
test_union_interval_interval_cc_oc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- i1
    checkTrue(union(i1, i2) == r &
              union(i2, i1) == r)
}
## 1: c-----------c
## 2: c-----------o
## r: c-----------c
test_union_interval_interval_cc_co__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- i1
    checkTrue(union(i1, i2) == r &
              union(i2, i1) == r)
}
## 1: c-----------o
## 2: o-----------c
## r: c-----------c
test_union_interval_interval_co_oc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    checkTrue(union(i1, i2) == r &
              union(i2, i1) == r)
}
## 1: o-----------o
## 2: o-----------o
## r: o-----------o
test_union_interval_interval_oo_oo__2_eq_1 <- function() {
    i1 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- i2
    checkTrue(union(i1, i2) == r &
              union(i2, i1) == r)
}
## 1: c-----------c
## 2: o-----------o
## r: c-----------c
test_union_interval_interval_cc_oo__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- i1
    checkTrue(union(i1, i2) == r &
              union(i2, i1) == r)
}

## setdiff
## --------------------------------------------------------------------------

## time - interval:
## 1: as.nanoival("-----------")
## 2:    as.nanoival("---")
## r: as.nanoival("--")   as.nanoival("----")
test_setdiff_time_interval_cc__2_subset_of_1 <- function() {
    s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
    i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=3, by=one_second),
            seq(nanotime("2015-01-01 12:00:06"), length.out=4, by=one_second))
    checkEquals(setdiff(s1, i2), r)
}
test_setdiff_time_interval_oc__2_subset_of_1 <- function() {
    s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=4, by=one_second),
            seq(nanotime("2015-01-01 12:00:06"), length.out=4, by=one_second))
    checkEquals(setdiff(s1, i2), r)
}
test_setdiff_time_interval_co__2_subset_of_1 <- function() {
    s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
    i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=3, by=one_second),
            seq(nanotime("2015-01-01 12:00:05"), length.out=5, by=one_second))
    checkEquals(setdiff(s1, i2), r)
}
test_setdiff_time_interval_oo__2_subset_of_1 <- function() {
    s1 <- seq(nanotime("2015-01-01 12:00:00"), length.out=10, by=one_second)
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- c(seq(nanotime("2015-01-01 12:00:00"), length.out=4, by=one_second),
            seq(nanotime("2015-01-01 12:00:05"), length.out=5, by=one_second))
    checkEquals(setdiff(s1, i2), r)
}

## interval - interval:
## 1: c-----------c
## 2: c-----------c
## r:
## LLL
## test_setdiff_interval_interval_cc_cc__2_eq_1 <- function() {
##     i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
##     i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
##     r  <- vector(mode="interval", length=0)
##     checkEquals(setdiff(i1, i2), r)
## }
## 1: c-----------c
## 2: o-----------c
## r: c
test_setdiff_interval_interval_cc_oc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+")
    checkEquals(setdiff(i1, i2), r)
}
## 1: c-----------c
## 2: c-----------o
## r:             c
test_setdiff_interval_interval_cc_co__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:05+")
    checkEquals(setdiff(i1, i2), r)
}
## 1: c-----------o
## 2: o-----------c
## r: c
test_setdiff_interval_interval_co_oc__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+")
    checkEquals(setdiff(i1, i2), r)
}
## 1: o-----------o
## 2: o-----------o
## r:
## LLL
## test_setdiff_interval_interval_oo_oo__2_eq_1 <- function() {
##     i1 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
##     i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
##     r  <- vector(mode="interval", length=0)
##     checkEquals(setdiff(i1, i2), r)
## }
## 1: c-----------c
## 2: o-----------o
## r: c-----------c
test_setdiff_interval_interval_cc_oo__2_eq_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+"),
            as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:05+"))
    checkEquals(setdiff(i1, i2), r)
}
## 1: c-----------c
## 2:             c-----------o
## r: c-----------o            
test_setdiff_interval_interval_cc_co__2_gt_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
    r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    checkEquals(setdiff(i1, i2), r)
}
## 1: c-----------c
## 2:             o-----------o
## r: c-----------c            
test_setdiff_interval_interval_cc_oo__2_gt_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
    r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    checkEquals(setdiff(i1, i2), r)
}
## 1: c-----------c
## 2:             o-----------c
## r: c-----------c            
test_setdiff_interval_interval_cc_oc__2_gt_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    i2 <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07-")
    r  <- as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    checkEquals(setdiff(i1, i2), r)
}
## 1:             c-----------c
## 2: o-----------c
## r:             o-----------c
test_setdiff_interval_interval_cc_oc__2_lt_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05+")
    r  <- as.nanoival("-2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
    checkEquals(setdiff(i1, i2), r)
}
## 1:             c-----------c
## 2: o-----------o
## r:             c-----------c
test_setdiff_interval_interval_cc_oo__2_lt_1 <- function() {
    i1 <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+")
    checkEquals(setdiff(i1, i2), r)
}
## various tests where we add a third interval
## 1:             c-----------c c--------c
## 2: o-----------o
## r:             c-----------c c--------c
test_setdiff_interval_interval_cc_oo__2_lt_1_3rd <- function() {
    i1 <- c(as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+"),
            as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10+"))
    i2 <- as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:05-")
    r  <- c(as.nanoival("+2015-01-01 12:00:05 -> 2015-01-01 12:00:07+"),
            as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10+"))
    checkEquals(setdiff(i1, i2), r)
}
## 1: c-----------c        c--------o
## 2: o--------------------c
## r:                      o--------c
test_setdiff_interval_interval_cc_co_oc_3rd <- function() {
    i1 <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:05+"),
            as.nanoival("+2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
    i2 <-   as.nanoival("-2015-01-01 12:00:03 -> 2015-01-01 12:00:08+")
    r  <- c(as.nanoival("+2015-01-01 12:00:03 -> 2015-01-01 12:00:03+"),
            as.nanoival("-2015-01-01 12:00:08 -> 2015-01-01 12:00:10-"))
    checkEquals(setdiff(i1, i2), r)
}


## ops +, -


