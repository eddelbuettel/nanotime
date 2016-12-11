
##' Nanosecond resolution datetime functionality
##'
##' Functions to operate on nanosecond time resolution.
##'
##' @section Caveats:
##'
##' Working with dates and times is \emph{difficult}. One
##' needs a representation of both \emph{time points} and
##' \emph{time duration}. In R, think of \code{Date} or
##' \code{POSIXct} objects for the former, and \code{difftime}
##' for the later. Here we (currently) only have time points,
##' but they are effectively also durations relative to the
##' epoch of January 1, 1970.
##'
##' @section Design:
##'
##' There are two external libraries doing two key components.
##'
##' We rely on the \code{\link{bit64}} package for \code{integer64}
##' types to represent nanoseconds relative to the epoch.  This is
##' similar to \code{POSIXct} which uses fractional seconds since the
##' epoch---so here we are essentially having the same values, but
##' multiplied by 10 to the power 9 and stored as integers.  We need
##' to rely on the external package as we require 64 bit integers
##' whereas R itself onky has 32 bit integers.  The
##' \code{\link{bit64}} package is clever about how it manages to
##' provide such an integer using only the 64-bit double type and very
##' clever (and efficient) transformations.
##'
##' The other is the CCTZ library in C++. It extending the C++11 standard
##' library type \code{chrono} type in very useful ways for time zones and
##' localtime.  We use its formating and parsing features.
##'
##' @param x The object which want to convert to class \code{nanotime}
##' @param ... Required for print method signatures but ignored here
##' @author Dirk Eddelbuettel
##' @examples
##' x <- nanotime("1970-01-01T00:00:00.000000001+00:00")
##' print(x)
##' x <- x + 1
##' print(x)
##' x <- x + 10
##' print(x)
nanotime <- function(x) {
    ## generic function, converts an object to a raw
    UseMethod("nanotime")
}

##' @rdname nanotime
nanotime.default <- function(x) {
    oldClass(x) <- c("integer64", "nanotime")
    x
}

##' @rdname nanotime
nanotime.numeric <- function(x) {
    y <- as.integer64(x)
    oldClass(y) <- c("integer64", "nanotime")
    y
}

##' @rdname nanotime
nanotime.character <- function(x) {
    d <- RcppCCTZ:::parseDouble(x)
    y <- as.integer64(d[,1]) * 1e9 + as.integer64(d[, 2])
    oldClass(y) <- c("integer64", "nanotime")
    y
}

##' @rdname nanotime
nanotime.matrix <- function(x) {
    y <- as.integer64(x[,1]) * 1e9 + as.integer64(x[, 2])
    oldClass(y) <- c("integer64", "nanotime")
    y
}

##' @rdname nanotime
nanotime.POSIXct <- function(x) {
    y <- as.integer64(as.numeric(x) * 1e9)
    oldClass(y) <- c("integer64", "nanotime")
    y
}


##' @rdname nanotime
print.nanotime <- function(x, ...) {
    ##z <- as.double(x) * 1e-9
    ##print(RcppCCTZ:::formatDouble(z))
    ##
    ## we get dispatched here from print.integer64 after its strips
    ## its class off, we pretty much do the same, this ends up printing
    ## the integer value(s) as character
    a <- attributes(x)
    ret <- as.character(x)
    a$class <- minusclass(a$class, "nanotime")
    attributes(ret) <- a
    print(ret, ...)
    invisible(x)
}

##' @rdname nanotime
index2char.nanotime <- function(x, frequency = NULL, digits = getOption("digits") - 3, ...) {
    secs  <- trunc(as.double(x/1e9))
    nanos <- as.double(x - secs*1e9)
    RcppCCTZ:::formatDouble(secs, nanos, ...)
}

##' @rdname nanotime
showNanotime <- function(x, ...) {
    print(index2char(x, ...))
}


## ## -- from bit64, but when class is c("integer64", "nanotime") we cannot override there
##
## print.iInteger64 <- function (x, quote = FALSE, ...) {
##     ## -- commented out:  cat("integer64\n")
##     a <- attributes(x)
##     ret <- as.character(x)
##     a$class <- minusclass(a$class, "integer64")
##     attributes(ret) <- a
##     print(ret, quote = quote, ...)
##     invisible(x)
## }
