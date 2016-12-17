
##' Nanosecond resolution datetime functionality
##'
##' Functions to operate on nanosecond time resolution using integer64
##' bit representation. Convertions functions for several standard R
##' types are provided, and more will be added as needed.
##'
##' Notice that the conversion from POSIXct explicitly sets the last
##' three digits to zero. Nanosecond time stored in a 64-bit integer
##' has nineteen digits precision where doubles (which are used
##' internally for POSIXct as well) only have sixteen digits.  So
##' rather than showing three more (essentially \emph{random}) digits
##' it is constructed such that these three additional digits are
##' zeros.
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
##' The other is the CCTZ library in C++, which we access via the
##' \code{\link{RcppCCTZ}} package. CCTZ extends the C++11 standard
##' library type \code{chrono} type in very useful ways for time zones and
##' localtime.  We use its formating and parsing features.
##'
##' @param x The object which want to convert to class \code{nanotime}
##' @param frequency Required for \code{index2char} method but ignored here
##' @param justify Required for \code{format} method but ignored here
##' @param digits Required for  \code{format} method but ignored here
##' @param na.encode Required for  \code{format} method but ignored here
##' @param trim Required for  \code{format} method but ignored here
##' @param tz Required for \code{as.POSIXct} and \code{as.POSIXlt},
##' can be set via \code{options("nanotimeFormat")} and uses \sQuote{UTC} as
##' a default and fallback
##' @param ... Required for print method signature but ignored here
##' @return A nanotime object
##' @author Dirk Eddelbuettel
##' @examples
##' x <- nanotime("1970-01-01T00:00:00.000000001+00:00")
##' print(x)
##' x <- x + 1
##' print(x)
##' format(x)
##' x <- x + 10
##' print(x)
##' format(x)
##' format(nanotime(Sys.time()) + 1:3)  # three elements each 1 ns apart
nanotime <- function(x) {
    ## generic function, converts an object to a raw
    UseMethod("nanotime")
}

##' @rdname nanotime
nanotime.default <- function(x) {
    oldClass(x) <- c("nanotime", "integer64")
    x
}

##' @rdname nanotime
nanotime.numeric <- function(x) {
    y <- as.integer64(x)
    oldClass(y) <- c("nanotime", "integer64")
    y
}

##' @rdname nanotime
nanotime.character <- function(x) {
    fmt <- getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%E*S%Ez")
    tz <- getOption("nanotimeTz", default="UTC")
    d <- RcppCCTZ::parseDouble(x, fmt=fmt, tz=tz)
    y <- as.integer64(d[,1]) * 1e9 + as.integer64(d[, 2])
    oldClass(y) <- c("nanotime", "integer64")
    y
}

##' @rdname nanotime
nanotime.matrix <- function(x) {
    y <- as.integer64(x[,1]) * 1e9 + as.integer64(x[, 2])
    oldClass(y) <- c("nanotime", "integer64")
    y
}

##' @rdname nanotime
nanotime.POSIXct <- function(x) {
    y <- as.integer64(as.numeric(x) * 1e6) * 1000 # force last three digits to be zero
    oldClass(y) <- c("nanotime", "integer64")
    y
}

##' @rdname nanotime
nanotime.POSIXlt <- function(x) {
    nanotime(as.POSIXct(x))
}

##' @rdname nanotime
nanotime.Date <- function(x) {
    nanotime(as.POSIXct(x))
}

##' @rdname nanotime
print.nanotime <- function(x, ...) {
    ##NextMethod()	# cleaner ?

    ## the following borrows from bit64::print.integer64
    a <- attributes(x)
    ret <- x
    a$class <- minusclass(a$class, "nanotime")
    attributes(ret) <- a
    print(ret, ...)

    invisible(x)
}

##' @rdname nanotime
format.nanotime <- function(x, 
                            justify="right",
                            digits=NULL,
                            na.encode=FALSE,
                            trim=TRUE,
                            ...) {
    fmt <- getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%E*S%Ez")
    tz <- getOption("nanotimeTz", default="UTC")
    secs  <- trunc(as.double(x/1e9))
    nanos <- as.double(x - secs*1e9)
    RcppCCTZ::formatDouble(secs, nanos, fmt=fmt, tgttzstr=tz)
}

##' @rdname nanotime
index2char.nanotime <- function(x, frequency = NULL, ...) {
    secs  <- trunc(as.double(x/1e9))
    nanos <- as.double(x - secs*1e9)
    RcppCCTZ::formatDouble(secs, nanos,
                           fmt=getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%E*S%Ez"),
                           tgttzstr=getOption("nanotimeTz", default="UTC"))
}

##' @rdname nanotime
as.POSIXct.nanotime <- function(x, tz, ...) {
    if (missing(tz)) tz <- getOption("nanotimeTz", default="UTC")
    #if (verbose) warning("Lossy conversion dropping precision")
    pt <- as.POSIXct(as.double(x/1e9), tz=tz, origin="1970-01-01")
    pt
}

##' @rdname nanotime
as.POSIXlt.nanotime <- function(x, tz, ...) {
    if (missing(tz)) tz <- getOption("nanotimeTz", default="UTC")
    as.POSIXlt(as.POSIXct(x, tz=tz))
}

##' @rdname nanotime
as.Date.nanotime <- function(x, ...) {
    as.Date(as.POSIXct(x))
}

##' @rdname nanotime
as.data.frame.nanotime <- function(x, ...) {
    cl <- oldClass(x)
    on.exit(attr(x, "class") <- cl)
    attr(x, "class") <- minusclass(cl, "nanotime")
    ret <- as.data.frame(x, ...)
    k <- length(ret)
    for (i in 1:k) attr(ret[[i]], "class") <- cl
    ret
}
