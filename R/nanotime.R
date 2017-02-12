##' @import bit64
##' @importFrom RcppCCTZ parseDouble formatDouble
##' @importFrom zoo index2char
##' @import methods

##' @rdname nanotime
##' @export
setClass("nanotime", contains = "integer64")

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
##' @section Output Format:
##'
##' Formatting and character conversion for \code{nanotime} objects is
##' done by functions from the \code{\link{RcppCCTZ}} package relying
##' on code from its embedded \code{CCTZ} library. The default format
##' is ISO3339 compliant: \code{\%Y-\%m-\%dT\%H:\%M:\%E9S\%Ez}. It
##' specifies a standard ISO 8601 part for date and time --- as well
##' as nine digits of precision for fractional seconds (down to
##' nanoseconds) and on offset (typically zero as we default to UTC).
##' It can be overriden by using \code{options()} with the key of
##' \code{nanotimeFormat} and a suitable value. Similarly,
##' \code{nanotimeTz} can be used to select a different timezone.
##' 
##' @param x The object which want to convert to class \code{nanotime}
##' @param tz Required for \code{as.POSIXct} and \code{as.POSIXlt},
##' can be set via \code{options("nanotimeFormat")} and uses \sQuote{UTC} as
##' a default and fallback
##' @param ... further arguments passed to or from methods.
##' @param e1 left operand object 
##' @param e2 rigt operand object 
##' @param digits Required for \code{Math2} signature but ignored here
##' @param recursive argument for method \code{c}
##' @param object argument for method \code{show}
##' @param na.rm a logical indicating whether missing values should be removed.
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @param z Required for \code{Complex} signature but ignored here
##' @param deparse.level argument for methods \code{rbind} and \code{cbind}
##' @param value argument for \code{nanotime-class} 
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
## nanotime <- function(x) {
##     UseMethod("nanotime")    	# generic function,
## }
##' @export
nanotime <- function(x) {
  new("nanotime", as.integer64(x))
}

setGeneric("nanotime")

##' @rdname nanotime
##' @export
setMethod("nanotime",
          "character",
          function(x) {
            fmt <- getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%E9S%Ez")
            tz <- getOption("nanotimeTz", default="UTC")
            d <- RcppCCTZ::parseDouble(x, fmt=fmt, tz=tz)
            new("nanotime", as.integer64(d[,1]) * 1e9 + as.integer64(d[, 2]))
          })

##' @rdname nanotime
##' @export
## This does not lead to S3 dispatch, the call must be 'nanotime.matrix'
nanotime.matrix <- function(x) {
  new("nanotime", as.integer64(x[,1]) * 1e9 + as.integer64(x[, 2]))
}

##' @rdname nanotime
##' @export
setMethod("nanotime",
          "POSIXct",
          function(x) {
            ## force last three digits to be zero
            new("nanotime", as.integer64(as.numeric(x) * 1e6) * 1000)
          })

##' @rdname nanotime
##' @export
setMethod("nanotime",
          "POSIXlt",
          function(x) {
            nanotime(as.POSIXct(x))
          })

##' @rdname nanotime
##' @export
setMethod("nanotime",
          "Date",
          function(x) {
            nanotime(as.POSIXct(x))
          })

##' @rdname nanotime
##' @export
setMethod("print",
          "nanotime",
          function(x, ...) {
            print(format(x, ...))
            invisible(x)
          })

##' @rdname nanotime
##' @export
setMethod("show",
          signature("nanotime"),
          function(object) print(format(object)))


##' @rdname nanotime
##' @export
format.nanotime <- function(x, tz="", ...)
{
  if (missing(tz)) {
    if (!is.null(tzone <- attr(x, "tzone")))
      tz <- tzone
    else
      tz <- getOption("nanotimeTz", default="UTC")
  }
  fmt <- getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%E9S%Ez")
  bigint <- as.integer64(x)
  secs  <- as.integer64(bigint / as.integer64(1000000000))
  nanos <- bigint - secs * as.integer64(1000000000)
  RcppCCTZ::formatDouble(as.double(secs), as.double(nanos), fmt=fmt, tgttzstr=tz)
}


##' @rdname nanotime
##' @export
index2char.nanotime <- function(x, ...) {
  bigint <- as.integer64(x)
  secs  <- as.integer64(bigint / as.integer64(1000000000))
  nanos <- bigint - secs * as.integer64(1000000000)
  RcppCCTZ::formatDouble(as.double(secs), as.double(nanos),
                         fmt=getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%E9S%Ez"),
                         tgttzstr=getOption("nanotimeTz", default="UTC"))
}

##' @rdname nanotime
##' @export
as.POSIXct.nanotime <- function(x, tz, ...) {
  if (missing(tz)) {
    if (!is.null(tzone <- attr(x, "tzone")))
      tz <- tzone
    else
      tz <- getOption("nanotimeTz", default="UTC")
  }
  ## if (verbose) warning("Lossy conversion dropping precision")
  pt <- as.POSIXct(as.double(x/1e9), tz=tz, origin="1970-01-01")
  pt
}

##' @rdname nanotime
##' @export
as.POSIXlt.nanotime <- function(x, tz, ...) {
  as.POSIXlt(as.POSIXct(x, tz=tz))
}

##' @rdname nanotime
##' @export
as.Date.nanotime <- function(x, ...) {
  as.Date(as.POSIXct(x))
}

##' @rdname nanotime
##' @export
as.data.frame.nanotime <- function(x, ...) {
  ret <- as.data.frame(S3Part(x, strictS3=TRUE), ...)
  ## this works, but see if there's a more idiomatic and efficient way
  ## of doing this:
  ret[] <- nanotime(S3Part(x, strictS3=TRUE))
  ret
}

##' @rdname nanotime
##' @export
as.integer64.nanotime <- function(x, ...) {
  S3Part(x, strictS3=TRUE)
}


## ------------ `-`
##' @rdname nanotime
##' @export
setMethod("-", c("nanotime", "character"),
          function(e1, e2) {
            stop("invalid operand types")
          })

##' @rdname nanotime
##' @export
setMethod("-", c("nanotime", "nanotime"),
          function(e1, e2) {
            S3Part(e1, strictS3=TRUE) - S3Part(e2, strictS3=TRUE)
          })

##' @rdname nanotime
##' @export
setMethod("-", c("nanotime", "integer64"),
          function(e1, e2) {
            new("nanotime", S3Part(e1, strictS3=TRUE) - e2)
          })

##' @rdname nanotime
##' @export
setMethod("-", c("nanotime", "numeric"),
          function(e1, e2) {
            new("nanotime", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanotime
##' @export
setMethod("-", c("ANY", "nanotime"),
          function(e1, e2) {
            stop("invalid operand types")
          })



## ----------- `+`
##' @rdname nanotime
##' @export
setMethod("+", c("nanotime", "ANY"),
          function(e1, e2) {
            stop("invalid operand types")
          })

##' @rdname nanotime
##' @export
setMethod("+", c("nanotime", "integer64"),
          function(e1, e2) {
            new("nanotime", S3Part(e1, strictS3=TRUE) + e2)
          })

##' @rdname nanotime
##' @export
setMethod("+", c("nanotime", "numeric"),
          function(e1, e2) {
            new("nanotime", S3Part(e1, strictS3=TRUE) + e2)
          })

##' @rdname nanotime
##' @export
setMethod("+", c("ANY", "nanotime"),
          function(e1, e2) {
            stop("invalid operand types")
          })

##' @rdname nanotime
##' @export
setMethod("+", c("integer64", "nanotime"),
          function(e1, e2) {
            new("nanotime", e1 + S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanotime
##' @export
setMethod("+", c("numeric", "nanotime"),
          function(e1, e2) {
            new("nanotime", e1 + S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanotime
##' @export
setMethod("+", c("nanotime", "nanotime"),
          function(e1, e2) {
            stop("invalid operand types")
          })

## ---------- other ops

##' @rdname nanotime
##' @export
setMethod("Arith", c("nanotime", "ANY"),
          function(e1, e2) {
            callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname nanotime
##' @export
setMethod("Compare", c("nanotime", "ANY"),
          function(e1, e2) {
            callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname nanotime
##' @export
setMethod("Logic", c("nanotime", "ANY"),
          function(e1, e2) {
            ## this is the same error message that R gives for "A" | "A"
            stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanotime
##' @export
setMethod("Logic", c("ANY", "nanotime"),
          function(e1, e2) {
            ## this is the same error message that R gives for "A" | "A"
            stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanotime
##' @export
setMethod("Math", c("nanotime"),
          function(x) {
            ## this is the same error message that R gives for abs("A")
            stop("non-numeric argument to mathematical function")
          })

##' @rdname nanotime
##' @export
setMethod("Math2", c("nanotime"),
          function(x, digits) {
            ## this is the same error message that R gives for round("A")
            stop("non-numeric argument to mathematical function")
          })

##' @rdname nanotime
##' @export
setMethod("Summary", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
            ## this is the same error message that R gives for sum("A")
            stop("invalid 'type' (nanotime) of argument")
          })

##' @rdname nanotime
##' @export
setMethod("min", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
            new("nanotime", callNextMethod())
          })

##' @rdname nanotime
##' @export
setMethod("max", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
            new("nanotime", callNextMethod())
          })

##' @rdname nanotime
##' @export
setMethod("range", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
            new("nanotime", callNextMethod())
          })


##' @rdname nanotime
##' @export
setMethod("Complex", c("nanotime"),
          function(z) {
            ## this is the same error message that R gives for Arg("A")
            stop("non-numeric argument to function")
          })

## ----------- non ops

##' @rdname nanotime
##' @export
setMethod("[",
          signature("nanotime"),
          function (x, i, j, ..., drop=FALSE) {
            new("nanotime", callNextMethod())              
          })

##' @rdname nanotime
##' @export
setMethod("[<-",
          signature("nanotime"),
          function (x, i, j, ...) {
            new("nanotime", callNextMethod())              
          })

##' @rdname nanotime
##' @export
setMethod("c",
          signature("nanotime"),
          function (x, ..., recursive=FALSE) {
            ## this leaves names==c("x", ...), TODO: can we do better?
            new("nanotime", callNextMethod(S3Part(x, strictS3=TRUE), ..., recursive=FALSE))
            ## originally had the following, and for some reason it's
            ## about one order of magnitude slower with a corresponding
            ## increase in memory usage:
            ## new("nanotime", callNextMethod())
          })

## TODO: figure out if we need rbind/cbind... POSIXct doesn't provide them...

## can't use 'rbind2' because 'nanotime' is derived from 'integer64'
## and the S3 method will be called instead:
## TODO, check that the "masked" message this produces is harmless...
##' @rdname nanotime
setGeneric("rbind",
    function(..., deparse.level = 1) standardGeneric("rbind"),
    signature = "...")

##' @rdname nanotime
##' @export
setMethod("rbind",
          signature("nanotime"),
          function (x, ..., deparse.level = 1) {
            new("nanotime", rbind(S3Part(x, strictS3=TRUE), ..., deparse.level=deparse.level))
          })

##' @rdname nanotime
setGeneric("cbind",
    function(..., deparse.level = 1) standardGeneric("cbind"),
    signature = "...")

##' @rdname nanotime
##' @export
setMethod("cbind",
          signature("nanotime"),
          function (x, ..., deparse.level = 1) {
            new("nanotime", cbind(S3Part(x, strictS3=TRUE), ..., deparse.level=deparse.level))
          })

## -------- conversions TODO: figure out if we need conversions
## maybe we can do something for this:
                                
## > a <- as.character(1:10)
## > a
##  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
## > a[1] <- nanotime(1)
## > a
##  [1] "4.94065645841247e-324" "2"                     "3"                    
##  [4] "4"                     "5"                     "6"                    
##  [7] "7"                     "8"                     "9"                    
## [10] "10"                   



