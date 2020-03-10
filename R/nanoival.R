## TODO:
## - what do do when subsetting NA nanotime?
## - prevent matrix, array, cbind, rbind, compared with non-interval
## - see how we can better organize documentation for S4 methods that are
##   defined only to produce an error
## 



##' @rdname nanoival
setClass("nanoival", contains="complex")

##' Interval type with nanosecond precision
##'
##' \code{nanoival} is a time interval type (an S4 class) with
##' nanosecond precision. One of its purposes is to allow quick
##' subsetting of a \code{nanotime} vector. \code{nanoival} is
##' composed of a \code{nanotime} pair which defines the start and end
##' of the time interval. Additionally, it has a pair of logical
##' values which determine if the start and end of the time interval
##' are open (true) or closed (false).
##'
##' An interval object can be constructed with the constructor
##' \code{nanoival} which takes as arguments two \code{nanotime}
##' objects that define the start and the end of the interval,
##' together with two \code{logical} arguments that define if the
##' start and the end of the interval are open (true) or closed
##' (false) (note that these objects can all be vector, and therefore
##' the interval object is not necessarily scalar). Alternatively, an
##' interval can be constructed with a \code{character}: the format
##' follows that of \code{nanotime}; the start time is preceeded by
##' either \code{-} or \code{+} indicating if the interval start is
##' open (-) or closed (+); the start and end times are separated by
##' an arrow \code{->}; the end is folloed by either \code{-} or
##' \code{+} which have the same semantics as the start time.
##'
##' The most important set of methods defined for \code{interval} are
##' set functions \code{intersect}, \code{union} and \code{setdiff}.
##'
##' Additionally, \code{interval} allows the subsetting into a
##' \code{nanotime} vector. Note that subsetting is allowed only if
##' the \code{nanotime} vector is sorted.
##'
##' Finally, accessors are provided to get the interval start
##' (\code{start}), the end (\code{end}), the open/close status of the
##' start (\code{sopen}) and the open/close status of the end
##' (\code{eopen}). The former return a \code{nanotime} while the
##' latter return a \code{logical}.
##' 
##' @section Output Format:
##'
##' Formatting and character conversion for \code{nanoival} objects is
##' identical to \code{nanotime} objects. The default format is
##' ISO3339 compliant: \code{\%Y-\%m-\%dT\%H:\%M:\%E9S\%Ez}. It
##' specifies a standard ISO 8601 part for date and time --- as well
##' as nine digits of precision for fractional seconds (down to
##' nanoseconds) and on offset (typically zero as we default to UTC).
##' It can be overriden by using \code{options()} with the key of
##' \code{nanotimeFormat} and a suitable value. Similarly,
##' \code{nanotimeTz} can be used to select a different timezone.
##' 
##' @param x,from a \code{nanoival} object
##' @param tz \code{character} indicating a timezone
##' @param ... further arguments passed to or from methods.
##' @param e1 Operand of class \code{nanoival}
##' @param e2 Operand of class \code{nanoival}
##' @param format A character string. Can also be set via
##'     \code{options("nanotimeFormat")} and uses
##'     \sQuote{\%Y-\%m-\%dT\%H:\%M:\%E9S\%Ez} as a default and
##'     fallback
##' @param object argument for method \code{show}
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @param value argument for \code{nanoival-class}
##' @param start \code{nanotime} start of interval
##' @param end \code{nanotime} end of interval
##' @param sopen logical indicating if the start of the interval is open
##' @param eopen logical indicating if the end of the interval is open
##' @param quote indicates if the output of \code{print} should be
##'     quoted
##' @return A nanoival object
##' @author Dirk Eddelbuettel
##' @author Leonardo Silvestri
##' @examples
##' ## creating a \code{nanoival}, with the start time included ('+') and the end
##' ## time excluded ('-')
##' as.nanoival("+2012-03-01T21:21:00.000000001+00:00->2015-01-01T21:22:00.000000999+04:00-")
##'
##' ## a \code{nanoival} can also be created with a pair of \code{nanotime} objects, a start
##' ## and an end, and optionally two logicals determining if the interval start(end) are open
##' ## or closed; by default the start is closed and end is open:
##' start <- nanotime("2012-03-01T21:21:00.000000001+00:00")
##' end <- nanotime("2013-03-01T21:21:00.000000001+00:00")
##' nanoival(start, end)
##'
##' ## a vector of 'nanotime' can be subsetted by a 'nanoival':
##' one_second <- 1e9
##' a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
##' idx <- c(as.nanoival("-2012-12-12 12:12:10+00:00 -> 2012-12-12 12:12:14+00:00-"),
##'          as.nanoival("+2012-12-12 12:12:18+00:00 -> 2012-12-12 12:12:20+00:00+"))
##' a[idx]
##' @aliases +,ANY,nanoival-method             
##' @aliases +,nanoival,ANY-method             
##' @aliases +,nanoival,nanoival-method        
##' @aliases -,ANY,nanoival-method             
##' @aliases -,nanoival,ANY-method             
##' @aliases -,nanoival,nanoival-method        
##' @aliases Arith,nanoival,ANY-method         
##' @aliases Compare,nanoival,ANY-method       
##' @aliases Complex,nanoival-method           
##' @aliases Logic,ANY,nanoival-method         
##' @aliases Logic,nanoival,ANY-method         
##' @aliases Logic,nanoival,nanoival-method    
##' @aliases Math2,nanoival-method             
##' @aliases Math,nanoival-method              
##' @aliases Summary,nanoival-method           
##' 
##' @seealso \code{\link{intersect.idx}}, \code{\link{setdiff.idx}}, 
##' @rdname nanoival
nanoival <- function(start, end, sopen=FALSE, eopen=TRUE) {
    if (nargs() == 0) {
        new("nanoival", as.complex(NULL))
    } else {
        .Call("_nanoival_new",
              as.integer64(start),
              as.integer64(end),
              as.logical(sopen),
              as.logical(eopen))
    }
}

##' @noRd
setGeneric("nanoival.start", function(x) standardGeneric("nanoival.start"))

##' @rdname nanoival
##' @aliases nanoival.start
setMethod("nanoival.start",
          "nanoival",
          function(x) {
            res <- .Call("_nanoival_get_start", x)
            oldClass(res)  <- "integer64"
            new("nanotime", res)
          })

##' @noRd
setGeneric("nanoival.end", function(x) standardGeneric("nanoival.end"))

##' @rdname nanoival
##' @aliases nanoival.end
setMethod("nanoival.end",
          "nanoival",
          function(x) {
            res <- .Call("_nanoival_get_end", x)
            oldClass(res)  <- "integer64"
            new("nanotime", res)
          })

##' @noRd
setGeneric("nanoival.sopen", function(x) standardGeneric("nanoival.sopen"))

##' @rdname nanoival
##' @aliases nanoival.sopen
setMethod("nanoival.sopen",
          "nanoival",
          function(x) { .Call("_nanoival_get_sopen", x) })

##' @noRd
setGeneric("nanoival.eopen", function(x) standardGeneric("nanoival.eopen"))

##' @rdname nanoival
##' @aliases nanoival.eopen
setMethod("nanoival.eopen",
          "nanoival",
          function(x) { .Call("_nanoival_get_eopen", x) })

##' @rdname nanoival
format.nanoival <- 
  function(x, ...) {
    if (length(x) == 0) {
      "nanoival(0)"
    } else {
      s  <- paste0(ifelse(nanoival.sopen(x), "-", "+"),
                   format(nanoival.start(x), ...), " -> ",
                   format(nanoival.end(x), ...),
                   ifelse(nanoival.eopen(x), "-", "+"))
      if (!is.null(attr(x, "names", exact=TRUE))) {
        names(s) <- names(x)
      }
      s[.Call("_nanoival_isna", x)] = NA_character_
      s
    }
  }

##' @rdname nanoival
setMethod("print",
          "nanoival",
          function(x, quote=FALSE, ...) {
              ## like in nanotime, we must prevent the conversion to printout to be too large LLL
              s <- format(x, ...)
              print(s, quote=quote)
              invisible(s)
          })

##' @rdname nanoival
setMethod("show",
          signature("nanoival"),
          function(object) print(object))


## setMethod("format",
##           signature("nanoival"),
##           function(x, ..., justify = "none") {
##               ## like in nanotime, we must prevent the conversion to printout to be too large LLL
##               oldClass(x) <- "integer64"
##               j <- 1
##               s <- character(0)
##               while (j < length(x)) {
##                   s <- c(s, 
##                          paste0(ifelse(x[j+2] > 1, "-", "+"),
##                                 format(nanotime(x[j])), " -> ",
##                                 format(nanotime(x[j+1])),
##                                 ifelse(x[j+2]==1 | x[j+2]==4294967297, "-", "+")))
##                   j <- j + 3
##               }
##               if (!is.null(attr(x, "names", exact=TRUE))) {
##                   names(s) <- names(x)[c(TRUE, FALSE, FALSE)]
##               }
##               s
##           })


.secondaryNanoivalParse <- function(x, format="", tz="") {
  format <- .getFormat(format)
  tz <- .getTz(x, tz)
  ## parse the +-, split on -> and process the two
  s1 = substr(x, 1, 1)
  if (any(s1 != '-' & s1 != '+')) {
    stop("`nanoival` must start with '+' or '-'")
  }
  sopen <- s1 == "-"
  xlen <- nchar(x)
  e1 <- substr(x, xlen, xlen)
  if (any(e1 != '-' & e1 != '+')) {
    stop("`nanoival` must end with '+' or '-'")
  }
  eopen <- e1 == "-"
  start_stop <- strsplit(substr(x, 2, xlen-1), "->")
  start <- sapply(start_stop, function(x) head(x, 1))
  end <- sapply(start_stop, function(x) tail(x, 1))
  res <- nanoival(nanotime(start, format, tz),
                  nanotime(end,   format, tz),
                  sopen,
                  eopen)
  names(res) <- names(x)
  res
}


##' @noRd
setGeneric("as.nanoival", function(from, format="", tz="") standardGeneric("as.nanoival"))

##' @rdname nanoival
##' @aliases as.nanoival
setMethod("as.nanoival",
          "character",
          function(from, format="", tz="") {
            tryCatch(.Call("_nanoival_make", from, tz), error=function(e) {
              if (e$message == "Cannot retrieve timezone") {
                stop(e$message)
              } else {
                .secondaryNanoivalParse(from, format, tz)
              }
            }) 
          })

setAs("character", "nanoival", function(from) as.nanoival(from))


##' @rdname nanoival
setMethod("as.nanoival",
          "NULL",
          function(from, format="", tz="") {
              new("nanoival", as.complex(NULL))
          })

##' @rdname nanoival
setMethod("as.nanoival",
          "missing",
          function(from, format="", tz="") {
              new("nanoival", as.complex(NULL))
          })

##' @rdname nanoival
setMethod("is.na",
          "nanoival",
          function(x) {
              .Call("_nanoival_isna", x)
          })


##' @rdname nanoival
setMethod("is.na<-",
          "nanoival",
          function(x, value) {
              x[value] <- NA_nanoival_
              x
          })

## ------------ logical comp

##' @rdname nanoival
setMethod("<", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_lt', e1, e2)
          })

##' @rdname nanoival
setMethod("<=", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_le', e1, e2)
          })

##' @rdname nanoival
setMethod(">", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_gt', e1, e2)
          })

##' @rdname nanoival
setMethod(">=", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_ge', e1, e2)
          })

##' @rdname nanoival
setMethod("==", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_eq', e1, e2)
          })

##' @rdname nanoival
setMethod("!=", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_ne', e1, e2)
          })


## ------------ `-`
##' @noRd
setMethod("-", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })


##' @rdname nanoival
setMethod("-", c("nanoival", "integer64"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_minus", e1, e2))
          })

##' @rdname nanoival
setMethod("-", c("nanoival", "numeric"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_minus", e1, as.integer64(e2)))
          })

##' @noRd
setMethod("-", c("ANY", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @noRd
setMethod("-", c("nanoival", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })


## ----------- `+`
##' @noRd
setMethod("+", c("nanoival", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @noRd
setMethod("+", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoival
setMethod("+", c("nanoival", "integer64"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e1, e2))
          })

##' @rdname nanoival
setMethod("+", c("nanoival", "numeric"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e1, as.integer64(e2)))
          })

##' @noRd
setMethod("+", c("ANY", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoival
setMethod("+", c("integer64", "nanoival"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e2, e1))
          })

##' @rdname nanoival
setMethod("+", c("numeric", "nanoival"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e2, as.integer64(e1)))
          })

##' @noRd
setMethod("+", c("nanoival", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })


## ---------- other ops

##' @noRd
setMethod("Arith", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"nanoival\" objects")           
          })

##' @noRd
setMethod("Compare", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })


##' @noRd
setMethod("Logic", c("nanoival", "nanoival"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @noRd
setMethod("Logic", c("nanoival", "ANY"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @noRd
setMethod("Logic", c("ANY", "nanoival"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @noRd
setMethod("Math", c("nanoival"),
          function(x) {
              ## this is the same error message that R gives for abs("A")
              stop("non-numeric argument to mathematical function")
          })

##' @noRd
setMethod("Math2", c("nanoival"),
          function(x, digits) {
              ## this is the same error message that R gives for round("A")
              stop("non-numeric argument to mathematical function")
          })

##' @noRd
setMethod("Summary", c("nanoival"),
          function(x, ..., na.rm = FALSE) {
              ## this is the same error message that R gives for sum("A")
              stop("invalid 'type' (nanoival) of argument")
          })

##' @noRd
setMethod("Complex", c("nanoival"),
          function(z) {
              ## this is the same error message that R gives for Arg("A")
              stop("non-numeric argument to function")
          })

## ----------- non ops

##' @rdname nanoival
setMethod("[[",
          signature("nanoival"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoival", callNextMethod())
          })

##' @rdname nanoival
setMethod("[",
          signature("nanoival", "logical"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("nanoival", x[i])
          })

##' @rdname nanoival
setMethod("[",
          signature("nanoival", "numeric"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("nanoival", x[i])
          })

##' @rdname nanoival
setMethod("[<-",
          signature("nanoival", "logical", "ANY", "nanoival"),
          function (x, i, j, ..., value) {
              x <- S3Part(x, strictS3=TRUE)
              x[i] <- S3Part(value, strictS3=TRUE)
              new("nanoival", x)
          })

##' @rdname nanoival
c.nanoival <- function(...) {
    args <- list(...)
    s3args <- lapply(args, function (x) S3Part(x, strictS3=TRUE))
    res <- do.call(c, s3args)
    names(res) <- names(args)
    new("nanoival", res)
}

## S4 'c' doesn't handle names correctly, and couldn't find a way to rectify that:

## ##' @rdname nanoival
## ## setMethod("c",
##           signature("nanoival"),
##           function(x, ...) {
##               print("method ccc")
##               args <- list(...)
##               res <- callNextMethod()
##               names(res) <- names(args)
##               print(names(args))
##               new("nanoival", res)
##           })

##' @rdname nanoival
setMethod("t", c("nanoival"),
          function(x) {
              ## identity, like POSIXct, because nanoival doesn't support arrays
              x
          })


## ##' @rdname nanoival
## ## setMethod("cbind2",
##           signature("nanoival", "nanoival"),
##           function (x, y, ...) {
##               print(dimnames(x))
##               print("calling next method")
##               res <- callNextMethod()
##               print(attributes(res))
##               new("nanoival", res)              
##           })

## ##' @rdname nanoival
## ## setMethod("cbind2",
##           signature("nanoival", "nanoival"),
##           function (x, y, ...) {
##               x <- t(x)
##               y <- t(y)
##               print(dimnames(x))
##               print("calling next method")
##               res <- callNextMethod()
##               print(attributes(res))
##               new("nanoival", res)              
##           })


## ##' @rdname nanoival
## ## setMethod("rbind2",
##           signature("nanoival", "nanoival"),
##           function (x, y, ...) {
##               print("rbind2")
##               dim(x)
##               x <- t(x)
##               y <- t(y)
##               print(dim(x))
##               print(dim(y))
##               print("rbind2 dimnames")
##               print(dimnames(x))
##               new("nanoival", callNextMethod())              
##           })

## set functions
## -------------

##' Set operations
##'
##' Performs set intersection, union and difference between vectors of
##' temporal types from the \code{nanotime} package.
##'
##' @param x,y a temporal type
##' @return \code{intersect}, \code{union}, \code{setdiff} return
##'     temporal types that are the result of the intersection. For
##'     instance, set operations on two \code{nanoival} return a
##'     \code{nanoival}, whereas intersection between a
##'     \code{nanoival} and a \code{nanotime} returns a
##'     \code{nanotime}. \code{intersect.idx} return a list of vectors
##'     representing the element indices that intersect and
##'     \code{setdiff.idx} returns a vector representing the element
##'     indices to be removed.
##' @examples
##' ## a vector of 'nanotime' can be subsetted by a 'nanoival' which is equivalent to 'intersect':
##' one_second <- 1e9
##' a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
##' idx <- c(as.nanoival("-2012-12-12 12:12:10+00:00 -> 2012-12-12 12:12:14+00:00-"),
##'          as.nanoival("+2012-12-12 12:12:18+00:00 -> 2012-12-12 12:12:20+00:00+"))
##' a[idx]
##' intersect(a, idx)
##'
##' ## 'nanoival' also has the set operations 'union', 'intersect', 'setdiff':
##' a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
##' i <- as.nanoival("-2012-12-12 12:12:14+00:00 -> 2012-12-12 12:12:18+00:00-")
##' setdiff(a, i)
##'
##' i1 <- as.nanoival("+2012-12-12 12:12:14+00:00 -> 2012-12-12 12:12:17+00:00-")
##' i2 <- as.nanoival("+2012-12-12 12:12:16+00:00 -> 2012-12-12 12:12:18+00:00-")
##' union(i1, i2)
##'
##' ## Finally, 'intersect.idx' returns the indices of the intersection:
##' a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
##' idx <- as.nanoival("+2012-12-12 12:12:14+00:00 -> 2012-12-12 12:12:19+00:00+")
##' idx_intersect <- intersect.idx(a, idx)
##'
##' ## Intersection can be performed using these indices:
##' a[idx_intersect$x]
##'
##' ## which is equivalent to:
##' a[idx]
##' 
##' @rdname set_operations
##' 
setMethod("intersect",
          c("nanoival", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_intersect', x, y)
              new("nanoival", res)
          })

##' @rdname set_operations
setMethod("union",
          c("nanoival", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_union', x, y)
              new("nanoival", res)
          })

##' @rdname set_operations
setMethod("setdiff",
          c("nanoival", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_setdiff', x, y)
              new("nanoival", res)
          })


## nanotime and nanoival:
## ---------------------

##' @rdname nanoival
setMethod("[",
          signature("nanotime", "nanoival"),
          function (x, i, ..., drop=FALSE) {
              if (is.unsorted(x)) stop("x must be sorted")
              i <- sort(i)
              .Call('_nanoival_intersect_time_interval', x, i)
          })


##' @noRd
setGeneric("intersect.idx", function(x, y) standardGeneric("intersect.idx"))


## need to add nanotime/nanotime LLL

##' @rdname set_operations
##' @aliases intersect.idx
setMethod("intersect.idx",
          c("nanotime", "nanoival"),
          function(x, y) {
              if (is.unsorted(x)) stop("x must be sorted")
              y <- sort(y)
              .Call('_nanoival_intersect_idx_time_interval', x, y)
          })


##' @rdname set_operations
setMethod("intersect",
          c("nanotime", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              .Call('_nanoival_intersect_time_interval', x, y)
          })

##' @rdname set_operations
setMethod("setdiff",
          c("nanotime", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_setdiff_time_interval', x, y)
              oldClass(res) <- "integer64"
              new("nanotime", res)
          })

##' @noRd
setGeneric("setdiff.idx", function(x, y) standardGeneric("setdiff.idx"))

##' @rdname set_operations
##' @aliases setdiff.idx
setMethod("setdiff.idx",
          c("nanotime", "nanoival"),
          function(x, y) {
              if (is.unsorted(x)) stop("x must be sorted")
              y <- sort(y)
              .Call('_nanoival_setdiff_idx_time_interval', x, y)
          })


## misc functions:
## --------------

##' Test if a \code{nanoival} vector is Not Sorted
##'
##' Test if an object is not sorted (in increasing order), without the
##' cost of sorting it.
##'
##' @param x a \code{nanoival} vector
##' @param na.rm logical.  Should missing values be removed before
##'     checking?
##' @param strictly logical indicating if the check should be for
##'     _strictly_ increasing values.
##'
##' @seealso \code{\link{sort}}
##' 
setMethod("is.unsorted", "nanoival",
          function(x, na.rm=FALSE, strictly=FALSE) {
              if (typeof(strictly) != "logical") {
                  stop("argument 'strictly' must be a logical")
              }
              .Call('_nanoival_is_unsorted', x, strictly)
          })


##' Sorting or Ordering Vectors
##'
##' Sort (or _order_) a vector of \code{nanoival} into ascending or
##' descending order
##'
##' @param x a vector of \code{nanoival}
##' @param decreasing logical.  Should the sort be increasing or
##'     decreasing?
##' @seealso \code{\link{is.unsorted}}
##' 
setMethod("sort", c("nanoival"),
          function(x, decreasing=FALSE)
            new("nanoival", .Call('_nanoival_sort', x, decreasing)))


##' Sequence Generation
##'
##' Generate a sequence of \code{nanoival}
##'
##' @param ... arguments passed to or from methods; the only
##'     interesting additional argument is \code{tz} where the
##'     \code{to} argument is of type \code{nanoperiod}
##' @param from,to the starting and (maximal) end values of the
##'     sequence
##' @param by \code{nanoduration} or \code{nanoperiod} increment of
##'     the sequence; note that if the class is \code{nanoperiod} the
##'     additional argument \code{tz} must be speficied and is of
##'     \code{character} type indicating a timezone
##' @param length.out an integer desired length of the sequence
##' @param along.with take the length from the length of this argument.
##' @examples
##' from <- as.nanoival("-2018-01-14T13:00:00+00:00 -> 2018-01-14T15:00:00+00:00+")
##' seq(from, by=as.nanoperiod("1m"), length.out=5, tz="America/New_York")
setMethod("seq", c("nanoival"),
          function(from, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)
          {
              if (is.null(to)) {
                  start <- seq(nanoival.start(from), by=by, length.out=length.out, along.with=along.with, ...)
              } else {
                  start  <- seq(nanoival.start(from), nanoival.start(to), by, length.out, along.with, ...)
              }
              if (is.null(by)) {
                  end <- seq(nanoival.end(from), by=start[2]-start[1], length.out=length(start), ...)
              } else {
                  end <- seq(nanoival.end(from), by=by, length.out=length(start), ...)
              }
              nanoival(start, end, nanoival.sopen(from), nanoival.eopen(from))
          })


##' @rdname nanoival
NA_nanoival_ <- new("nanoival", complex(1, -4.9406564584124654418e-324, -4.9406564584124654418e-324))
