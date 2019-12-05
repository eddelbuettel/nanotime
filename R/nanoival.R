## TODO:
## - what do do when subsetting NA nanotime?
## - prevent matrix, array, cbind, rbind, compared with non-interval
## - see how we can better organize documentation for S4 methods that are
##   defined only to produce an error
## 



##' @rdname nanoival
##' @export
setClass("nanoival", contains="complex")

##' Interval type with nanosecond precision
##'
##' \code{nanoival} is a time interval type (an S4 class) with
##' nanosecond functionality. One of its purposes is to allow quick
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
##' @param x a \code{nanoival} object
##' @param tz a timezone string
##' @param ... further arguments passed to or from methods.
##' @param e1 Operand of class \code{nanoival}
##' @param e2 Operand of class \code{nanoival}
##' @param digits Required for \code{Math2} signature but ignored here
##' @param recursive argument for method \code{c}
##' @param object argument for method \code{show}
##' @param na.rm a logical indicating whether missing values should be removed.
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @param z Required for \code{Complex} signature but ignored here
##' @param value argument for \code{nanoival-class}
##' @param start \code{nanotime} start of interval
##' @param end \code{nanotime} end of interval
##' @param sopen logical indicating if the start of the interval is open
##' @param eopen logical indicating if the end of the interval is open
##' @return A nanoival object
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
##' ## a vector of \code{nanotime} can be subsetted by an interval:
##' fmt <- "%Y-%m-%d %H:%M:%S"
##' one_second <- 1e9
##' a <- seq(nanotime("2012-12-12 12:12:12", fmt), length.out=10, by=one_second)
##' idx <- c(as.nanoival("-2012-12-12 12:12:10 -> 2012-12-12 12:12:14-", fmt),
##'          as.nanoival("+2012-12-12 12:12:18 -> 2012-12-12 12:12:20+", fmt))
##' a[idx]
##'
##' ## \code{nanoival} also has the set operations \code{union}, \code{intersect},
##' ## \code{setdiff}
##' a <- seq(nanotime("2012-12-12 12:12:12", fmt), length.out=10, by=one_second)
##' i <- as.nanoival("-2012-12-12 12:12:14 -> 2012-12-12 12:12:18-", fmt)
##' setdiff(a, i)
##'
##' i1 <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:17-", fmt)
##' i2 <- as.nanoival("+2012-12-12 12:12:16 -> 2012-12-12 12:12:18-", fmt)
##' union(i1, i2)
##'
##' ## Finally, \code{intersect.idx} which gives back the indices of the intersection is
##' ## defined:
##' a <- seq(nanotime("2012-12-12 12:12:12", fmt), length.out=10, by=one_second)      
##' idx <- as.nanoival("+2012-12-12 12:12:14 -> 2012-12-12 12:12:19+", fmt)
##' intersect.idx(a, idx)                                                        
##' ## which gives back:                                                               
##' ## $x                                                                        
##' ## [1] 3 4 5 6 7 8                                                           
##' ##                                                                           
##' ## $y                                                                        
##' ## [1] 1 1 1 1 1 1                                                           
##'                                                                              

##' @rdname nanoival
##' @export
nanoival <- function(start, end, sopen=FALSE, eopen=TRUE) {
  .Call("_nanoival_new",
        as.integer64(start),
        as.integer64(end),
        as.logical(sopen),
        as.logical(eopen))
}

setGeneric("nanoival.start", function(x) standardGeneric("nanoival.start"))
##' @rdname nanoival
##' @export
setMethod("nanoival.start",
          "nanoival",
          function(x) {
            res <- .Call("_nanoival_get_start", x)
            oldClass(res)  <- "integer64"
            new("nanotime", res)
          })

setGeneric("nanoival.end", function(x) standardGeneric("nanoival.end"))
##' @rdname nanoival
##' @export
setMethod("nanoival.end",
          "nanoival",
          function(x) {
            res <- .Call("_nanoival_get_end", x)
            oldClass(res)  <- "integer64"
            new("nanotime", res)
          })

setGeneric("nanoival.sopen", function(x) standardGeneric("nanoival.sopen"))
##' @rdname nanoival
##' @export
setMethod("nanoival.sopen",
          "nanoival",
          function(x) { .Call("_nanoival_get_sopen", x) })

setGeneric("nanoival.eopen", function(x) standardGeneric("nanoival.eopen"))
##' @rdname nanoival
##' @export
setMethod("nanoival.eopen",
          "nanoival",
          function(x) { .Call("_nanoival_get_eopen", x) })

##' @rdname nanoival
##' @export
format.nanoival <- 
  function(x, ..., justify = "none") {
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
##' @export
setMethod("print",
          "nanoival",
          function(x, ...) {
            ## like in nanotime, we must prevent the conversion to printout to be too large LLL
            s <- format(x, ...)
            print(s)
            invisible(s)
          })

##' @rdname nanoival
##' @export
setMethod("show",
          signature("nanoival"),
          function(object) print(object))

##' @rdname nanoival
##' @export
setMethod("names",
          signature("nanoival"),
          function(x) {
              callNextMethod()
          })

##' @rdname nanoival
##' @export
setMethod("length",
          signature("nanoival"),
          function(x) { callNextMethod() })

## ##' @rdname nanoival
## ##' @export
## setMethod("as.data.frame.nanoival",
##           signature("nanoival"),
##           function(x, row.names=NULL, optional=FALSE, ...)
##           {
##               if (is.null(row.names))
##                   row.names <- seq_len(length(x))
##               value <- list(x)
##               attr(value, "names") <- "1"
##               attr(value, "row.names") <- row.names
##               class(value) <- "data.frame"
##               value
##           })


## ##' @rdname nanoival
## ##' @export
## as.data.frame.nanoival <- 
##           function(x, row.names=NULL, optional=FALSE, ...)
##           {
##               if (is.null(row.names))
##                   row.names <- seq_len(length(x))
##               value <- list(x)
##               attr(value, "names") <- "1"
##               attr(value, "row.names") <- row.names
##               class(value) <- "data.frame"
##               value
##           }

##' @rdname nanoival
##' @export
as.data.frame.nanoival <- function(x, row.names=NULL, optional=FALSE, ...)
{
  NextMethod()
}


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

##' @rdname nanoival
##' @export
setMethod("names<-",
          signature("nanoival"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- value
              new("nanoival", x)
          })

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


setGeneric("as.nanoival", function(x, format="", tz="") standardGeneric("as.nanoival"))
##' @rdname nanoival
##' @export
setMethod("as.nanoival",
          "character",
          function(x, format="", tz="") {
            tryCatch(.Call("_nanoival_make", x, tz), error=function(e) {
              if (e$message == "Cannot retrieve timezone") {
                stop(e$message)
              } else {
                .secondaryNanoivalParse(x, format, tz)
              }
            }) 
          })

##' @rdname nanoival
##' @export
setMethod("as.nanoival",
          "NULL",
          function(x, format="", tz="") {
              new("nanoival", as.complex(NULL))
          })

##' @rdname nanoival
##' @export
setMethod("is.na",
          "nanoival",
          function(x) {
              .Call("_nanoival_isna", x)
          })


##' @rdname nanoival
##' @export
setMethod("is.na<-",
          "nanoival",
          function(x, value) {
              x[value] <- NA_nanoival_
              x
          })

## ------------ logical comp

##' @rdname nanoival
##' @export
setMethod("<", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_lt', e1, e2)
          })

##' @rdname nanoival
##' @export
setMethod("<=", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_le', e1, e2)
          })

##' @rdname nanoival
##' @export
setMethod(">", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_gt', e1, e2)
          })

##' @rdname nanoival
##' @export
setMethod(">=", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_ge', e1, e2)
          })

##' @rdname nanoival
##' @export
setMethod("==", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_eq', e1, e2)
          })

##' @rdname nanoival
##' @export
setMethod("!=", c("nanoival", "nanoival"),
          function(e1, e2) {
              .Call('_nanoival_ne', e1, e2)
          })


## ------------ `-`
##' @rdname nanoival
##' @export
setMethod("-", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })


##' @rdname nanoival
##' @export
setMethod("-", c("nanoival", "integer64"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_minus", e1, e2))
          })

##' @rdname nanoival
##' @export
setMethod("-", c("nanoival", "numeric"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_minus", e1, as.integer64(e2)))
          })

##' @rdname nanoival
##' @export
setMethod("-", c("ANY", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoival
##' @export
setMethod("-", c("nanoival", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })


## ----------- `+`
##' @rdname nanoival
##' @export
setMethod("+", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoival
##' @export
setMethod("+", c("nanoival", "integer64"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e1, e2))
          })

##' @rdname nanoival
##' @export
setMethod("+", c("nanoival", "numeric"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e1, as.integer64(e2)))
          })

##' @rdname nanoival
##' @export
setMethod("+", c("ANY", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoival
##' @export
setMethod("+", c("integer64", "nanoival"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e2, e1))
          })

##' @rdname nanoival
##' @export
setMethod("+", c("numeric", "nanoival"),
          function(e1, e2) {
              new("nanoival", .Call("_nanoival_plus", e2, as.integer64(e1)))
          })

##' @rdname nanoival
##' @export
setMethod("+", c("nanoival", "nanoival"),
          function(e1, e2) {
              stop("invalid operand types")
          })


## ---------- other ops

##' @rdname nanoival
##' @export
setMethod("Arith", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"nanoival\" objects")           
          })

##' @rdname nanoival
##' @export
setMethod("Compare", c("nanoival", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })


##' @rdname nanoival
##' @export
setMethod("Logic", c("nanoival", "ANY"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanoival
##' @export
setMethod("Logic", c("ANY", "nanoival"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanoival
##' @export
setMethod("Math", c("nanoival"),
          function(x) {
              ## this is the same error message that R gives for abs("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname nanoival
##' @export
setMethod("Math2", c("nanoival"),
          function(x, digits) {
              ## this is the same error message that R gives for round("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname nanoival
##' @export
setMethod("Summary", c("nanoival"),
          function(x, ..., na.rm = FALSE) {
              ## this is the same error message that R gives for sum("A")
              stop("invalid 'type' (nanoival) of argument")
          })

##' @rdname nanoival
##' @export
setMethod("Complex", c("nanoival"),
          function(z) {
              ## this is the same error message that R gives for Arg("A")
              stop("non-numeric argument to function")
          })

## ----------- non ops

##' @rdname nanoival
##' @export
setMethod("[[",
          signature("nanoival"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoival", callNextMethod())
          })

##' @rdname nanoival
##' @export
setMethod("[",
          signature("nanoival", "logical"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("nanoival", x[i])
          })

##' @rdname nanoival
##' @export
setMethod("[",
          signature("nanoival", "numeric"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("nanoival", x[i])
          })

##' @rdname nanoival
##' @export
setMethod("[<-",
          signature("nanoival", "logical", "ANY", "nanoival"),
          function (x, i, j, ..., value) {
              x <- S3Part(x, strictS3=TRUE)
              x[i] <- S3Part(value, strictS3=TRUE)
              new("nanoival", x)
          })

##' @rdname nanoival
##' @export
c.nanoival <- function(...) {
    args <- list(...)
    s3args <- lapply(args, function (x) S3Part(x, strictS3=TRUE))
    res <- do.call(c, s3args)
    names(res) <- names(args)
    new("nanoival", res)
}

## S4 'c' doesn't handle names correctly, and couldn't find a way to rectify that:

## ##' @rdname nanoival
## ##' @export
## setMethod("c",
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
##' @export
setMethod("t", c("nanoival"),
          function(x) {
              ## identity, like POSIXct, because nanoival doesn't support arrays
              x
          })


## ##' @rdname nanoival
## ##' @export
## setMethod("cbind2",
##           signature("nanoival", "nanoival"),
##           function (x, y, ...) {
##               print(dimnames(x))
##               print("calling next method")
##               res <- callNextMethod()
##               print(attributes(res))
##               new("nanoival", res)              
##           })

## ##' @rdname nanoival
## ##' @export
## setMethod("cbind2",
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
## ##' @export
## setMethod("rbind2",
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

##' @rdname nanoival
##' @export
setMethod("intersect",
          c("nanoival", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_intersect', x, y)
              new("nanoival", res)
          })

##' @rdname nanoival
##' @export
setMethod("union",
          c("nanoival", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_union', x, y)
              new("nanoival", res)
          })

##' @rdname nanoival
##' @export
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
##' @export
setMethod("[",
          signature("nanotime", "nanoival"),
          function (x, i, ..., drop=FALSE) {
              if (is.unsorted(x)) stop("x must be sorted")
              i <- sort(i)
              res <- .Call('_nanoival_intersect_time_interval', x, i)
              oldClass(res) <- "integer64"
              new("nanotime", res)            
          })


setGeneric("intersect.idx", function(x, y) standardGeneric("intersect.idx"))


## need to add nanotime/nanotime LLL

##' @rdname nanoival
##' @export
setMethod("intersect.idx",
          c("nanotime", "nanoival"),
          function(x, y) {
              if (is.unsorted(x)) stop("x must be sorted")
              y <- sort(y)
              .Call('_nanoival_intersect_idx_time_interval', x, y)
          })


##' @rdname nanoival
##' @export
setMethod("intersect",
          c("nanotime", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_intersect_time_interval', x, y)
              oldClass(res) <- "integer64"
              new("nanotime", res)
          })

##' @rdname nanoival
##' @export
setMethod("setdiff",
          c("nanotime", "nanoival"),
          function(x, y) {
              x <- sort(x)
              y <- sort(y)
              res <- .Call('_nanoival_setdiff_time_interval', x, y)
              oldClass(res) <- "integer64"
              new("nanotime", res)
          })

setGeneric("setdiff.idx", function(x, y) standardGeneric("setdiff.idx"))

##' @rdname nanoival
##' @export
setMethod("setdiff.idx",
          c("nanotime", "nanoival"),
          function(x, y) {
              if (is.unsorted(x)) stop("x must be sorted")
              y <- sort(y)
              .Call('_nanoival_setdiff_idx_time_interval', x, y)
          })


## misc functions:
## --------------

##' @rdname nanoival
##' @export
setMethod("is.unsorted", "nanoival",
          function(x, na.rm=FALSE, strictly=FALSE) {
              if (typeof(strictly) != "logical") {
                  stop("argument 'strictly' must be a logical")
              }
              .Call('_nanoival_is_unsorted', x, strictly)
          })

##' @rdname nanoival
##' @export
setMethod("sort", c("nanoival"),
          function(x, decreasing=FALSE, ...)
            new("nanoival", .Call('_nanoival_sort', x, decreasing)))


NA_nanoival_ <- new("nanoival", complex(1, -4.9406564584124654418e-324, -4.9406564584124654418e-324))
