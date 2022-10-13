##' @rdname nanotime
setClass("nanotime", contains = "integer64")
## setClass("nanotime", contains = "numeric") also possible if we don't want integer64

##' Nanosecond resolution datetime functionality
##'
##' Functions to operate on nanosecond time resolution using integer64
##' bit representation. Conversion functions for several standard R
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
##' Working with dates and times is \emph{difficult}. One needs a
##' representation of both \emph{time points} and \emph{time
##' duration}. In R, think of \code{Date} or \code{POSIXct} objects
##' for the former, and \code{difftime} for the later. Here we have
##' time points \code{nanotime}, an interval type \code{nanoival} and
##' two flavors of duration which are a simple count of nanoseconds
##' \code{nanoduration} and a calendar duration that is able to track
##' concepts such as months and days \code{nanoperiod}. Point in time
##' and intervals are all based on durations relative to the epoch of
##' January 1, 1970.
##'
##' @section Input and Output Format:
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
##' For input, some slack it cut, and various shortened formats are
##' accepted by default such as \code{2020-03-10} or \code{2020-03-10
##' 18:16:00}, or \code{2020-03-10 18:16:00.001} (and the \sQuote{T}
##' separator is optional.
##'
##' @section \code{tz} parameter usage in constructors:
##'
##' The \code{tz} parameter is allowed only when constructing a
##' \code{nanotime} from a \code{character}. This is because any
##' \code{numeric}, \code{Date} and \code{POSIXct} is de facto
##' considered an offset since the epoch. On the contrary, a
##' \code{character} is considered interpretable and hence if it does
##' not contain a timezone in its representation, it is possible to
##' specify the \code{tz} argument to specify in which timezone it
##' should be interpreted. This is useful in particular if one wants
##' to convert a \code{Date} to be aligned to the beginning of the day
##' in a specific timezone; in this case one should convert the
##' \code{Date} to a \code{character} before calling the
##' \code{nanotime} constructor with the desired timezone.
##' 
##' @param x,from \code{nanotime} objects
##' @param tz character specifying a timezone which is required for
##'     \code{as.POSIXct}, \code{as.POSIXlt} and can be specified for
##'     \code{as.nanotime}, \code{format} and \code{print}; it can
##'     also be set via \code{options("nanotimeTz")} and uses
##'     \sQuote{UTC} as a default and fallback
##' @param ... further arguments passed to or from methods.
##' @param e1 Operand of class \code{nanotime}
##' @param e2 Operand of class \code{nanotime}
##' @param format A character string. Can also be set via
##'     \code{options("nanotimeFormat")} and uses
##'     \sQuote{\%Y-\%m-\%dT\%H:\%M:\%E9S\%Ez} as a default and
##'     fallback
##' @param digits Required for \code{Math2} signature but ignored here
##' @param object argument for method \code{show}
##' @param na.rm a logical indicating whether missing values should be
##'     removed.
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @param z Required for \code{Complex} signature but ignored here
##' @param value argument for \code{nanotime-class}
##' @param quote indicates if the output of \code{print} should be
##'     quoted
##' @return A nanotime object
##' @author Dirk Eddelbuettel
##' @author Leonardo Silvestri
##' @examples
##' \dontrun{
##' x <- nanotime(1)
##' print(x)
##' as.nanotime("1970-01-01T00:00:00.000000001+00:00")
##' as.nanotime("2020-03-10 Europe/Berlin")
##' as.nanotime("2020-03-10 18:31:23.001", tz="America/New_York")
##' x <- x + 1
##' print(x)
##' format(x)
##' x <- x + 10
##' print(x)
##' format(x)
##' nanotime(Sys.time()) + 1:3  # three elements each 1 ns apart
##' seq(x, by=as.nanoperiod("1d"), length.out=5, tz="Asia/Tokyo")
##' }
##' @seealso \code{\link{nanoival}}, \code{\link{nanoduration}},
##'     \code{\link{nanoperiod}}, \code{\link{seq.nanotime}}

nanotime <- function(from, ...) {
    if (missing(from)) {
        from = NULL
    }
    new("nanotime", as.integer64(from, keep.names=TRUE))
}
##' @noRd
setGeneric("nanotime")

##' @rdname nanotime
as.nanotime <- function(from, ...) {
    if (missing(from)) {
        from = NULL
    }
    new("nanotime", as.integer64(from, keep.names=TRUE))
}
##' @noRd
setGeneric("as.nanotime")

setAs("integer64", "nanotime", function(from) new("nanotime", as.integer64(from, keep.names=TRUE)))


.getTz <- function(x, tz="") {
    if (tz=="") {
        if (!is.null(tzone <- attr(x, "tzone")))
            tz <- tzone
        else
            tz <- getOption("nanotimeTz", default="UTC")
    }
    tz
}

.getFormat <- function(format="") {
    if (format=="") {
        format <- getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%EXS%Ez")
    }
    format
}


.secondaryNanotimeParse <- function(x, format="", tz="") {
    if (length(x) == 0) return(character(0)) # nocov
    format <- gsub("%EXS", "%E9S", .getFormat(format))
    tz <- .getTz(x, tz)
    n <- names(x)
    d <- RcppCCTZ::parseDouble(x, fmt=format, tzstr=tz)
    res <- new("nanotime", as.integer64(d[,1]) * as.integer64(1e9) + as.integer64(d[, 2]))
    if (!is.null(n)) {
        names(S3Part(res, strictS3=TRUE)) <- n
    }
    res
}


.nanotime_character <- function(from, format="", tz="") {
    tryCatch(nanotime_make_impl(from, tz), error=function(e) {
        if (grepl("Cannot retrieve timezone", e$message) ||
            e$message == "timezone is specified twice: in the string and as an argument") {
            stop(e$message)
        } else {
            .secondaryNanotimeParse(from, format, tz)
        }
    })
}

##' @rdname nanotime
setMethod("nanotime", "character", .nanotime_character)

##' @rdname nanotime
setMethod("as.nanotime", "character", .nanotime_character)

setAs("character", "nanotime", function(from) .nanotime_character(from))

##' @rdname nanotime
## This does not lead to S3 dispatch, the call must be 'nanotime.matrix'
nanotime.matrix <- function(x) {
    n <- names(x)
    res <- new("nanotime", as.integer64(x[,1]) * as.integer64(1e9) + as.integer64(x[, 2]))
    if (!is.null(n)) {
        names(res) <- n 						## #nocov
    }
    res
}


.nanotime_posixct <- function(from) {
    ## force last three digits to be zero
    n <- names(from)
    res <- new("nanotime", as.integer64(as.numeric(from) * 1e6) * 1000)
    if (!is.null(n)) {
        names(S3Part(res, strictS3=TRUE)) <- n
    }
    res
}

##' @rdname nanotime
setMethod("nanotime", signature(from="POSIXct"), .nanotime_posixct)

##' @rdname nanotime
setMethod("as.nanotime", signature(from="POSIXct"), .nanotime_posixct)

setAs("POSIXct", "nanotime", .nanotime_posixct)


##' @rdname nanotime
setMethod("nanotime", "POSIXlt", function(from) nanotime(as.POSIXct(from)))

##' @rdname nanotime
setMethod("as.nanotime", "POSIXlt", function(from) nanotime(as.POSIXct(from)))

setAs("POSIXlt", "nanotime", function(from) nanotime(as.POSIXct(from)))


##' @rdname nanotime
setMethod("nanotime", "Date", function(from) nanotime(as.POSIXct(from)))

##' @rdname nanotime
setMethod("as.nanotime", "Date", function(from) nanotime(as.POSIXct(from)))

setAs("Date", "nanotime", function(from) nanotime(as.POSIXct(from)))


##' @rdname nanotime
setMethod("print",
          "nanotime",
          function(x, format="", tz="", quote=FALSE, ...) {
              format <- .getFormat(format)
              tz <- .getTz(x, tz)
              max.print <- options()$max.print
              if (length(x) > max.print) {                   		## #nocov start
                  f <- head(x, max.print)
                  print(format(f, format, tz, ...), quote=quote)
                  cat(paste(' [ reached getOption("max.print") -- omitted',
                            length(x) - max.print, "entries ]\n"))
              }                                              		## #nocov end
              else {
                  print(format(x, format, tz, ...), quote=quote)
              }
              invisible(x)
          })

##' @rdname nanotime
setMethod("show",
          signature("nanotime"),
          function(object) print(object))  				## #nocov

##' @rdname nanotime
format.nanotime <- function(x, format="", tz="", ...)
{
    if (length(x) == 0) {
        "nanotime(0)"
    } else {
        format <- .getFormat(format)
        tz <- .getTz(x, tz)
        if (length(x) == 0) {
            return(character(0))						## #nocov
        }
        bigint <- as.integer64(x)
        secs  <- as.integer64(bigint / as.integer64(1000000000))
        nanos <- bigint - secs * as.integer64(1000000000)

        nanos_notna <- nanos[!is.na(nanos)]

        ## EXS has special meaning for us: print with the least number of nanotime digits
        if (length(nanos_notna) != 0 && isTRUE(as.logical(grep("%EXS", format)))) {
            if (all(nanos_notna %% 1000000000 == 0)) {
                format <- gsub("%EXS", "%S", format)
            } else if (all(nanos_notna %% 1000000 == 0)) {
                format <- gsub("%EXS", "%E3S", format)
            } else if (all(nanos_notna %% 1000 == 0)) {
                format <- gsub("%EXS", "%E6S", format)
            } else {
                format <- gsub("%EXS", "%E9S", format)
            }
        }

        res <- RcppCCTZ::formatDouble(as.double(secs), as.double(nanos), fmt=format, tgttzstr=tz)
        res[is.na(x)] <- as.character(NA)
        n <- names(x)
        if (!is.null(n)) {
            names(res) <- n
        }
        res
    }
}

##' @rdname nanotime
index2char.nanotime <- function(x, ...) {
    bigint <- as.integer64(x)
    secs  <- as.integer64(bigint / as.integer64(1000000000))
    nanos <- bigint - secs * as.integer64(1000000000)
    RcppCCTZ::formatDouble(as.double(secs), as.double(nanos),
                           fmt=getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%E9S%Ez"),
                           tgttzstr=getOption("nanotimeTz", default="UTC"))
}

##' @rdname nanotime
as.POSIXct.nanotime <- function(x, tz="", ...) {
    ## if (verbose) warning("Lossy conversion dropping precision")
    if ((tz == "" || is.null(tz)) && length(attr(x, "tzone")) > 0) {
        tz <- attr(x, "tzone")
    }
    pt <- as.POSIXct(as.double(S3Part(x, strictS3=TRUE)/1e9), tz=tz, origin="1970-01-01")
    pt
}

setAs("nanotime", "POSIXct", function(from) as.POSIXct.nanotime(from))


##' @rdname nanotime
as.POSIXlt.nanotime <- function(x, tz="", ...) {
    as.POSIXlt(as.POSIXct(x, tz=tz))
}

setAs("nanotime", "POSIXlt", function(from) as.POSIXlt.nanotime(from))

##' @rdname nanotime
as.Date.nanotime <- function(x, ...) {
    as.Date(as.POSIXct(x))
}

setAs("nanotime", "Date", function(from) as.Date(as.POSIXct(from)))


##' @rdname nanotime
as.data.frame.nanotime <- function(x, ...) {
    ret <- as.data.frame(S3Part(x, strictS3=TRUE), ...)
    ## this works, but see if there's a more idiomatic and efficient way
    ## of doing this:
    ret[] <- nanotime(S3Part(x, strictS3=TRUE))
    ret
}

##' @rdname nanotime
##' @method as.integer64 nanotime
as.integer64.nanotime <- function(x, ...) {
    S3Part(x, strictS3=TRUE)
}

setAs("nanotime", "integer64", function(from) S3Part(from, strictS3=TRUE))

## ------------ `-`
##' @rdname nanotime
setMethod("-", c("nanotime", "character"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanotime
setMethod("-", c("nanotime", "nanotime"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanotime
setMethod("-", c("nanotime", "integer64"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) - e2)
          })

##' @rdname nanotime
setMethod("-", c("nanotime", "numeric"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) - e2)
          })

##' @rdname nanotime
setMethod("-", c("ANY", "nanotime"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanotime
setMethod("-", c("nanotime", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                 stop("unary '-' is not defined for \"nanotime\" objects")
              }
              else {
                  stop("invalid operand types")    			## #nocov
              }
          })


## ----------- `+`
##' @rdname nanotime
setMethod("+", c("nanotime", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                 stop("unary '+' is not defined for \"nanotime\" objects")  ## #nocov
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname nanotime
setMethod("+", c("nanotime", "integer64"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) + e2)
          })

##' @rdname nanotime
setMethod("+", c("nanotime", "numeric"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) + e2)
          })

##' @rdname nanotime
setMethod("+", c("ANY", "nanotime"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanotime
setMethod("+", c("integer64", "nanotime"),
          function(e1, e2) {
              new("nanotime", e1 + S3Part(e2, strictS3=TRUE))  		## #nocov
          })

##' @rdname nanotime
setMethod("+", c("numeric", "nanotime"),
          function(e1, e2) {
              new("nanotime", e1 + S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanotime
setMethod("+", c("nanotime", "nanotime"),
          function(e1, e2) {
              stop("invalid operand types")
          })


## ---------- other ops

##' @rdname nanotime
setMethod("Arith", c("nanotime", "nanotime"),
          function(e1, e2) {
              stop("operation not defined for \"nanotime\" objects")
          })

##' @rdname nanotime
setMethod("Arith", c("nanotime", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"nanotime\" objects")
          })

##' @rdname nanotime
setMethod("Arith", c("ANY", "nanotime"),
          function(e1, e2) {
              stop("operation not defined for \"nanotime\" objects")    ## #nocov
          })

##' @rdname nanotime
setMethod("Compare", c("nanotime", "character"),
          function(e1, e2) {
              ne2 <- nanotime(e2)
              callNextMethod(e1, ne2)
          })

##' @rdname nanotime
setMethod("Compare", c("character", "nanotime"),
          function(e1, e2) {
              ne1 <- nanotime(e1)
              callNextMethod(ne1, e2)
          })

##' @rdname nanotime
setMethod("Compare", c("nanotime", "POSIXt"),
          function(e1, e2) {
              ne2 <- nanotime(e2)
              callNextMethod(e1, ne2)
          })

##' @rdname nanotime
setMethod("Compare", c("POSIXt", "nanotime"),
          function(e1, e2) {
              ne1 <- nanotime(e1)
              callNextMethod(ne1, e2)
          })

##' @rdname nanotime
setMethod("Compare", c("nanotime", "ANY"),
          function(e1, e2) {
              if (inherits(e2, "nanotime")) {
                  e2 <- S3Part(e2, strictS3=TRUE)
              }
              callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname nanotime
setMethod("Logic", c("nanotime", "ANY"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanotime
setMethod("Logic", c("ANY", "nanotime"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanotime
setMethod("Math", c("nanotime"),
          function(x) {
              ## this is the same error message that R gives for abs("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname nanotime
setMethod("Math2", c("nanotime"),
          function(x, digits) {
              ## this is the same error message that R gives for round("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname nanotime
setMethod("Summary", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
              ## this is the same error message that R gives for sum("A")
              stop("invalid 'type' (nanotime) of argument")
          })

##' @rdname nanotime
setMethod("min", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
              new("nanotime", callNextMethod())
          })

##' @rdname nanotime
setMethod("max", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
              new("nanotime", callNextMethod())
          })

##' @rdname nanotime
setMethod("range", c("nanotime"),
          function(x, ..., na.rm = FALSE) {
              new("nanotime", callNextMethod())
          })


##' @rdname nanotime
setMethod("Complex", c("nanotime"),
          function(z) {
              ## this is the same error message that R gives for Arg("A")
              stop("non-numeric argument to function")
          })

## ----------- non ops

##' @rdname nanotime
setMethod("[[",
          signature("nanotime"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanotime", callNextMethod())
})

##' @rdname nanotime
setMethod("[",
          signature("nanotime", "numeric"),
          function (x, i, j, ..., drop=FALSE) {
              if (!missing(j) || length(list(...)) > 0) {
                  warning("unused indices or arguments in 'nanotime' subsetting")
              }
              if (isTRUE(any(i < 0))) {
                  new("nanotime", S3Part(x, strictS3=TRUE)[i])
              } else {
                  nanotime_subset_numeric_impl(x, i)
              }
          })

##' @rdname nanotime
setMethod("[",
          signature("nanotime", "logical"),
          function (x, i, j, ..., drop=FALSE) {
              if (!missing(j) || length(list(...)) > 0) {
                  warning("unused indices or arguments in 'nanotime' subsetting")
              }
              nanotime_subset_logical_impl(x, i)
          })

##' @rdname nanotime
setMethod("[",
          signature("nanotime", "character"),
          function (x, i, j, ..., drop=FALSE) {
              ## we don't implement 'period_subset_character_impl' but
              ## do the gymnastic of finding the NAs here at R level;
              ## it's not as efficient, but using 'character' indexing
              ## is by itself inefficient, so the overhead should have
              ## no practical consequences.
              if (!missing(j) || length(list(...)) > 0) {
                  warning("unused indices or arguments in 'nanotime' subsetting")
              }
              na_index <- !(i %in% names(x)) | is.na(i)
              res <- new("nanotime", S3Part(x, strictS3=TRUE)[i])
              res[na_index] <- NA_nanotime_
              res
          })

##' @rdname nanotime
setMethod("[",
          signature("nanotime", "ANY"),
          function (x, i, j, ..., drop=FALSE) {
              stop("']' not defined on 'nanotime' for index of type 'ANY'")
          })

##' @rdname nanotime
setMethod("[<-",
          signature("nanotime"),
          function (x, i, j, ..., value) {
              new("nanotime", callNextMethod())
          })

##' @rdname nanotime
c.nanotime <- function(...) {
    nanotime(c.integer64(...))
}

##' @rdname nanotime
##' @name nanotime-package
NULL

##' @rdname nanotime
setMethod("names<-",
          signature("nanotime"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- value
              x
          })

##' @rdname nanotime
setMethod("is.na",
          signature("nanotime"),
          function(x) {
              callNextMethod(S3Part(x, strictS3=TRUE))
          })


##' Sequence Generation
##'
##' Generate a sequence of \code{nanotime}
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
##' @param length.out integer indicating the desired length of the sequence
##' @param along.with take the length from the length of this argument.
##' @examples
##' \dontrun{
##' from <- as.nanotime("2018-01-14T12:44:00+00:00")
##' to   <- as.nanotime("2019-01-14T12:44:00+00:00")
##' seq(from, to, by=as.nanoperiod("1m"), tz="America/New_York")
##' seq(from, by=as.nanoperiod("1y"), length.out=4, tz="Europe/London")
##' }
##' @method seq nanotime
seq.nanotime <-
    function(from, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)
{
    if((One <- nargs() == 1L)) {
	stop("'seq.nanotime' cannot be called with only one argument")
    }
    if (sum(is.null(to), is.null(by), is.null(length.out) && is.null(along.with)) > 1) {
        stop("at least two of 'to', 'by', and 'length.out' or 'along.with' must be specified")
    }
    is.logint <- function(.) (is.integer(.) || is.logical(.)) && !is.object(.)
    if (!missing(along.with) && !is.null(along.with)) {
	length.out <- length(along.with)
    }
    else if(!missing(length.out) && !is.null(length.out)) {
        len <- length(length.out)
        if(!len) stop("argument 'length.out' must be of length 1")
        if(len > 1L) {
            warning("first element used of 'length.out' argument")
            length.out <- length.out[1L]
        }
	if(!(intn1 <- is.logint(length.out)))
	    length.out <- as.numeric(ceiling(length.out))
    }
    if (length(from) != 1L) stop("'from' must be of length 1")
    if (!missing(to) && !is.null(to) && length(to) != 1L) stop("'to' must be of length 1")
    if (!missing(to) && !is.null(to) && !(inherits(to, "nanotime")))
	stop("'to' must be a 'nanotime'")
    if (is.null(length.out)) {
	if (missing(by) || is.null(by))
	    stop("'by' must be specified if 'length.out' is NULL")
	else { # dealing with 'by'
            if (length(by) != 1L) stop("'by' must be of length 1")
            if (inherits(by, "nanoperiod")) {
                ## fish out the 'tz' parameter:
                args <- list(...)
                if (!any("tz" %in% names(args))) {
                    stop("'tz' is a mandatory argument for sequences with a 'period' step")
                }
                period_seq_from_to_impl(from, to, by, args$tz)
            } else {
                nanotime(seq(as.integer64(from), as.integer64(to), by=by))
            }
	}
    }
    else if(!is.finite(length.out) || length.out < 0L)
	stop("'length.out' must be a non-negative number")
    else if (length.out == 0L) nanotime()
    else if (missing(by) || is.null(by)) {
        ## cannot be with 'period', so just call the S3 function:
        ## calculate 'by' because of 'bit64' bug:
        by = as.integer64((to - from) / (length.out - 1))
        nanotime(seq(as.integer64(from), as.integer64(to), by, NULL, NULL, ...))
    }
    else if (missing(to) || is.null(to)) {
        if (length(by) != 1L) stop("'by' must be of length 1")
        if (inherits(by, "nanoperiod")) {
            ## fish out the 'tz' parameter:
            args <- list(...)
            if (!any("tz" %in% names(args))) {
                stop("'tz' is a mandatory argument for sequences with a 'period' step")
            }
            period_seq_from_length_impl(from, by, as.integer64(length.out), args$tz)
        } else {
            nanotime(seq(as.integer64(from), by=as.integer64(by),
                         length.out=length.out, along.with=along.with, ...))
        }
    }
    else stop("too many arguments")
}


##' @rdname seq.nanotime
setMethod("seq", c("nanotime"), seq.nanotime)


##' Test if Two Objects are (Nearly) Equal
##'
##' Compare \code{target} and \code{current} testing \sQuote{near
##' equality}.  If they are different, comparison is still made to
##' some extent, and a report of the differences is returned.  Do not
##' use \code{all.equal} directly in \code{if} expressions---either
##' use \code{isTRUE(all.equal(....))} or \code{\link{identical}} if
##' appropriate.
##'
##' @param target,current \code{nanotime} arguments to be compared
##' @param ... further arguments for different methods
##' @param tolerance numeric >= 0. Differences smaller than
##'     \code{tolerance} are not reported.  The default value is close
##'     to \code{1.5e-8}.
##' @param  scale \code{NULL} or numeric > 0, typically of length 1 or
##'          \code{length(target)}.  See \sQuote{Details}.
##' @param  countEQ logical indicating if the \code{target == current} cases should be
##'          counted when computing the mean (absolute or relative)
##'          differences.  The default, \code{FALSE} may seem misleading in
##'          cases where \code{target} and \code{current} only differ in a few
##'          places; see the extensive example.
##' @param formatFUN a \code{function} of two arguments, \code{err}, the relative, absolute
##'          or scaled error, and \code{what}, a character string indicating
##'          the _kind_ of error; maybe used, e.g., to format relative and
##'          absolute errors differently.
##' @param check.attributes logical indicating if the \code{attributes} of \code{target}
##'          and \code{current} (other than the names) should be compared.
##'
##' @seealso \code{\link{identical}}, \code{\link{isTRUE}},
##'     \code{\link{==}}, and \code{\link{all}} for exact equality
##'     testing.
##'
##' @method all.equal nanotime
##'
all.equal.nanotime <- function(target, current, tolerance = sqrt(.Machine$double.eps), 
                               scale = NULL, countEQ = FALSE,
                               formatFUN = function(err, what) format(err), ..., check.attributes = TRUE) {
    if (inherits(target, "nanotime")) target  <- as.integer64(target)
    if (inherits(current, "nanotime")) current <- as.integer64(current)
    
    if (!is.numeric(tolerance)) 
        stop("'tolerance' should be numeric")
    if (!is.numeric(scale) && !is.null(scale)) 
        stop("'scale' should be numeric or NULL")
    if (!is.logical(check.attributes)) 
        stop(gettextf("'%s' must be logical", "check.attributes"), 
             domain = NA)
    msg <- NULL
    msg <- if (check.attributes) 
               attr.all.equal(target, current, tolerance = tolerance, 
                              scale = scale, ...)
    if (data.class(target) != data.class(current)) {
        msg <- c(msg, paste0("target is ", data.class(target), 
                             ", current is ", data.class(current)))
        return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    cplx <- is.complex(target)
    if (lt != lc) {
        if (!is.null(msg)) 
            msg <- msg[-grep("\\bLengths\\b", msg)]
        msg <- c(msg, paste0(if (cplx) "Complex" else "Numeric", 
                             ": lengths (", lt, ", ", lc, ") differ"))
        return(msg)
    }
    out <- is.na(target)
    if (any(out != is.na(current))) {
        msg <- c(msg, paste("'is.NA' value mismatch:", sum(is.na(current)), 
                            "in current", sum(out), "in target"))
        return(msg)
    }
    out <- out | target == current
    if (all(out)) 
        return(if (is.null(msg)) TRUE else msg)
    if (countEQ) {
        N <- length(out)
        sabst0 <- sum(abs(target[out]))
    } else {
        sabst0 <- 0
    }
    target <- target[!out]
    current <- current[!out]
    if (!countEQ) 
        N <- length(target)
## the following is in the orignal function, but can't be reached here:
##    if (is.integer(target) && is.integer(current)) 
##        target <- as.double(target)
    xy <- sum(abs(target - current))/N
    what <- if (is.null(scale)) {
                xn <- (sabst0 + sum(abs(target)))/N
                if (is.finite(xn) && xn > tolerance) {
                    xy <- xy/xn
                    "relative"
                } else {
                    "absolute"
                }
            } else {
                stopifnot(all(scale > 0))
                xy <- xy/scale
                if (all(abs(scale - 1) < 1e-07)) 
                    "absolute"
                else "scaled"
            }
    if (cplx) 
        what <- paste(what, "Mod")      # nocov
    if (is.na(xy) || xy > tolerance) 
        msg <- c(msg, paste("Mean", what, "difference:", formatFUN(xy, what)))
    if (is.null(msg)) {
        TRUE                            # nocov
    } else msg
}

##' @rdname all.equal.nanotime
setMethod("all.equal", c(target = "nanotime", current="ANY"), all.equal.nanotime)


##' @rdname nanotime
NA_nanotime_ <- nanotime(NA)

##' Get a component of a date time
##'
##' Get a component of a date time. \code{nano_wday} returns the
##' numeric position in a week, with Sunday == 0. \code{nano_mday}
##' returns the numeric day (i.e. a value from 1 to
##' 31). \code{nano_month} returns the month (i.e. a value from 1 to
##' 12). \code{nano_year} returns the year.
##'
##' Note that the \code{tz} parameter is mandatory because the day
##' boundary is different depending on the time zone and
##' \code{nanotime} does not store the timezone as it is just an
##' offset in nanoseconds from the epoch.
##'
##' @param x a \code{nanotime} object
##' @param tz \code{character} a string representing a timezone
##' @examples
##' \dontrun{
##' nano_wday(as.nanotime("2020-03-14 23:32:00-04:00"), "America/New_York")
##' nano_wday(as.nanotime("2020-03-14 23:32:00 America/New_York"), "Europe/Paris")
##' nano_mday(as.nanotime("2020-03-14 23:32:00-04:00"), "America/New_York")
##' nano_mday(as.nanotime("2020-03-14 23:32:00 America/New_York"), "Europe/Paris")
##' nano_month(as.nanotime("2020-12-31 23:32:00-04:00"), "America/New_York")
##' nano_month(as.nanotime("2020-12-31 23:32:00 America/New_York"), "Europe/Paris")
##' nano_year(as.nanotime("2020-12-31 23:32:00-04:00"), "America/New_York")
##' nano_year(as.nanotime("2020-12-31 23:32:00 America/New_York"), "Europe/Paris")
##' }
##'
##' @rdname nano_year
##' @aliases nano_wday
##' @aliases nano_wday,nanotime-method nano_mday,nanotime-method
##' @aliases nano_month,nanotime-method nano_year,nanotime-method
##'
setGeneric("nano_wday", function(x, tz) standardGeneric("nano_wday"))
setMethod("nano_wday", c("nanotime"), function(x, tz) nanotime_wday_impl(x, tz))

##' @rdname nano_year
##' @aliases nano_mday
##'
setGeneric("nano_mday", function(x, tz) standardGeneric("nano_mday"))
setMethod("nano_mday", c("nanotime"), function(x, tz) nanotime_mday_impl(x, tz))

##' @rdname nano_year
##' @aliases nano_month
##'
setGeneric("nano_month", function(x, tz) standardGeneric("nano_month"))
setMethod("nano_month", c("nanotime"), function(x, tz) nanotime_month_impl(x, tz))

##' @rdname nano_year
##'
setGeneric("nano_year", function(x, tz) standardGeneric("nano_year"))
setMethod("nano_year", c("nanotime"), function(x, tz) nanotime_year_impl(x, tz))

## rounding ops:

##' Rounding down or up a \code{nanotime} type
##'
##' The functions \code{nano_floor} and \code{nano_ceiling} round down or up, respectively. Although
##' the underlying implementation of \code{nanotime} has negative numbers for values before
##' 1970-01-01 UTC, the rounding is always done backward in time for \code{nano_floor} and forward
##' in time for \code{nano_ceiling}. The functions take a \code{nanotime} argument \code{x} which is
##' the instance to round, together with a second argument \code{precision} which indicates an
##' arbitrary precision to which the rounding should be performed. This argument can be either a
##' \code{nanoduration} or or a \code{nanoperiod}. In the latter case, the argument \code{tz} must
##' also be specified in order to give the \code{nanoperiod} a meaning. Finally, the \code{nanotime}
##' argument \code{origin} can be optionally specified to fix the rounding to a specific point in
##' time.
##'
##' This flexible rounding must be understood in the context of a vector. The rounding precision can
##' then be considered as an interval that defines a grid over which the elements are either
##' assigned to the starting value of the interval to which they belong (\code{nano_floor}) or the
##' ending value of the interval to which they belong (\code{nano_ceiling}). This allows for a
##' grouping of a \code{nanotime} vector on which a statistic may then be run. In the examples
##' below, such a use case is shown in the context of a \code{data.table} object.
##'
##' If "business" concepts such as month or days are needed, the \code{argument} precision must be
##' of type \code{period}. It is then mandatory to specify the timezone argument \code{tz} as this
##' ensures timezone correctness of the intervals including for example for the rare hourly
##' transitions of some countries going from a timezone with a whole hour difference with UTC to one
##' with a fractional hour difference. In the case of a \code{period}, the functions align the
##' rounding if the precision is an integer divisor of a larger quantity. For instance, if one
##' specifies a rounding of 6 hours, a divisor of a day, the hours are aligned on days and the
##' rounding is made to a grid at hours 0, 6, 12 and 18 in the specified timezone. If the precision
##' is not a divisor, the grid is aligned to the nearest hour before the first element of the vector
##' to round.
##'
##' The argument \code{origin} controls the reference point of the rounding, allowing arbitrary
##' specification of the reference point of the rounding.
##'
##' @param x a \code{nanotime} object which must be sorted
##' @param precision a \code{nanoduration} or \code{nanoperiod} object
##'     indicating the rounding precision
##' @param ... for future additional arguments
##' @examples
##' \dontrun{
##' ## "classic" rounding:
##' nano_floor(as.nanotime("2010-10-10 11:12:15 UTC"), as.nanoduration("01:00:00"))
##' ## rounding with arbitrary precision:
##' nano_floor(as.nanotime("2010-10-10 11:12:15 UTC"), as.nanoduration("06:00:00"))
##' nano_floor(as.nanotime("2010-10-10 11:23:15 UTC"), as.nanoduration("00:15:00"))
##' nano_ceiling(as.nanotime("2010-10-10 11:23:15 UTC"), as.nanoduration("01:15:23"))
##' ## controlling the reference point via the 'origin' argument:
##' nano_ceiling(as.nanotime("2010-10-10 11:23:15 UTC"),
##'              as.nanoduration("01:15:23"),
##'              origin=as.nanotime("2010-10-10 11:23:15 UTC"))
##' ## using business concepts and rounding across a daylight saving change:
##' v <- seq(as.nanotime("2020-03-08 America/New_York"),
##'          by=as.nanoperiod("06:00:00"), length.out=8, tz="America/New_York")
##' print(nano_floor(v, as.nanoperiod("1d"), tz="America/New_York"), tz="America/New_York")
##' ## using the concept in a 'data.table':
##' library(data.table)
##' n <- 3 * 24
##' idx <- seq(as.nanotime("2020-03-07 America/New_York"),
##'            by=as.nanoperiod("01:00:00"), length.out=n, tz="America/New_York")
##' dt <- data.table(idx, a=1:n, b=2:(n+1))
##' dt_mean <- dt[, list(mean = mean(a)),
##'               by=nano_ceiling(idx, as.nanoperiod("1d"), tz="America/New_York")]
##' }
##'
##' @aliases nano_floor
##' 
##' @rdname rounding
setGeneric("nano_ceiling", def = function(x, precision, ...) standardGeneric("nano_ceiling"))

##' @rdname rounding
setGeneric("nano_floor",   def = function(x, precision, ...) standardGeneric("nano_floor"))


##' Replicate Elements
##'
##' Replicates the values in 'x' similarly to the default method.
##'
##' @param x a vector of \code{nanotime}
##' @param ... further arguments:
##' 
##'        'times' an integer-valued vector giving the (non-negative)
##'            number of times to repeat each element if of length
##'            'length(x)', or to repeat the whole vector if of length
##'            1. Negative or 'NA' values are an error. A 'double'
##'            vector is accepted, other inputs being coerced to an
##'            integer or double vector.
##' 
##'        'length.out' non-negative integer. The desired length of the
##'            output vector. Other inputs will be coerced to a double
##'            vector and the first element taken. Ignored if 'NA' or
##'            invalid.
##' 
##'        'each' non-negative integer. Each element of 'x' is repeated
##'            'each' times.  Other inputs will be coerced to an integer
##'            or double vector and the first element taken. Treated as
##'            '1' if 'NA' or invalid.
setMethod("rep", c(x = "nanotime"), function(x, ...) {
    new("nanotime", callNextMethod())
})

##' @rdname nanotime
as.character.nanotime <- function(x, ...) {
    format(x, ...)
}
