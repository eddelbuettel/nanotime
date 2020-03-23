##' @rdname nanoduration
setClass("nanoduration", contains = "integer64")

##' Duration type with nanosecond precision
##'
##' The type \code{nanoduration} is a length of time (implemented as
##' an S4 class) with nanosecond precision. It is a count of
##' nanoseconds and may be negative. The expected arithmetic
##' operations are provided, including sequence generation.
##'
##' A \code{nanoduration} can be constructed with the function
##' \code{as.nanoduration} which can take the types \code{integer64},
##' \code{integer} and \code{numeric} (all indicating the count in
##' nanosecond units) or the type \code{character}.
##'
##' It can also be constructed by specifying with individual arguments
##' the hours, minutes, seconds and nanoseconds with a call to
##' \code{nanoduration}.
##'
##' A \code{nanoduration} is displayed as hours, minutes, seconds and
##' nanoseconds like this: \code{110:12:34.123_453_001}. The nanosecond
##' precision displayed is adjusted as necessary, so e.g. 1 second is
##' displayed as \code{00:00:01}.
##'
##' @param hours number of hours
##' @param minutes number of minutes
##' @param seconds number of seconds
##' @param nanoseconds number of nanoseconds
##' @param x a \code{nanoduration} object
##' @param ... further arguments passed to or from methods.
##' @param e1 Operand of class \code{nanoival}
##' @param e2 Operand of class \code{nanoival}
##' @param object argument for method \code{show}
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @param value argument for \code{nanoduration-class}
##' @param na.rm if \code{TRUE} NA values are removed for the
##'     computation
##' @param quote indicates if the output of \code{print} should be
##'     quoted
##' @return A nanoduration object
##' @author Dirk Eddelbuettel
##' @author Leonardo Silvestri
##' @examples
##' ## constructors:
##' nanoduration(hours=10, minutes=3, seconds=2, nanoseconds=999999999)
##' as.nanoduration("10:03:02.999_999_999")
##' as.nanoduration(36182999999999)
##'
##' ## arithmetic:
##' as.nanoduration(10e9) - as.nanoduration(9e9)
##' as.nanoduration(10e9) + as.nanoduration(-9e9)
##' as.nanoduration("24:00:00") / 2
##' as.nanoduration("24:00:00") / as.nanoduration("12:00:00")
##'
##' @seealso
##' \code{\link{nanotime}}
##'
##' @aliases  *,ANY,nanoduration-method
##' @aliases  *,nanoduration,ANY-method
##' @aliases  *,nanoduration,nanoduration-method
##' @aliases  +,ANY,nanoduration-method
##' @aliases  /,ANY,nanoduration-method
##' @aliases  /,nanoduration,ANY-method
##' @aliases  Complex,nanoduration-method
##' @aliases  Logic,ANY,nanoduration-method
##' @aliases  Logic,nanoduration,ANY-method
##' @aliases  Logic,nanoduration,nanoduration-method
##' @aliases  Math2,nanoduration-method
##' @aliases  Math,nanoduration-method
##' @aliases  Summary,nanoduration-method
##'
##' @rdname nanoduration
nanoduration <- function(hours, minutes, seconds, nanoseconds) {
    if (nargs()==0) {
        as.nanoduration(NULL)
    } else {
        make_duration_impl(as.integer64(hours),
                           as.integer64(minutes),
                           as.integer64(seconds),
                           as.integer64(nanoseconds))
    }
}


##' @noRd
setGeneric("as.nanoduration", function(x) standardGeneric("as.nanoduration"))

##' @rdname nanoduration
##' @aliases as.nanoduration
setMethod("as.nanoduration",
          "character",
          function(x) {
              duration_from_string_impl(x)
          })

setAs("character", "nanoduration", function(from) as.nanoduration(from))

##' @rdname nanoduration
setMethod("as.nanoduration",
          "integer64",
          function(x) {
              new("nanoduration", x)
          })

setAs("integer64", "nanoduration", function(from) as.nanoduration(from))


##' @rdname nanoduration
setMethod("as.nanoduration",
          "numeric",
          function(x) {
              nm <- names(x)
              res <- new("nanoduration", as.integer64(x))
              if (!is.null(nm)) {
                  names(res) <- nm
              }
              res
          })

setAs("numeric", "nanoduration", function(from) as.nanoduration(from))


##' @rdname nanoduration
setMethod("as.nanoduration",
          "integer",
          function(x) {
              nm <- names(x)
              res <- new("nanoduration", as.integer64(x))
              if (!is.null(nm)) {
                  names(res) <- nm
              }
              res
          })

setAs("integer", "nanoduration", function(from) as.nanoduration(from))

##' @rdname nanoduration
setMethod("as.nanoduration",
          "NULL",
          function(x) {
              new("nanoduration", as.integer64(NULL))
          })

##' @rdname nanoduration
setMethod("as.nanoduration",
          "missing",
          function(x) {
              new("nanoduration", integer64())
          })

##' @rdname nanoduration
setMethod("show",
          signature("nanoduration"),
          function(object) print(object))

##' @rdname nanoduration
setMethod("print",
          "nanoduration",
          function(x, quote=FALSE, ...) {
              if (length(x) == 0) {
                  print("nanoduration(0)", quote=quote)
              } else {
                  print(duration_to_string_impl(x), quote=quote)
              }
          })

##' @rdname nanoduration
format.nanoduration <- function(x, ...) {
    as.character(x)
}


##' @rdname nanoduration
##' @method as.integer64 nanoduration
as.integer64.nanoduration <- function(x, ...) {
    S3Part(x, strictS3=TRUE)
}


##' @rdname nanoduration
setMethod("as.character",
          signature("nanoduration"),
          function(x) {
              duration_to_string_impl(x)
          })


##' @rdname nanoduration
setMethod("is.na",
          "nanoduration",
          function(x) {
              duration_is_na_impl(x)
          })


## ------------ `-`

## nanoduration - other
##' @rdname nanoduration
setMethod("-", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
setMethod("-", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
setMethod("-", c("nanoduration", "integer"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
setMethod("-", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
setMethod("-", c("nanoduration", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  new("nanoduration", -S3Part(e1, strictS3=TRUE))
              }
              else {
                  stop("invalid operand types")
              }
          })

## other - nanoduration

##' @rdname nanoduration
setMethod("-", c("nanotime", "nanoduration"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
setMethod("-", c("integer64", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
setMethod("-", c("integer", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
setMethod("-", c("numeric", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
setMethod("-", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `+`

##' @rdname nanoduration
setMethod("+", c("nanoduration", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  e1
              }
              else {
                  stop("invalid operand types")
              }
          })
##' @rdname nanoduration
setMethod("+", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) + e2)
          })
##' @rdname nanoduration
setMethod("+", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) + e2)
          })
##' @rdname nanoduration
setMethod("+", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) + e2)
          })

##' @rdname nanoduration
setMethod("+", c("nanotime", "nanoduration"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) + S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanoduration
setMethod("+", c("nanoival", "nanoduration"),
          function(e1, e2) {
              new("nanoival", nanoival_plus_impl(e1, e2))
          })

##' @noRd
setMethod("+", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname nanoduration
setMethod("+", c("integer64", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 + S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
setMethod("+", c("numeric", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 + S3Part(e2, strictS3=TRUE))
          })

## ----------- `*`

##' @noRd
setMethod("*", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname nanoduration
setMethod("*", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) * e2)
          })
##' @rdname nanoduration
setMethod("*", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) * e2)
          })
##' @rdname nanoduration
setMethod("*", c("numeric", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 * S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
setMethod("*", c("integer64", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 * S3Part(e2, strictS3=TRUE))
          })
##' @noRd
setMethod("*", c("nanoduration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @noRd
setMethod("*", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `/`

##' @rdname nanoduration
setMethod("/", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              as.integer64(S3Part(e1, strictS3=TRUE) / S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
setMethod("/", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
##' @rdname nanoduration
setMethod("/", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
##' @noRd
setMethod("/", c("nanoduration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @noRd
setMethod("/", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ---------- other ops

##' @rdname nanoduration
setMethod("Arith", c("nanoduration", "ANY"),
          function(e1, e2) {
              callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname nanoduration
setMethod("Compare", c("nanoduration", "ANY"),
          function(e1, e2) {
              callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @noRd
setMethod("Logic", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @noRd
setMethod("Logic", c("nanoduration", "ANY"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @noRd
setMethod("Logic", c("ANY", "nanoduration"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanoduration
setMethod("abs", c("nanoduration"),
          function(x) {
              new("nanoduration", callNextMethod(S3Part(x, strictS3=TRUE)))
          })
##' @rdname nanoduration
setMethod("sign", c("nanoduration"),
          function(x) {
              callNextMethod(S3Part(x, strictS3=TRUE))
          })


##' @noRd
setMethod("Math", c("nanoduration"),
          function(x) {
              ## this is the same error message that R gives for abs("A")
              stop("non-numeric argument to mathematical function")
          })

##' @noRd
setMethod("Math2", c("nanoduration"),
          function(x, digits) {
              ## this is the same error message that R gives for round("A")
              stop("non-numeric argument to mathematical function")
          })

##' @noRd
setMethod("Summary", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              ## this is the same error message that R gives for sum("A")
              stop("invalid 'type' (nanoduration) of argument")
          })

##' @rdname nanoduration
setMethod("sum", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
setMethod("min", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
setMethod("max", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
setMethod("range", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })


##' @noRd
setMethod("Complex", c("nanoduration"),
          function(z) {
              ## this is the same error message that R gives for Arg("A")
              stop("non-numeric argument to function")
          })


## non-ops:
## -------

## subset/subassign

##' @rdname nanoduration
setMethod("[[",
          signature("nanoduration"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
setMethod("[",
          signature("nanoduration"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
setMethod("[<-",
          signature("nanoduration"),
          function (x, i, j, ..., value) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
c.nanoduration <- function(...) {
    as.nanoduration(c.integer64(...))
}

##' @rdname nanotime
as.data.frame.nanoduration <- function(x, ...) {
    ret <- as.data.frame(S3Part(x, strictS3=TRUE), ...)
    ## this works, but see if there's a more idiomatic and efficient way
    ## of doing this:
    ret[] <- as.nanoduration(S3Part(x, strictS3=TRUE))
    ret
}

##' Sequence Generation
##'
##' Generate a sequence of \code{nanoduration}
##'
##' @param ... arguments passed to or from methods
##' @param from,to the starting and (maximal) end values of the
##'     sequence
##' @param by the increment of the sequence
##' @param length.out integer indicating the desired length of the sequence
##' @param along.with take the length from the length of this argument.
##'
##' @examples
##' seq(from=as.nanoduration(0), by=as.nanoduration("01:00:00"), length.out=10)
##' @method seq nanoduration
seq.nanoduration <- function(from, to=NULL, by=NULL, length.out=NULL, along.with=NULL, ...) {
    ## workaroud 'bit64' bug:
    if (is.null(by)) {
        if (is.null(length.out)) {
            length.out  <- length(along.with)
        }
        by = (to - from) / (length.out - 1)
    }
    as.nanoduration(seq(as.integer64(from), to, as.integer64(by), length.out, along.with, ...))
}


##' @rdname nanoduration
NA_nanoduration_  <- as.nanoduration(NA_integer_)
