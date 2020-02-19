##' @rdname nanoduration
##' @export
setClass("nanoduration", contains = "integer64")

##' Duration type with nanosecond precision
##'
##' \code{nanoduration} is a length of time type (implemented as an S4
##' class) with nanosecond precision. It is a count of nanoseconds
##' which may be negative.
##'
##' A nanoduration can be constructed with the function 'as.nanoduration'
##' which can take the types 'integer64', 'integer' and 'numeric' (all
##' indicating the count in nanosecond units) or the type 'character'.

##' @section Output Format:
##'
##' A \code{nanoduration} is displayed as hours, minutes, seconds and
##' nanoseconds like this: \code{110:12:34.123_453_000}.
##'
##'
##' @param hours number of hours
##' @param minutes number of minutes
##' @param seconds number of seconds
##' @param nanoseconds number of nanoseconds
##' @param x a \code{nanoduration} object
##' @param tz a timezone string
##' @param ... further arguments passed to or from methods.
##' @param e1 Operand of class \code{nanoival}
##' @param e2 Operand of class \code{nanoival}
##' @param digits Required for \code{Math2} signature but ignored here
##' @param recursive argument for method \code{c}
##' @param object argument for method \code{show}
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @param z Required for \code{Complex} signature but ignored here
##' @param value argument for \code{nanoduration-class}
##' @return A nanoduration object
##' @author Leonardo Silvestri
##' @examples

##' @rdname nanoduration
##' @export
nanoduration <- function(hours, minutes, seconds, nanoseconds) {
  .Call("make_duration",
        as.integer64(hours),
        as.integer64(minutes),
        as.integer64(seconds),
        as.integer64(nanoseconds))
}


setGeneric("as.nanoduration", function(x) standardGeneric("as.nanoduration"))

##' @rdname nanoduration
##' @export
setMethod("as.nanoduration",
          "character",
          function(x) {
              .Call('duration_from_string', x)
          })

setAs("character", "nanoduration", function(from) as.nanoduration(from))

##' @rdname nanoduration
##' @export
setMethod("as.nanoduration",
          "integer64",
          function(x) {
              new("nanoduration", x)
          })

setAs("integer64", "nanoduration", function(from) as.nanoduration(from))


##' @rdname nanoduration
##' @export
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
##' @export
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
##' @export
setMethod("show",
          signature("nanoduration"),
          function(object) print(object))

##' @rdname nanoduration
##' @export
setMethod("print",
          "nanoduration",
          function(x, ...) {
              print(.Call('duration_to_string', x))
          })

##' @rdname nanoduration
##' @export
format.nanoduration <- function(x, ...) {
    as.character(x)
}

  
## needed? LLL
as.integer64.nanoduration <- function(x, ...) {
    S3Part(x, strictS3=TRUE)
}


##' @rdname nanoduration
##' @export
setMethod("as.character",
          signature("nanoduration"),
          function(x) {
              .Call('duration_to_string', x)
          })


##' @rdname nanoduration
##' @export
setMethod("is.na",
          "nanoduration",
          function(x) {
              .Call("duration_is_na", x)
          })


## ------------ `-`

## nanoduration - other
##' @rdname nanoduration
##' @export
setMethod("-", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
##' @export
setMethod("-", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
##' @export
setMethod("-", c("nanoduration", "integer"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
##' @export
setMethod("-", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
##' @export
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
##' @export
setMethod("-", c("nanotime", "nanoduration"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname nanoduration
##' @export
setMethod("-", c("integer64", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
##' @export
setMethod("-", c("integer", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
##' @export
setMethod("-", c("numeric", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
##' @export
setMethod("-", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `+`

##' @rdname nanoduration
##' @export
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
##' @export
setMethod("+", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) + e2)
          })
##' @rdname nanoduration
##' @export
setMethod("+", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) + e2)
          })
##' @rdname nanoduration
##' @export
setMethod("+", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) + e2)
          })

##' @rdname nanoduration
##' @export
setMethod("+", c("nanotime", "nanoduration"),
          function(e1, e2) {
              new("nanotime", S3Part(e1, strictS3=TRUE) + S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanoduration
##' @export
setMethod("+", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname nanoduration
##' @export
setMethod("+", c("integer64", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 + S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
##' @export
setMethod("+", c("numeric", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 + S3Part(e2, strictS3=TRUE))
          })

## ----------- `*`

##' @rdname nanoduration
##' @export
setMethod("*", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")              
          })
##' @rdname nanoduration
##' @export
setMethod("*", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) * e2)
          })
##' @rdname nanoduration
##' @export
setMethod("*", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", S3Part(e1, strictS3=TRUE) * e2)
          })
##' @rdname nanoduration
##' @export
setMethod("*", c("numeric", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 * S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
##' @export
setMethod("*", c("integer64", "nanoduration"),
          function(e1, e2) {
              new("nanoduration", e1 * S3Part(e2, strictS3=TRUE))
          })
##' @rdname nanoduration
##' @export
setMethod("*", c("nanoduration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname nanoduration
##' @export
setMethod("*", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `/`

##' @rdname nanoduration
##' @export
setMethod("/", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname nanoduration
##' @export
setMethod("/", c("nanoduration", "integer64"),
          function(e1, e2) {
              new("nanoduration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
##' @rdname nanoduration
##' @export
setMethod("/", c("nanoduration", "numeric"),
          function(e1, e2) {
              new("nanoduration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
##' @rdname nanoduration
##' @export
setMethod("/", c("nanoduration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname nanoduration
##' @export
setMethod("/", c("ANY", "nanoduration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ---------- other ops

##' @rdname nanoduration
##' @export
setMethod("Arith", c("nanoduration", "ANY"),
          function(e1, e2) {
              callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname nanoduration
##' @export
setMethod("Compare", c("nanoduration", "ANY"),
          function(e1, e2) {
              callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname nanoduration
##' @export
setMethod("Logic", c("nanoduration", "nanoduration"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanoduration
##' @export
setMethod("Logic", c("nanoduration", "ANY"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanoduration
##' @export
setMethod("Logic", c("ANY", "nanoduration"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname nanoduration
##' @export
setMethod("abs", c("nanoduration"),
          function(x) {
              new("nanoduration", callNextMethod(S3Part(x, strictS3=TRUE)))
          })
##' @rdname nanoduration
##' @export
setMethod("sign", c("nanoduration"),
          function(x) {
              callNextMethod(S3Part(x, strictS3=TRUE))
          })


##' @rdname nanoduration
##' @export
setMethod("Math", c("nanoduration"),
          function(x) {
              ## this is the same error message that R gives for abs("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname nanoduration
##' @export
setMethod("Math2", c("nanoduration"),
          function(x, digits) {
              ## this is the same error message that R gives for round("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname nanoduration
##' @export
setMethod("Summary", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              ## this is the same error message that R gives for sum("A")
              stop("invalid 'type' (nanoduration) of argument")
          })

##' @rdname nanoduration
##' @export
setMethod("sum", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
##' @export
setMethod("min", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
##' @export
setMethod("max", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
##' @export
setMethod("range", c("nanoduration"),
          function(x, ..., na.rm = FALSE) {
              new("nanoduration", callNextMethod())
          })


##' @rdname nanoduration
##' @export
setMethod("Complex", c("nanoduration"),
          function(z) {
              ## this is the same error message that R gives for Arg("A")
              stop("non-numeric argument to function")
          })


## non-ops:
## -------

## subset/subassign

##' @rdname nanoduration
##' @export
setMethod("[[",
          signature("nanoduration"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
##' @export
setMethod("[",
          signature("nanoduration"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
##' @export
setMethod("[<-",
          signature("nanoduration"),
          function (x, i, j, ..., value) {
              new("nanoduration", callNextMethod())
          })

##' @rdname nanoduration
##' @export
c.nanoduration <- function(...) {
    as.nanoduration(c.integer64(...))
}

##' @rdname nanotime
##' @export
as.data.frame.nanoduration <- function(x, ...) {
    ret <- as.data.frame(S3Part(x, strictS3=TRUE), ...)
    ## this works, but see if there's a more idiomatic and efficient way
    ## of doing this:
    ret[] <- as.nanoduration(S3Part(x, strictS3=TRUE))
    ret
}

NA_nanoduration_  <- as.nanoduration(NA_integer_)
