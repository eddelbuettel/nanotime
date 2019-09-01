##' @rdname duration
##' @export
setClass("duration", contains = "integer64")

##' Duration type with nanosecond precision
##'
##' \code{duration} is a length of time type (implemented as an S4
##' class) with nanosecond precision. It is a count of nanoseconds
##' which may be negative.
##'
##' A duration can be constructed with the function 'as.duration'
##' which can take the types 'integer64', 'integer' and 'numeric' (all
##' indicating the count in nanosecond units) or the type 'character'.

##' @section Output Format:
##'
##' A \code{duration} is displayed as hours, minutes, seconds and
##' nanoseconds like this: \code{110:12:34.123_453_000}.
##'
##'
##' @param hours number of hours
##' @param minutes number of minutes
##' @param seconds number of seconds
##' @param nanoseconds number of nanoseconds
##' @param x a \code{duration} object
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
##' @param value argument for \code{duration-class}
##' @return A duration object
##' @author Leonardo Silvestri
##' @examples

##' @rdname duration
##' @export
duration <- function(hours, minutes, seconds, nanoseconds) {
  .Call("make_duration",
        as.integer64(hours),
        as.integer64(minutes),
        as.integer64(seconds),
        as.integer64(nanoseconds))
}


setGeneric("as.duration", function(x) standardGeneric("as.duration"))

##' @rdname duration
##' @export
setMethod("as.duration",
          "character",
          function(x) {
              res <- .Call('duration_from_string', x)
              oldClass(res) <- "integer64"
              new("duration", res)
          })

##' @rdname duration
##' @export
setMethod("as.duration",
          "integer64",
          function(x) {
              new("duration", x)
          })

##' @rdname duration
##' @export
setMethod("as.duration",
          "numeric",
          function(x) {
              nm <- names(x)
              res <- new("duration", as.integer64(x))
              if (!is.null(nm)) {
                  names(res) <- nm
              }
              res
          })

##' @rdname duration
##' @export
setMethod("as.duration",
          "integer",
          function(x) {
              nm <- names(x)
              res <- new("duration", as.integer64(x))
              if (!is.null(nm)) {
                  names(res) <- nm
              }
              res
          })

##' @rdname duration
##' @export
setMethod("show",
          signature("duration"),
          function(object) print(object))

##' @rdname duration
##' @export
setMethod("print",
          "duration",
          function(x, ...) {
              print(.Call('duration_to_string', x))
          })

## needed? LLL
as.integer64.duration <- function(x, ...) {
    S3Part(x, strictS3=TRUE)
}

setMethod("as.character",
          signature("duration"),
          function(x) {
              .Call('duration_to_string', x)
          })


## ------------ `-`

## duration - other
##' @rdname duration
##' @export
setMethod("-", c("duration", "duration"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - S3Part(e2, strictS3=TRUE))
          })
##' @rdname duration
##' @export
setMethod("-", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname duration
##' @export
setMethod("-", c("duration", "integer"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname duration
##' @export
setMethod("-", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - e2)
          })
##' @rdname duration
##' @export
setMethod("-", c("duration", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  new("duration", -S3Part(e1, strictS3=TRUE))
              }
              else {
                  stop("invalid operand types")
              }
          })

## other - duration

##' @rdname duration
##' @export
setMethod("-", c("integer64", "duration"),
          function(e1, e2) {
              new("duration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname duration
##' @export
setMethod("-", c("integer", "duration"),
          function(e1, e2) {
              new("duration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname duration
##' @export
setMethod("-", c("numeric", "duration"),
          function(e1, e2) {
              new("duration", e1 - S3Part(e2, strictS3=TRUE))
          })
##' @rdname duration
##' @export
setMethod("-", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `+`

##' @rdname duration
##' @export
setMethod("+", c("duration", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  e1
              }
              else {
                  stop("invalid operand types")
              }
          })
##' @rdname duration
##' @export
setMethod("+", c("duration", "duration"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) + e2)
          })
##' @rdname duration
##' @export
setMethod("+", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) + e2)
          })
##' @rdname duration
##' @export
setMethod("+", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) + e2)
          })
##' @rdname duration
##' @export
setMethod("+", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname duration
##' @export
setMethod("+", c("integer64", "duration"),
          function(e1, e2) {
              new("duration", e1 + S3Part(e2, strictS3=TRUE))
          })
##' @rdname duration
##' @export
setMethod("+", c("numeric", "duration"),
          function(e1, e2) {
              new("duration", e1 + S3Part(e2, strictS3=TRUE))
          })

## ----------- `*`

##' @rdname duration
##' @export
setMethod("*", c("duration", "duration"),
          function(e1, e2) {
              stop("invalid operand types")              
          })
##' @rdname duration
##' @export
setMethod("*", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) * e2)
          })
##' @rdname duration
##' @export
setMethod("*", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) * e2)
          })
##' @rdname duration
##' @export
setMethod("*", c("numeric", "duration"),
          function(e1, e2) {
              new("duration", e1 * S3Part(e2, strictS3=TRUE))
          })
##' @rdname duration
##' @export
setMethod("*", c("integer64", "duration"),
          function(e1, e2) {
              new("duration", e1 * S3Part(e2, strictS3=TRUE))
          })
##' @rdname duration
##' @export
setMethod("*", c("duration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname duration
##' @export
setMethod("*", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `/`

##' @rdname duration
##' @export
setMethod("/", c("duration", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname duration
##' @export
setMethod("/", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
##' @rdname duration
##' @export
setMethod("/", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
##' @rdname duration
##' @export
setMethod("/", c("duration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname duration
##' @export
setMethod("/", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ---------- other ops

##' @rdname duration
##' @export
setMethod("Arith", c("duration", "ANY"),
          function(e1, e2) {
              callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname duration
##' @export
setMethod("Compare", c("duration", "ANY"),
          function(e1, e2) {
              callNextMethod(S3Part(e1, strictS3=TRUE), e2)
          })

##' @rdname duration
##' @export
setMethod("Logic", c("duration", "duration"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname duration
##' @export
setMethod("Logic", c("duration", "ANY"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname duration
##' @export
setMethod("Logic", c("ANY", "duration"),
          function(e1, e2) {
              ## this is the same error message that R gives for "A" | "A"
              stop("operations are possible only for numeric, logical or complex types")
          })

##' @rdname duration
##' @export
setMethod("abs", c("duration"),
          function(x) {
              new("duration", callNextMethod(S3Part(x, strictS3=TRUE)))
          })
##' @rdname duration
##' @export
setMethod("sign", c("duration"),
          function(x) {
              callNextMethod(S3Part(x, strictS3=TRUE))
          })


##' @rdname duration
##' @export
setMethod("Math", c("duration"),
          function(x) {
              ## this is the same error message that R gives for abs("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname duration
##' @export
setMethod("Math2", c("duration"),
          function(x, digits) {
              ## this is the same error message that R gives for round("A")
              stop("non-numeric argument to mathematical function")
          })

##' @rdname duration
##' @export
setMethod("Summary", c("duration"),
          function(x, ..., na.rm = FALSE) {
              ## this is the same error message that R gives for sum("A")
              stop("invalid 'type' (duration) of argument")
          })

##' @rdname duration
##' @export
setMethod("sum", c("duration"),
          function(x, ..., na.rm = FALSE) {
              new("duration", callNextMethod())
          })

##' @rdname duration
##' @export
setMethod("min", c("duration"),
          function(x, ..., na.rm = FALSE) {
              new("duration", callNextMethod())
          })

##' @rdname duration
##' @export
setMethod("max", c("duration"),
          function(x, ..., na.rm = FALSE) {
              new("duration", callNextMethod())
          })

##' @rdname duration
##' @export
setMethod("range", c("duration"),
          function(x, ..., na.rm = FALSE) {
              new("duration", callNextMethod())
          })


##' @rdname duration
##' @export
setMethod("Complex", c("duration"),
          function(z) {
              ## this is the same error message that R gives for Arg("A")
              stop("non-numeric argument to function")
          })


## non-ops:
## -------

## subset/subassign

##' @rdname duration
##' @export
setMethod("[",
          signature("duration"),
          function (x, i, j, ..., drop=FALSE) {
              new("duration", callNextMethod())
          })

##' @rdname duration
##' @export
setMethod("[<-",
          signature("duration"),
          function (x, i, j, ..., value) {
              new("duration", callNextMethod())
          })

##' @rdname duration
##' @export
c.duration <- function(...) {
    res <- do.call(c.integer64, list(...))
    new("duration", res)
}

##' @rdname duration
##' @export
setMethod("names<-",
          signature("duration"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- value
              x
          })
