##' @rdname nanoperiod
##' @export
setClass("nanoperiod", contains = "complex")

##' Period type with nanosecond precision
##'
##' \code{nanoperiod} is a length of time type (implemented as an S4
##' class) with nanosecond precision. It differs from \code{nanoduration}
##' because it is capable of representing calendar months and days. It
##' can of course also reprensent years (immutably 12 months) and
##' weeks (immutably 7 days). 
##'
##' @section Output Format:
##'
##' A \code{nanoperiod} is displayed as months, days, and \code{nanoduration}
##' like this: \code{10m2d/10:12:34.123_453_000}.
##'
##' @section Details:
##'
##' 
##' 
##' Adding or subtracting \code{nanoperiod} and \code{nanotime} require a
##' timezone as third argument. For this reason it is not possible to
##' use the binary operator `code{+}`. Instead the functions
##' `\code{plus}` and `\code{minus}` are defined.
##'
##' 
##' @param x The object which want to convert to class \code{nanoperiod}
##' @param ... further arguments passed to or from methods.
##' @param e1 Operand of class \code{nanoperiod}
##' @param e2 Operand of class \code{nanoperiod}
##' @param object argument for method \code{show}
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @author Leonardo Silvestri
##' @examples
##' x <- nanoperiod(months=12, days=7, duration="01:00:00")
##' print(x)
##' y <- nanotime("1970-01-01T00:00:00.000000001+00:00")
##' plus(y, x, tz="America/Chicago")
##' 


##' @rdname nanoperiod
##' @export
nanoperiod <- function(months=0, days=0, duration=as.nanoduration(0)) {
    as.nanoperiod(paste0(as.integer64(months), "m", as.integer64(days), "d", "/", as.nanoduration(duration)))
}

setGeneric("nanoperiod")


setGeneric("as.nanoperiod", function(x) standardGeneric("as.nanoperiod"))

##' @rdname nanoperiod
##' @export
setMethod("as.nanoperiod",
          "character",
          function(x) {
              new("nanoperiod", .Call('period_from_string', x))
          })

##' @rdname nanoperiod
##' @export
setMethod("as.nanoperiod",
          "integer64",
          function(x) {
              new("nanoperiod", .Call('period_from_integer64', x))
          })

##' @rdname nanoperiod
##' @export
setMethod("as.nanoperiod",
          "numeric",
          function(x) {
              new("nanoperiod", .Call('period_from_double', x))
          })

##' @rdname nanoperiod
##' @export
setMethod("as.nanoperiod",
          "integer",
          function(x) {
              new("nanoperiod", .Call('period_from_integer', x))
          })

##' @rdname nanoperiod
##' @export
setMethod("as.nanoperiod",
          "nanoduration",
          function(x) {
              new("nanoperiod", .Call('period_from_integer64', x))
          })

##' @rdname nanoperiod
##' @export
setMethod("show",
          signature("nanoperiod"),
          function(object) print(object))

##' @rdname nanoperiod
##' @export
setMethod("print",
          "nanoperiod",
          function(x, ...) {
              print(.Call('period_to_string', x))
          })

##' @rdname nanoperiod
##' @export
format.nanoperiod <- function(x, ...) {
  .Call('period_to_string', x)
}

##' @rdname nanoperiod
##' @export
setMethod("as.character",
          signature("nanoperiod"),
          function(x) {
              .Call('period_to_string', x)
          })

##' @rdname nanoperiod
##' @export
setMethod("is.na",
          "nanoperiod",
          function(x) {
              .Call("period_isna", x)
          })

##' @rdname nanoperiod
##' @export
setMethod("is.na<-",
          "nanoperiod",
          function(x, value) {
              x[value] <- NA_nanoperiod_
              x
          })

## accessors

## ----------- non ops

##' @rdname nanoperiod
##' @export
setMethod("[[",
          signature("nanoperiod", "ANY"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoperiod", callNextMethod())
          })

##' @rdname nanoperiod
##' @export
setMethod("[",
          signature("nanoperiod", "ANY"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoperiod", callNextMethod())
          })

##' @rdname nanoperiod
##' @export
setMethod("[<-",
          signature("nanoperiod", "ANY", "ANY", "ANY"),
          function (x, i, j, ..., value) {
              new("nanoperiod", callNextMethod())
          })

##' @rdname nanoperiod
##' @export
c.nanoperiod <- function(...) {
    args <- list(...)
    s3args <- lapply(args, function (x) S3Part(x, strictS3=TRUE))
    res <- do.call(c, s3args)
    names(res) <- names(args)
    new("nanoperiod", res)
}

setGeneric("nanoperiod.month", function(x) standardGeneric("nanoperiod.month"))
setMethod("nanoperiod.month",
          "nanoperiod",
          function(x) {
              .Call('period_month', x)
          })

setGeneric("nanoperiod.day", function(x) standardGeneric("nanoperiod.day"))
setMethod("nanoperiod.day",
          "nanoperiod",
          function(x) {
              .Call('period_day', x)
          })

setGeneric("nanoperiod.nanoduration", function(x) standardGeneric("nanoperiod.nanoduration"))
setMethod("nanoperiod.nanoduration",
          "nanoperiod",
          function(x) {
              .Call('period_duration', x)
          })

##' @rdname nanoperiod
##' @export
setMethod("names",
          signature("nanoperiod"),
          function(x) {
              callNextMethod()
          })

##' @rdname nanoperiod
##' @export
setMethod("names<-",
          signature("nanoperiod"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- value
              new("nanoperiod", x)
          })


## ----------- make sure ops that don't make sense error out
##' @rdname nanoperiod
##' @export
setMethod("Ops", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"nanoperiod\" objects")
          })
##' @rdname nanoperiod
##' @export
setMethod("Ops", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("operation not defined for \"nanoperiod\" objects")
          })

##' @rdname nanoperiod
##' @export
setMethod("Math", c("nanoperiod"),
          function(x) {
              stop("operation not defined for \"nanoperiod\" objects")
          })

##' @rdname nanoperiod
##' @export
setMethod("Math2", c("nanoperiod"),
          function(x, digits) {
              stop("operation not defined for \"nanoperiod\" objects")
          })

##' @rdname nanoperiod
##' @export
setMethod("Summary", c("nanoperiod"),
          function(x, ..., na.rm = FALSE) {
              stop("invalid 'type' (nanoperiod) of argument")
          })

##' @rdname nanoperiod
##' @export
setMethod("Complex", c("nanoperiod"),
          function(z) {
              stop("operation not defined for \"nanoperiod\" objects")
          })

## ------------ `-`
##' @rdname nanoperiod
##' @export
setMethod("-", c("nanoperiod", "ANY"),
          function(e1, e2) {
              print("entering - nanoperiod ANY")
              if (missing(e2)) {
                  ## incorrect LLL
                  new("nanoperiod", -S3Part(e1, strictS3=TRUE))
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname nanoperiod
##' @export
setMethod("-", c("nanoperiod", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_period_period", e1, e2)
          })

## --
##' @rdname nanoperiod
##' @export
setMethod("-", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @export
setMethod("-", c("nanoperiod", "nanoduration"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
##' @export
setMethod("-", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
##' @export
setMethod("-", c("nanotime", "nanoperiod"),
          function(e1, e2) {
              stop(paste0("binary '-' is not defined for \"nanotime\" and \"nanoperiod\" ",
                          "objects; instead use \"minus(e1, e2, tz)\""))
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoperiod", "nanotime"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## setMethod("-", c("nanoperiod", "integer"),
##           function(e1, e2) {
##               .Call("minus_period_integer64", e1, as.integer64(e2))
##           })

##' @rdname nanoperiod
##' @export
setMethod("-", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, as.integer64(e2))
          })

## --
##' @rdname nanoperiod
##' @export
setMethod("-", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## --
##' @rdname nanoperiod
##' @export
setMethod("-", c("nanoduration", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_integer64_period", e1, e2)
          })

## --
##' @rdname nanoperiod
##' @export
setMethod("-", c("integer64", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_integer64_period", as.integer64(e1), e2)
          })

## --
##' @rdname nanoperiod
##' @export
setMethod("-", c("numeric", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_integer64_period", as.integer64(e1), e2)
          })


## ----------- `+`

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoperiod", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  e2
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoperiod", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_period", e1, e2)
          })

## --
##' @rdname nanoperiod
##' @export
setMethod("+", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoperiod", "nanoduration"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanotime", "nanoperiod"),
          function(e1, e2) {
              stop(paste0("binary '+' is not defined for \"nanotime\" and \"nanoperiod\" ",
                          "objects; instead use \"plus(e1, e2, tz)\""))
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, as.integer64(e2))
          })

## --
##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("nanoduration", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("integer64", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

##' @rdname nanoperiod
##' @export
setMethod("+", c("numeric", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, as.integer64(e1))
          })

## ----------- `*`

##' @rdname nanoperiod
##' @export
setMethod("*", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @export
setMethod("*", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @export
setMethod("*", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
##' @export
setMethod("*", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("multiplies_period_double", e1, e2)
          })

##' @rdname nanoperiod
##' @export
setMethod("*", c("integer64", "nanoperiod"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e2, e1)
          })

##' @rdname nanoperiod
##' @export
setMethod("*", c("numeric", "nanoperiod"),
          function(e1, e2) {
              .Call("multiplies_period_double", e2, e1)
          })

## ----------- `/`

##' @rdname nanoperiod
##' @export
setMethod("/", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @export
setMethod("/", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @export
setMethod("/", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("divides_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
##' @export
setMethod("/", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("divides_period_double", e1, e2)
          })

## Compare
## -------

##' @rdname nanoperiod
##' @export
setMethod("Compare", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("operation not defined for \"nanoperiod\" objects")
          })
##' @rdname nanoperiod
##' @export
setMethod("Compare", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"nanoperiod\" objects")
          })

##' @rdname nanoperiod
##' @export
setMethod("==", c("nanoperiod", "nanoperiod"), function(e1, e2) .Call("eq_period_period", e1, e2))

##' @rdname nanoperiod
##' @export
setMethod("!=", c("nanoperiod", "nanoperiod"), function(e1, e2) .Call("ne_period_period", e1, e2))

setMethod("all.equal",
          c("nanoperiod"),
          function(target, current, ..., tolerance=0) {
              callNextMethod()
          })


## ---------- plus/minus ops with nanotime and nanoperiod (which require 'tz')

setGeneric("plus",  function(e1, e2, tz) standardGeneric("plus"))
setGeneric("minus", function(e1, e2, tz) standardGeneric("minus"))


setMethod("plus", c("nanotime", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanotime_period", e1, e2, tz) 
          })

setMethod("plus", c("nanoperiod", "nanotime", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanotime_period", e2, e1, tz) 
          })


setMethod("minus", c("nanotime", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("minus_nanotime_period", e1, e2, tz) 
          })

setMethod("minus", c("nanoperiod", "nanotime", "character"),
          function(e1, e2, tz) {
            stop("operation not defined for \"nanoperiod\" objects")
          })


## ---------- plus/minus ops with nanoival and nanoperiod (which require 'tz')


setMethod("plus", c("nanoival", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanoival_period", e1, e2, tz) 
          })

setMethod("plus", c("nanoperiod", "nanoival", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanoival_period", e2, e1, tz) 
          })

setMethod("minus", c("nanoival", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("minus_nanoival_period", e1, e2, tz) 
          })

setMethod("minus", c("nanoperiod", "nanoival", "character"),
          function(e1, e2, tz) {
            stop("operation not defined for \"nanoperiod\" objects")
          })


NA_nanoperiod_ <- new("nanoperiod", complex(1, -1.0609978954826362e-314, 0))

