##' @rdname nanoperiod
setClass("nanoperiod", contains = "complex")

##' Period type with nanosecond precision
##'
##' \code{nanoperiod} is a length of time type (implemented as an S4
##' class) with nanosecond precision. It differs from
##' \code{nanoduration} because it is capable of representing calendar
##' months and days. It can thus represent years (12 months) and weeks
##' (7 days). A period is a somewhat abstract representation of time:
##' it is only when anchored to a point in time and in a specific time
##' zone that it is possible to convert it to a specific
##' duration. This means that many of the operations involving periods
##' need the additional argument \code{tz}.
##'
##' @section Constructors:
##' 
##' The true constructor is 
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
##' @param x,value An object of class \code{nanoperiod}
##' @param ... further arguments
##' @param e1 Operand of class \code{nanoperiod}
##' @param e2 Operand of class \code{nanoperiod}
##' @param object argument for method \code{show}
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @param months Used in the constructor to indicate the number of
##'     months of the \code{nanoperiod}
##' @param days Used in the constructor to indicate the number of
##'     days of the \code{nanoperiod}
##' @param duration Used in the constructor to indicate the duration
##'     component of the \code{nanoperiod}
##' @param quote indicates if the output of \code{print} should be
##'     quoted
##' @param tz \code{character} indicating a timezone
##' @author Dirk Eddelbuettel
##' @author Leonardo Silvestri
##' @examples
##' p <- nanoperiod(months=12, days=7, duration="01:00:00")
##' print(p)
##'
##' #  when adding a \code{nanoperiod} to a \code{nanotime} or to a
##' # \code{nanoival}, a time zone must be specified:
##' y <- nanotime("1970-01-01T00:00:00+00:00")
##' plus(y, p, tz="America/Chicago")
##'
##' @seealso \code{\link{nanotime}}, \code{\link{nanoduration}},
##'     \code{\link{nanoival}}, \code{\link{nanoperiod.month,nanoperiod-method}}
##'
##' @aliases Compare,ANY,nanoperiod-method Compare,nanoperiod,ANY-method
##' @aliases -,ANY,nanoperiod-method -,nanoperiod,nanotime-method
##' @aliases -,nanotime,nanoperiod-method
##' @aliases /,ANY,nanoperiod-method /,nanoperiod,ANY-method
##' @aliases Complex,nanoperiod-method Math,nanoperiod-method Math2,nanoperiod-method
##' @aliases Summary,nanoperiod-method
##' @aliases minus,nanoperiod,nanoival,character-method
##' 
##' @rdname nanoperiod
nanoperiod <- function(months=0, days=0, duration=as.nanoduration(0)) {
    if (nargs() == 0) {
        as.nanoperiod(NULL)
    } else {
        as.nanoperiod(paste0(as.integer64(months), "m", as.integer64(days), "d", "/", as.nanoduration(duration)))
    }
}

##' @noRd
setGeneric("nanoperiod")


##' @noRd
setGeneric("as.nanoperiod", function(x) standardGeneric("as.nanoperiod"))

##' @rdname nanoperiod
##' @aliases as.nanoperiod
setMethod("as.nanoperiod",
          "character",
          function(x) {
              new("nanoperiod", .Call('period_from_string', x))
          })

setAs("character", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "integer64",
          function(x) {
              new("nanoperiod", .Call('period_from_integer64', x))
          })

setAs("integer64", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "numeric",
          function(x) {
              new("nanoperiod", .Call('period_from_double', x))
          })

setAs("numeric", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "integer",
          function(x) {
              new("nanoperiod", .Call('period_from_integer', x))
          })

setAs("integer", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "nanoduration",
          function(x) {
              new("nanoperiod", .Call('period_from_integer64', x))
          })

setAs("nanoduration", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "NULL",
          function(x) {
              new("nanoperiod", .Call('period_from_string', character()))
          })

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "missing",
          function(x) {
              new("nanoperiod", .Call('period_from_string', character()))
          })


##' @rdname nanoperiod
setMethod("show",
          signature("nanoperiod"),
          function(object) print(object))

##' @rdname nanoperiod
setMethod("print",
          "nanoperiod",
          function(x, quote=FALSE, ...) {
              if (length(x)==0) {
                  print("nanoperiod(0)", quote=quote)
              } else {
                  print(.Call('period_to_string', x), quote=quote)
              }
          })

##' @rdname nanoperiod
format.nanoperiod <- function(x, ...) {
  .Call('period_to_string', x)
}

##' @rdname nanoperiod
setMethod("as.character",
          signature("nanoperiod"),
          function(x) {
              .Call('period_to_string', x)
          })

##' @rdname nanoperiod
setMethod("is.na",
          "nanoperiod",
          function(x) {
              .Call("period_isna", x)
          })

##' @rdname nanoperiod
setMethod("is.na<-",
          "nanoperiod",
          function(x, value) {
              x[value] <- NA_nanoperiod_
              x
          })

## accessors

## ----------- non ops

##' @rdname nanoperiod
setMethod("[[",
          signature("nanoperiod", "ANY"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoperiod", callNextMethod())
          })

##' @rdname nanoperiod
setMethod("[",
          signature("nanoperiod", "ANY"),
          function (x, i, j, ..., drop=FALSE) {
              new("nanoperiod", callNextMethod())
          })

##' @rdname nanoperiod
setMethod("[<-",
          signature("nanoperiod", "ANY", "ANY", "ANY"),
          function (x, i, j, ..., value) {
              new("nanoperiod", callNextMethod())
          })

##' @rdname nanoperiod
c.nanoperiod <- function(...) {
    args <- list(...)
    s3args <- lapply(args, function (x) S3Part(x, strictS3=TRUE))
    res <- do.call(c, s3args)
    names(res) <- names(args)
    new("nanoperiod", res)
}

##' @noRd
setGeneric("nanoperiod.month", function(x) standardGeneric("nanoperiod.month"))

##' Nanoperiod accessors
##' 
##' These functions allow access to the components of a \code{nanoperiod}
##' 
##' @rdname nanoperiod.month
##' @aliases nanoperiod.month nanoperiod.day nanoperiod.nanoduration
##' @param x A \code{nanoperiod}
##' @return \code{nanoperiod.month} and \code{nanoperiod.day} return
##'     an \code{integer64} whereas \code{nanoperiod.nanoduration}
##'     returns a \code{nanoduration}
##' @examples
##' p <- as.nanoperiod("2y1m1d/12:00:00")
##' nanoperiod.month(p)
##' nanoperiod.day(p)
##' nanoperiod.nanoduration(p)
##' @author Dirk Eddelbuettel
##' @author Leonardo Silvestri
##' @seealso \code{\link{nanoduration}}
setMethod("nanoperiod.month",
          "nanoperiod",
          function(x) {
              .Call('period_month', x)
          })

##' @noRd
setGeneric("nanoperiod.day", function(x) standardGeneric("nanoperiod.day"))
##' @rdname nanoperiod.month
setMethod("nanoperiod.day",
          "nanoperiod",
          function(x) {
              .Call('period_day', x)
          })

##' @noRd
setGeneric("nanoperiod.nanoduration", function(x) standardGeneric("nanoperiod.nanoduration"))
##' @rdname nanoperiod.month
setMethod("nanoperiod.nanoduration",
          "nanoperiod",
          function(x) {
              .Call('period_duration', x)
          })

##' @rdname nanoperiod
setMethod("names",
          signature("nanoperiod"),
          function(x) {
              callNextMethod()
          })

##' @rdname nanoperiod
setMethod("names<-",
          signature("nanoperiod"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- value
              new("nanoperiod", x)
          })


## ----------- make sure ops that don't make sense error out
##' @noRd
setMethod("Ops", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("operation not defined for 'nanoperiod' objects")
          })
##' @noRd
setMethod("Ops", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("operation not defined for 'nanoperiod' objects")
          })

##' @noRd
setMethod("Math", c("nanoperiod"),
          function(x) {
              stop("operation not defined for 'nanoperiod' objects")
          })

##' @noRd
setMethod("Math2", c("nanoperiod"),
          function(x, digits) {
              stop("operation not defined for 'nanoperiod' objects")
          })

##' @noRd
setMethod("Summary", c("nanoperiod"),
          function(x, ..., na.rm = FALSE) {
              stop("invalid 'type' (nanoperiod) of argument")
          })

##' @noRd
setMethod("Complex", c("nanoperiod"),
          function(z) {
              stop("operation not defined for 'nanoperiod' objects")
          })

## ------------ `-`
##' @rdname nanoperiod
setMethod("-", c("nanoperiod", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  .Call("minus_period", e1)
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname nanoperiod
setMethod("-", c("nanoperiod", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_period_period", e1, e2)
          })

## --

##' @rdname nanoperiod
##' @aliases -,nanoperiod,ANY-method
setMethod("-", c("nanoperiod", "nanoduration"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
setMethod("-", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e2)
          })

##' @noRd
setMethod("-", c("nanotime", "nanoperiod"),
          function(e1, e2) {
              stop(paste0("binary '-' is not defined for 'nanotime' and 'nanoperiod' objects; instead use 'minus(e1, e2, tz)'"))
          })

##' @noRd
setMethod("-", c("nanoperiod", "nanotime"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
setMethod("-", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, as.integer64(e2))
          })

## --
##' @noRd
setMethod("-", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## --
##' @rdname nanoperiod
setMethod("-", c("nanoduration", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_integer64_period", e1, e2)
          })

## --
##' @rdname nanoperiod
setMethod("-", c("integer64", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_integer64_period", as.integer64(e1), e2)
          })

## --
##' @rdname nanoperiod
setMethod("-", c("numeric", "nanoperiod"),
          function(e1, e2) {
              .Call("minus_integer64_period", as.integer64(e1), e2)
          })


## ----------- `+`

##' @rdname nanoperiod
##' @aliases +,ANY,nanoperiod-method
setMethod("+", c("nanoperiod", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  e1
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_period", e1, e2)
          })

## --
##' @noRd
setMethod("+", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "nanoduration"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "nanotime"),
          function(e1, e2) {
              stop(paste0("binary '+' is not defined for 'nanoperiod' and 'nanotime' objects; instead use 'plus(e1, e2, tz)'"))
          })

##' @rdname nanoperiod
setMethod("+", c("nanotime", "nanoperiod"),
          function(e1, e2) {
              stop(paste0("binary '+' is not defined for 'nanotime' and 'nanoperiod' objects; instead use 'plus(e1, e2, tz)'"))
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, as.integer64(e2))
          })

## --
##' @rdname nanoperiod
setMethod("+", c("nanoduration", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

##' @rdname nanoperiod
setMethod("+", c("integer64", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

##' @rdname nanoperiod
setMethod("+", c("numeric", "nanoperiod"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, as.integer64(e1))
          })

## ----------- `*`

##' @noRd
setMethod("*", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @noRd
setMethod("*", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @aliases *,nanoperiod,ANY-method *,ANY,nanoperiod-method
setMethod("*", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
setMethod("*", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("multiplies_period_double", e1, e2)
          })

##' @rdname nanoperiod
setMethod("*", c("integer64", "nanoperiod"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e2, e1)
          })

##' @rdname nanoperiod
setMethod("*", c("numeric", "nanoperiod"),
          function(e1, e2) {
              .Call("multiplies_period_double", e2, e1)
          })

## ----------- `/`

##' @rdname nanoperiod
##' @noRd
setMethod("/", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
##' @noRd
setMethod("/", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname nanoperiod
setMethod("/", c("nanoperiod", "integer64"),
          function(e1, e2) {
              .Call("divides_period_integer64", e1, e2)
          })

##' @rdname nanoperiod
setMethod("/", c("nanoperiod", "numeric"),
          function(e1, e2) {
              .Call("divides_period_double", e1, e2)
          })

## Compare
## -------

##' @noRd
setMethod("Compare", c("ANY", "nanoperiod"),
          function(e1, e2) {
              stop("operation not defined for 'nanoperiod' objects")
          })
##' @noRd
setMethod("Compare", c("nanoperiod", "ANY"),
          function(e1, e2) {
              stop("operation not defined for 'nanoperiod' objects")
          })

##' @rdname nanoperiod
setMethod("==", c("nanoperiod", "nanoperiod"), function(e1, e2) .Call("eq_period_period", e1, e2))

##' @rdname nanoperiod
setMethod("!=", c("nanoperiod", "nanoperiod"), function(e1, e2) .Call("ne_period_period", e1, e2))

##' Test if Two Objects are (Nearly) Equal
##'
##' Compare \code{target} and \code{current} testing \sQuote{near
##' equality}.  If they are different, comparison is still made to
##' some extent, and a report of the differences is returned.  Do not
##' use \code{all.equal} directly in \code{if} expressions---either
##' use \code{isTRUE(all.equal(....))} or \code{\link{identical}} if
##' appropriate.
##' 
##' @param target,current \code{nanoperiod} arguments to be compared
##'
##' @seealso \code{\link{identical}}, \code{\link{isTRUE}},
##'     \code{\link{==}}, and \code{\link{all}} for exact equality
##'     testing.
##' 
##' @method all.equal nanoperiod
##' 
setMethod("all.equal",
          c("nanoperiod"),
          function(target, current) {
              callNextMethod(target, current, tolerance=0)
          })


## ---------- plus/minus ops with nanotime and nanoperiod (which require 'tz')

##' @noRd
setGeneric("plus",  function(e1, e2, tz) standardGeneric("plus"))
##' @noRd
setGeneric("minus", function(e1, e2, tz) standardGeneric("minus"))


##' @rdname nanoperiod
##' @aliases plus
setMethod("plus", c("nanotime", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanotime_period", e1, e2, tz) 
          })

##' @rdname nanoperiod
setMethod("plus", c("nanoperiod", "nanotime", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanotime_period", e2, e1, tz) 
          })


##' @rdname nanoperiod
##' @aliases minus
setMethod("minus", c("nanotime", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("minus_nanotime_period", e1, e2, tz) 
          })

##' @rdname nanoperiod
setMethod("minus", c("nanoperiod", "nanotime", "character"),
          function(e1, e2, tz) {
            stop("operation not defined for 'nanoperiod' objects")
          })


## ---------- plus/minus ops with nanoival and nanoperiod (which require 'tz')


##' @rdname nanoperiod
setMethod("plus", c("nanoival", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanoival_period", e1, e2, tz) 
          })

##' @rdname nanoperiod
setMethod("plus", c("nanoperiod", "nanoival", "character"),
          function(e1, e2, tz) {
            .Call("plus_nanoival_period", e2, e1, tz) 
          })

##' @rdname nanoperiod
setMethod("minus", c("nanoival", "nanoperiod", "character"),
          function(e1, e2, tz) {
            .Call("minus_nanoival_period", e1, e2, tz) 
          })

##' @rdname nanoperiod
##' @noRd
setMethod("minus", c("nanoperiod", "nanoival", "character"),
          function(e1, e2, tz) {
            stop("operation not defined for 'nanoperiod' objects")
          })


##' @rdname nanoperiod
NA_nanoperiod_ <- new("nanoperiod", complex(1, -1.0609978954826362e-314, 0))

