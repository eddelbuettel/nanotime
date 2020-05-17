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
        if (!is.numeric(months)) {
            stop("argument 'months' must be numeric")
        }
        if (!is.numeric(days)) {
            stop("argument 'days' must be numeric")
        }        
        period_from_parts_impl(as.integer(months), as.integer(days), as.nanoduration(duration))
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
              period_from_string_impl(x)
          })

setAs("character", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "integer64",
          function(x) {
              period_from_integer64_impl(x)
          })

setAs("integer64", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "numeric",
          function(x) {
              period_from_double_impl(x)
          })

setAs("numeric", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "integer",
          function(x) {
              period_from_integer_impl(x)
          })

setAs("integer", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "nanoduration",
          function(x) {
              period_from_integer64_impl(x)
          })

setAs("nanoduration", "nanoperiod", function(from) as.nanoperiod(from))

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "NULL",
          function(x) {
              period_from_string_impl(character())
          })

##' @rdname nanoperiod
setMethod("as.nanoperiod",
          "missing",
          function(x) {
              period_from_string_impl(character())
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
                  print(period_to_string_impl(x), quote=quote)
              }
          })

##' @rdname nanoperiod
format.nanoperiod <- function(x, ...) {
  period_to_string_impl(x)
}

##' @rdname nanoperiod
setMethod("as.character",
          signature("nanoperiod"),
          function(x) {
              period_to_string_impl(x)
          })

##' @rdname nanoperiod
setMethod("is.na",
          "nanoperiod",
          function(x) {
              period_isna_impl(x)
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
          signature("nanoperiod", "numeric"),
          function (x, i, j, ..., drop=FALSE) {
              if (!missing(j) || length(list(...)) > 0) {
                  warning("unused indices or arguments in 'nanoperiod' subsetting")
              }
              if (isTRUE(any(i < 0))) {
                  new("nanoperiod", unclass(x)[i])
              } else {
                  period_subset_numeric_impl(x, i)
              }
          })

##' @rdname nanoperiod
setMethod("[",
          signature("nanoperiod", "logical"),
          function (x, i, j, ..., drop=FALSE) {
              if (!missing(j) || length(list(...)) > 0) {
                  warning("unused indices or arguments in 'nanoperiod' subsetting")
              }
              period_subset_logical_impl(x, i)
          })

##' @rdname nanoperiod
setMethod("[",
          signature("nanoperiod", "character"),
          function (x, i, j, ..., drop=FALSE) {
              ## we don't implement 'period_subset_character_impl' but
              ## do the gymnastic of finding the NAs here at R level;
              ## it's not as efficient, but using 'character' indexing
              ## is by itself inefficient, so the overhead should have
              ## no practical consequences.
              if (!missing(j) || length(list(...)) > 0) {
                  warning("unused indices or arguments in 'nanoperiod' subsetting")
              }
              na_index <- !(i %in% names(x)) | is.na(i)
              res <- new("nanoperiod", unclass(x)[i])
              res[na_index] <- NA_nanoperiod_
              res
          })

##' @rdname nanoperiod
setMethod("[",
          signature("nanoperiod", "ANY"),
          function (x, i, j, ..., drop=FALSE) {
              stop("']' not defined on 'nanoperiod' for index of type 'ANY'")
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
              period_month_impl(x)
          })

##' @noRd
setGeneric("nanoperiod.day", function(x) standardGeneric("nanoperiod.day"))
##' @rdname nanoperiod.month
setMethod("nanoperiod.day",
          "nanoperiod",
          function(x) {
              period_day_impl(x)
          })

##' @noRd
setGeneric("nanoperiod.nanoduration", function(x) standardGeneric("nanoperiod.nanoduration"))
##' @rdname nanoperiod.month
setMethod("nanoperiod.nanoduration",
          "nanoperiod",
          function(x) {
              period_duration_impl(x)
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
                  minus_period_impl(e1)
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname nanoperiod
setMethod("-", c("nanoperiod", "nanoperiod"),
          function(e1, e2) {
              minus_period_period_impl(e1, e2)
          })

## --

##' @rdname nanoperiod
##' @aliases -,nanoperiod,ANY-method
setMethod("-", c("nanoperiod", "nanoduration"),
          function(e1, e2) {
              minus_period_integer64_impl(e1, e2)
          })

##' @rdname nanoperiod
setMethod("-", c("nanoperiod", "integer64"),
          function(e1, e2) {
              minus_period_integer64_impl(e1, e2)
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
              minus_period_integer64_impl(e1, as.integer64(e2))
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
              minus_integer64_period_impl(e1, e2)
          })

## --
##' @rdname nanoperiod
setMethod("-", c("integer64", "nanoperiod"),
          function(e1, e2) {
              minus_integer64_period_impl(as.integer64(e1), e2)
          })

## --
##' @rdname nanoperiod
setMethod("-", c("numeric", "nanoperiod"),
          function(e1, e2) {
              minus_integer64_period_impl(as.integer64(e1), e2)
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
              plus_period_period_impl(e1, e2)
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
              plus_period_integer64_impl(e1, S3Part(e2, strictS3=TRUE))
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "integer64"),
          function(e1, e2) {
              plus_period_integer64_impl(e1, e2)
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "nanotime"),
          function(e1, e2) {
              stop(paste0("binary '+' is not defined for 'nanoperiod' and 'nanotime' objects; instead use 'plus(e1, e2, tz)'"))
          })

##' @rdname nanoperiod
setMethod("+", c("nanoival", "nanoperiod"),
          function(e1, e2) {
              stop(paste0("binary '+' is not defined for 'nanoival' and 'nanoperiod' objects; instead use 'plus(e1, e2, tz)'"))
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "nanoival"),
          function(e1, e2) {
              stop(paste0("binary '+' is not defined for 'nanoperiod' and 'nanoival' objects; instead use 'plus(e1, e2, tz)'"))
          })

##' @rdname nanoperiod
setMethod("+", c("nanotime", "nanoperiod"),
          function(e1, e2) {
              stop(paste0("binary '+' is not defined for 'nanotime' and 'nanoperiod' objects; instead use 'plus(e1, e2, tz)'"))
          })

##' @rdname nanoperiod
setMethod("+", c("nanoperiod", "numeric"),
          function(e1, e2) {
              plus_period_integer64_impl(e1, as.integer64(e2))
          })

## --
##' @rdname nanoperiod
setMethod("+", c("nanoduration", "nanoperiod"),
          function(e1, e2) {
              plus_period_integer64_impl(e2, e1)
          })

##' @rdname nanoperiod
setMethod("+", c("integer64", "nanoperiod"),
          function(e1, e2) {
              plus_period_integer64_impl(e2, e1)
          })

##' @rdname nanoperiod
setMethod("+", c("numeric", "nanoperiod"),
          function(e1, e2) {
              plus_period_integer64_impl(e2, as.integer64(e1))
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
              multiplies_period_integer64_impl(e1, e2)
          })

##' @rdname nanoperiod
setMethod("*", c("nanoperiod", "numeric"),
          function(e1, e2) {
              multiplies_period_double_impl(e1, e2)
          })

##' @rdname nanoperiod
setMethod("*", c("integer64", "nanoperiod"),
          function(e1, e2) {
              multiplies_period_integer64_impl(e2, e1)
          })

##' @rdname nanoperiod
setMethod("*", c("numeric", "nanoperiod"),
          function(e1, e2) {
              multiplies_period_double_impl(e2, e1)
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
              divides_period_integer64_impl(e1, e2)
          })

##' @rdname nanoperiod
setMethod("/", c("nanoperiod", "numeric"),
          function(e1, e2) {
              divides_period_double_impl(e1, e2)
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
setMethod("==", c("nanoperiod", "nanoperiod"), function(e1, e2) eq_period_period_impl(e1, e2))

##' @rdname nanoperiod
setMethod("!=", c("nanoperiod", "nanoperiod"), function(e1, e2) ne_period_period_impl(e1, e2))


## ---------- plus/minus ops with nanotime and nanoperiod (which require 'tz')

##' @noRd
setGeneric("plus",  function(e1, e2, tz) standardGeneric("plus"))
##' @noRd
setGeneric("minus", function(e1, e2, tz) standardGeneric("minus"))


##' @rdname nanoperiod
##' @aliases plus
setMethod("plus", c("nanotime", "nanoperiod", "character"),
          function(e1, e2, tz) {
            plus_nanotime_period_impl(e1, e2, tz)
          })

##' @rdname nanoperiod
setMethod("plus", c("nanoperiod", "nanotime", "character"),
          function(e1, e2, tz) {
            plus_nanotime_period_impl(e2, e1, tz)
          })


##' @rdname nanoperiod
##' @aliases minus
setMethod("minus", c("nanotime", "nanoperiod", "character"),
          function(e1, e2, tz) {
            minus_nanotime_period_impl(e1, e2, tz)
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
            plus_nanoival_period_impl(e1, e2, tz)
          })

##' @rdname nanoperiod
setMethod("plus", c("nanoperiod", "nanoival", "character"),
          function(e1, e2, tz) {
            plus_nanoival_period_impl(e2, e1, tz)
          })

##' @rdname nanoperiod
setMethod("minus", c("nanoival", "nanoperiod", "character"),
          function(e1, e2, tz) {
            minus_nanoival_period_impl(e1, e2, tz)
          })

##' @rdname nanoperiod
##' @noRd
setMethod("minus", c("nanoperiod", "nanoival", "character"),
          function(e1, e2, tz) {
            stop("operation not defined for 'nanoperiod' objects")
          })


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
##' @param ... further arguments for different methods
##' @param check.attributes logical indicating if the
##'     \code{attributes} of \code{target} and \code{current} (other than
##'     the names) should be compared.
##'
##' @seealso \code{\link{identical}}, \code{\link{isTRUE}},
##'     \code{\link{==}}, and \code{\link{all}} for exact equality
##'     testing.
##'
##' @method all.equal nanoperiod
##'
all.equal.nanoperiod <-
    function(target, current, ..., check.attributes = TRUE) all.equal.raw(target, current, ..., check.attributes)


##' @rdname all.equal.nanoperiod
setMethod("all.equal", c(target = "nanoperiod", current="ANY"), all.equal.nanoperiod)


##' @rdname nanoperiod
NA_nanoperiod_ <- new("nanoperiod", complex(1, -1.0609978954826362e-314, 0))


##' @rdname rounding
##' @param origin a \code{nanotime} scalar indicating the origin at which the rounding is considered
##' @param tz a \code{character} scalar indicating the time zone in which to conduct the rounding
setMethod("nano_ceiling", c(x="nanotime", precision="nanoperiod"),
          function(x, precision, origin=nanotime(), tz) {
              if (class(origin) != "nanotime") {
                  stop("'origin' must be of class 'nanotime'")
              }
              if (!is.character(tz)) {
                  stop("'tz' must be of type 'character'")
              }
              if (is.unsorted(x)) {
                  stop("'x' must be sorted")
              }
              ceiling_tz_impl(x, precision, origin, tz)
          })

##' @rdname rounding
setMethod("nano_floor",   c(x="nanotime", precision="nanoperiod"),
          function(x, precision, origin=nanotime(), tz) {
              if (class(origin) != "nanotime") {
                  stop("'origin' must be of class 'nanotime'")
              }
              if (!is.character(tz)) {
                  stop("'tz' must be of type 'character'")
              }
              if (is.unsorted(x)) {
                  stop("'x' must be sorted")
              }
              floor_tz_impl(x, precision, origin, tz)
          })


##' Replicate Elements
##'
##' Replicates the values in 'x' similarly to the default method.
##'
##' @param x a vector of \code{nanoperiod}
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
setMethod("rep", c(x = "nanoperiod"), function(x, ...) {
    new("nanoperiod", callNextMethod())
})
