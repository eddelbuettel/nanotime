##' @rdname period
##' @export
setClass("period", contains = "integer64")

##' Period type with nanosecond precision
##'
##' \code{period} is a length of time type (implemented as an S4
##' class) with nanosecond precision. It differs from \code{duration}
##' because it is capable of representing calendar months and days. It
##' can of course also reprensent years (immutably 12 months) and
##' weeks (immutably 7 days). 
##'
##' @section Output Format:
##'
##' A \code{period} is displayed as months, days, and \code{duration}
##' like this: \code{10m2d/10:12:34.123_453_000}.
##'
##' @section Details:
##'
##' 
##' 
##' Adding or subtracting \code{period} and \code{nanotime} require a
##' timezone as third argument. For this reason it is not possible to
##' use the binary operator `code{+}`. Instead the functions
##' `\code{plus}` and `\code{minus}` are defined.
##'
##' 
##' @param x The object which want to convert to class \code{period}
##' @param ... further arguments passed to or from methods.
##' @param e1 Operand of class \code{period}
##' @param e2 Operand of class \code{period}
##' @param object argument for method \code{show}
##' @param i index specifying elements to extract or replace.
##' @param j Required for \code{[} signature but ignored here
##' @param drop Required for \code{[} signature but ignored here
##' @author Leonardo Silvestri
##' @examples
##' x <- period(months=12, days=7, duration="01:00:00")
##' print(x)
##' y <- nanotime("1970-01-01T00:00:00.000000001+00:00")
##' plus(y, x, tz="America/Chicago")
##' 


##' @rdname period
##' @export
period <- function(months, days, duration=as.duration(0)) {
    as.period(paste0(as.integer64(months), "m", as.integer64(days), "d", "/", as.duration(duration)))
}

setGeneric("period")


setGeneric("as.period", function(x) standardGeneric("as.period"))

##' @rdname period
##' @export
setMethod("as.period",
          "character",
          function(x) {
              res <- .Call('period_from_string', x)
              oldClass(res) <- "integer64"
              new("period", res)
          })

##' @rdname period
##' @export
setMethod("as.period",
          "integer64",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), x)))
          })

##' @rdname period
##' @export
setMethod("as.period",
          "numeric",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), as.integer64(x))))
          })

##' @rdname period
##' @export
setMethod("as.period",
          "integer",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), as.integer64(x))))
          })

##' @rdname period
##' @export
setMethod("as.period",
          "duration",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), as.integer64(x))))
          })

##' @rdname period
##' @export
setMethod("show",
          signature("period"),
          function(object) print(object))

##' @rdname period
##' @export
setMethod("print",
          "period",
          function(x, ...) {
              print(.Call('period_to_string', x))
          })

##' @rdname period
##' @export
setMethod("as.character",
          signature("period"),
          function(x) {
              .Call('period_to_string', x)
          })

## accessors

## ----------- non ops

##' @rdname period
##' @export
setMethod("[",
          signature("period", "logical"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("period", x[rep(i, each=2)])
          })

##' @rdname period
##' @export
setMethod("[",
          signature("period", "numeric"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              i <- (i-1)*2 + 1
              i <- sapply(i, function(k) c(k, k+1))
              new("period", x[i])
          })
          
##' @rdname period
##' @export
setMethod("[",
          signature("period", "character"),
          function (x, i, j, ..., drop=FALSE) {
              idx <- sapply(i,
                            function(k) {
                                res <- grep(k, names(x))
                                if (length(res)==0) NA
                                else head(res, 1)
                            })
              idx[idx==integer(0)] <- NA
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("period", x[c(sapply(idx, function(x) c((x-1)*2+1, x*2)))])
          })

##' @rdname period
##' @export
setMethod("[<-",
          signature("period", "logical", "ANY", "period"),
          function (x, i, j, ..., value) {
              x <- S3Part(x, strictS3=TRUE)
              x[rep(i, each=2)] <- S3Part(value, strictS3=TRUE)
              new("period", x)
          })

##' @rdname period
##' @export
setMethod("[<-",
          signature("period", "numeric", "ANY", "period"),
          function (x, i, j, ..., value) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              i <- (i-1)*2 + 1
              i <- sapply(i, function(k) c(k, k+1))
              x[i] <- S3Part(value, strictS3=TRUE)
              new("period", x)
          })

##' @rdname period
##' @export
setMethod("[<-",
          signature("period", "character", "ANY", "period"),
          function (x, i, j, ..., value) {
              idx <- sapply(i,
                            function(k) {
                                res <- grep(k, names(x))
                                if (length(res)==0) NA
                                else head(res, 1)
                            })
              idx[idx==integer(0)] <- NA
              x[c(sapply(idx, function(x) c((x-1)*2+1, x*2)))] <- S3Part(value, strictS3=TRUE)
              new("period", x)
          })

##' @rdname period
##' @export
c.period <- function(...) {
    res <- do.call(c.integer64, list(...))
    names <- names(res)
    if (!is.null(names)) {
        names(res) <- substr(names, 1, nchar(names)-1)
    }
    new("period", res)
}

setGeneric("period.month", function(x) standardGeneric("period.month"))
setMethod("period.month",
          "period",
          function(x) {
              .Call('period_month', x)
          })

setGeneric("period.day", function(x) standardGeneric("period.day"))
setMethod("period.day",
          "period",
          function(x) {
              .Call('period_day', x)
          })

setGeneric("period.duration", function(x) standardGeneric("period.duration"))
setMethod("period.duration",
          "period",
          function(x) {
              .Call('period_duration', x)
          })

##' @rdname period
##' @export
setMethod("names",
          signature("period"),
          function(x) {
              callNextMethod()[c(TRUE, FALSE)]
          })

##' @rdname period
##' @export
setMethod("names<-",
          signature("period"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- rep(value, each=2)
              new("period", x)
          })


## ----------- make sure ops that don't make sense error out
##' @rdname period
##' @export
setMethod("Ops", c("period", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })
##' @rdname period
##' @export
setMethod("Ops", c("ANY", "period"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })

##' @rdname period
##' @export
setMethod("Math", c("period"),
          function(x) {
              stop("operation not defined for \"period\" objects")
          })

##' @rdname period
##' @export
setMethod("Math2", c("period"),
          function(x, digits) {
              stop("operation not defined for \"period\" objects")
          })

##' @rdname period
##' @export
setMethod("Summary", c("period"),
          function(x, ..., na.rm = FALSE) {
              stop("invalid 'type' (period) of argument")
          })

##' @rdname period
##' @export
setMethod("Complex", c("period"),
          function(z) {
              stop("operation not defined for \"period\" objects")
          })

##' @rdname period
##' @export
setMethod("min", c("period"),
          function(x, ..., na.rm = FALSE) {
              stop("operation not defined for \"period\" objects")
          })

##' @rdname period
##' @export
setMethod("max", c("period"),
          function(x, ..., na.rm = FALSE) {
              stop("operation not defined for \"period\" objects")
          })

##' @rdname period
##' @export
setMethod("range", c("period"),
          function(x, ..., na.rm = FALSE) {
              stop("operation not defined for \"period\" objects")
          })

## ------------ `-`
##' @rdname period
##' @export
setMethod("-", c("period", "ANY"),
          function(e1, e2) {
              print("entering - period ANY")
              if (missing(e2)) {
                  ## incorrect LLL
                  new("period", -S3Part(e1, strictS3=TRUE))
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname period
##' @export
setMethod("-", c("period", "period"),
          function(e1, e2) {
              .Call("minus_period_period", e1, e2)
          })

## --
##' @rdname period
##' @export
setMethod("-", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("-", c("period", "duration"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e1)
          })

##' @rdname period
##' @export
setMethod("-", c("period", "integer64"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e2)
          })

## setMethod("-", c("period", "integer"),
##           function(e1, e2) {
##               .Call("minus_period_integer64", e1, as.integer64(e2))
##           })

##' @rdname period
##' @export
setMethod("-", c("period", "numeric"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, as.integer64(e2))
          })

## --
##' @rdname period
##' @export
setMethod("-", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("-", c("duration", "period"),
          function(e1, e2) {
              .Call("minus_integer64_period", e1, e2)
          })

setMethod("-", c("integer64", "period"),
          function(e1, e2) {
              .Call("minus_integer64_period", as.integer64(e1), e2)
          })

## setMethod("-", c("integer", "period"),
##           function(e1, e2) {
##               .Call("minus_integer64_period", as.integer64(e1), e2)
##           })

setMethod("-", c("numeric", "period"),
          function(e1, e2) {
              .Call("minus_integer64_period", as.integer64(e1), e2)
          })


## ----------- `+`

##' @rdname period
##' @export
setMethod("+", c("period", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  e2
              }
              else {
                  stop("invalid operand types")
              }
          })

##' @rdname period
##' @export
setMethod("+", c("period", "period"),
          function(e1, e2) {
              .Call("plus_period_period", e1, e2)
          })

## --
##' @rdname period
##' @export
setMethod("+", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("+", c("period", "duration"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, S3Part(e2, strictS3=TRUE))
          })

##' @rdname period
##' @export
setMethod("+", c("period", "integer64"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, e2)
          })

##' @rdname period
##' @export
setMethod("+", c("period", "numeric"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, as.integer64(e2))
          })

## --
##' @rdname period
##' @export
setMethod("+", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("+", c("duration", "period"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

##' @rdname period
##' @export
setMethod("+", c("integer64", "period"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

##' @rdname period
##' @export
setMethod("+", c("numeric", "period"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, as.integer64(e1))
          })

## ----------- `*`

##' @rdname period
##' @export
setMethod("*", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("*", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("*", c("period", "integer64"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e1, e2)
          })

##' @rdname period
##' @export
setMethod("*", c("period", "numeric"),
          function(e1, e2) {
              .Call("multiplies_period_double", e1, e2)
          })

##' @rdname period
##' @export
setMethod("*", c("integer64", "period"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e2, e1)
          })

##' @rdname period
##' @export
setMethod("*", c("numeric", "period"),
          function(e1, e2) {
              .Call("multiplies_period_double", e2, e1)
          })

## ----------- `/`

##' @rdname period
##' @export
setMethod("/", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("/", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("/", c("period", "integer64"),
          function(e1, e2) {
              .Call("divides_period_integer64", e1, e2)
          })

##' @rdname period
##' @export
setMethod("/", c("period", "numeric"),
          function(e1, e2) {
              .Call("divides_period_double", e1, e2)
          })

## Compare
## -------

##' @rdname period
##' @export
setMethod("Compare", c("ANY", "period"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })
##' @rdname period
##' @export
setMethod("Compare", c("period", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })

##' @rdname period
##' @export
setMethod("==", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname period
##' @export
setMethod("==", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname period
##' @export
setMethod("!=", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })
##' @rdname period
##' @export
setMethod("!=", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

##' @rdname period
##' @export
setMethod("==", c("period", "period"), function(e1, e2) .Call("eq_period_period", e1, e2))

##' @rdname period
##' @export
setMethod("!=", c("period", "period"), function(e1, e2) .Call("ne_period_period", e1, e2))

setMethod("all.equal",
          c("period"),
          function(target, current, ..., tolerance=0) {
              callNextMethod()
          })

