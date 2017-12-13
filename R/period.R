setClass("period", contains = "integer64")

setGeneric("as.period", function(x) standardGeneric("as.period"))

setMethod("as.period",
          "character",
          function(x) {
              res <- .Call('period_from_string', x)
              oldClass(res) <- "integer64"
              new("period", res)
          })

setMethod("as.period",
          "integer64",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), x)))
          })

setMethod("as.period",
          "numeric",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), as.integer64(x))))
          })

setMethod("as.period",
          "integer",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), as.integer64(x))))
          })

setMethod("as.period",
          "duration",
          function(x) {
              new("period", c(rbind(rep(as.integer64(0), length(x)), as.integer64(x))))
          })

setMethod("show",
          signature("period"),
          function(object) print(object))

setMethod("print",
          "period",
          function(x, ...) {
              print(.Call('period_to_string', x))
          })

setMethod("as.character",
          signature("period"),
          function(x) {
              .Call('period_to_string', x)
          })

## accessors

## ----------- non ops

setMethod("[",
          signature("period", "logical"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("period", x[rep(i, each=2)])
          })

setMethod("[",
          signature("period", "numeric"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              i <- (i-1)*2 + 1
              i <- sapply(i, function(k) c(k, k+1))
              new("period", x[i])
          })
          
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

setMethod("[<-",
          signature("period", "logical", "ANY", "period"),
          function (x, i, j, ..., value) {
              x <- S3Part(x, strictS3=TRUE)
              x[rep(i, each=2)] <- S3Part(value, strictS3=TRUE)
              new("period", x)
          })

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

setMethod("names",
          signature("period"),
          function(x) {
              callNextMethod()[c(TRUE, FALSE)]
          })

setMethod("names<-",
          signature("period"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- rep(value, each=2)
              new("period", x)
          })


## ----------- make sure ops that don't make sense error out
setMethod("Ops", c("period", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })
setMethod("Ops", c("ANY", "period"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })

setMethod("Math", c("period"),
          function(x) {
              stop("operation not defined for \"period\" objects")
          })

setMethod("Math2", c("period"),
          function(x, digits) {
              stop("operation not defined for \"period\" objects")
          })

setMethod("Summary", c("period"),
          function(x, ..., na.rm = FALSE) {
              stop("invalid 'type' (nanoival) of argument")
          })

setMethod("Complex", c("period"),
          function(z) {
              stop("operation not defined for \"period\" objects")
          })

## ------------ `-`
setMethod("-", c("period", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  ## incorrect LLL
                  new("period", -S3Part(e1, strictS3=TRUE))
              }
              else {
                  stop("invalid operand types")
              }
          })

setMethod("-", c("period", "period"),
          function(e1, e2) {
              .Call("minus_period_period", e1, e2)
          })

## --
setMethod("-", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("-", c("period", "duration"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e1)
          })

setMethod("-", c("period", "integer64"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, e2)
          })

## setMethod("-", c("period", "integer"),
##           function(e1, e2) {
##               .Call("minus_period_integer64", e1, as.integer64(e2))
##           })

setMethod("-", c("period", "numeric"),
          function(e1, e2) {
              .Call("minus_period_integer64", e1, as.integer64(e2))
          })

## --
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

setMethod("+", c("period", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  e2
              }
              else {
                  stop("invalid operand types")
              }
          })

setMethod("+", c("period", "period"),
          function(e1, e2) {
              .Call("plus_period_period", e1, e2)
          })

## --
setMethod("+", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("+", c("period", "duration"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, S3Part(e2, strictS3=TRUE))
          })

setMethod("+", c("period", "integer64"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, e2)
          })

setMethod("+", c("period", "numeric"),
          function(e1, e2) {
              .Call("plus_period_integer64", e1, as.integer64(e2))
          })

## --
setMethod("+", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("+", c("duration", "period"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

setMethod("+", c("integer64", "period"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, e1)
          })

setMethod("+", c("numeric", "period"),
          function(e1, e2) {
              .Call("plus_period_integer64", e2, as.integer64(e1))
          })

## ----------- `*`

setMethod("*", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("*", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("*", c("period", "integer64"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e1, e2)
          })

setMethod("*", c("period", "numeric"),
          function(e1, e2) {
              .Call("multiplies_period_double", e1, e2)
          })

setMethod("*", c("integer64", "period"),
          function(e1, e2) {
              .Call("multiplies_period_integer64", e2, e1)
          })

setMethod("*", c("numeric", "period"),
          function(e1, e2) {
              .Call("multiplies_period_double", e2, e1)
          })

## ----------- `/`

setMethod("/", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("/", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("/", c("period", "integer64"),
          function(e1, e2) {
              .Call("divides_period_integer64", e1, e2)
          })

setMethod("/", c("period", "numeric"),
          function(e1, e2) {
              .Call("divides_period_double", e1, e2)
          })

## Compare
## -------

setMethod("Compare", c("ANY", "period"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })
setMethod("Compare", c("period", "ANY"),
          function(e1, e2) {
              stop("operation not defined for \"period\" objects")
          })

setMethod("==", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })
setMethod("==", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
setMethod("!=", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })
setMethod("!=", c("period", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("==", c("period", "period"), function(e1, e2) .Call("eq_period_period", e1, e2))
setMethod("!=", c("period", "period"), function(e1, e2) .Call("ne_period_period", e1, e2))
