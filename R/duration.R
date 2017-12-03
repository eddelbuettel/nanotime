setClass("duration", contains = "integer64")

setGeneric("as.duration", function(x) standardGeneric("as.duration"))

setMethod("as.duration",
          "character",
          function(x) {
              res <- .Call('duration_from_string', x)
              oldClass(res) <- "integer64"
              new("duration", res)
          })

setMethod("as.duration",
          "integer64",
          function(x) {
              new("duration", x)
          })

setMethod("as.duration",
          "numeric",
          function(x) {
              new("duration", as.integer64(x))
          })

setMethod("as.duration",
          "integer",
          function(x) {
              new("duration", as.integer64(x))
          })

setMethod("show",
          signature("duration"),
          function(object) print(object))

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

setMethod("-", c("duration", "duration"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - S3Part(e2, strictS3=TRUE))
          })

setMethod("-", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - e2)
          })

setMethod("-", c("duration", "integer"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - e2)
          })

setMethod("-", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) - e2)
          })

setMethod("-", c("duration", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  new("duration", -S3Part(e1, strictS3=TRUE))
              }
              else {
                  stop("invalid operand types")
              }
          })

setMethod("-", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `+`

setMethod("+", c("duration", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  e2
              }
              else {
                  stop("invalid operand types")
              }
          })

setMethod("+", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) + e2)
          })

setMethod("+", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) + e2)
          })

setMethod("+", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("+", c("integer64", "duration"),
          function(e1, e2) {
              new("duration", e1 + S3Part(e2, strictS3=TRUE))
          })

setMethod("+", c("numeric", "duration"),
          function(e1, e2) {
              new("duration", e1 + S3Part(e2, strictS3=TRUE))
          })

## ----------- `*`

setMethod("*", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) * e2)
          })
setMethod("*", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", S3Part(e1, strictS3=TRUE) * e2)
          })

setMethod("*", c("numeric", "duration"),
          function(e1, e2) {
              new("duration", e1 * S3Part(e2, strictS3=TRUE))
          })
setMethod("*", c("integer64", "duration"),
          function(e1, e2) {
              new("duration", e1 * S3Part(e2, strictS3=TRUE))
          })
setMethod("*", c("duration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
setMethod("*", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })

## ----------- `/`

setMethod("/", c("duration", "integer64"),
          function(e1, e2) {
              new("duration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
setMethod("/", c("duration", "numeric"),
          function(e1, e2) {
              new("duration", as.integer64(S3Part(e1, strictS3=TRUE) / e2))
          })
setMethod("/", c("duration", "ANY"),
          function(e1, e2) {
              stop("invalid operand types")
          })
setMethod("/", c("ANY", "duration"),
          function(e1, e2) {
              stop("invalid operand types")
          })


## subset

setMethod("[",
          signature("duration", "logical"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              new("duration", x[rep(i, each=2)])
          })

setMethod("[",
          signature("duration", "numeric"),
          function (x, i, j, ..., drop=FALSE) {
              ## verify ... is empty LLL
              x <- S3Part(x, strictS3=TRUE)
              i <- (i-1)*2 + 1
              i <- sapply(i, function(k) c(k, k+1))
              new("duration", x[i])
          })

setMethod("[<-",
          signature("duration", "logical", "ANY", "duration"),
          function (x, i, j, ..., value) {
              x <- S3Part(x, strictS3=TRUE)
              x[rep(i, each=2)] <- S3Part(value, strictS3=TRUE)
              new("duration", x)
          })

c.duration <- function(...) {
    res <- do.call(c.integer64, list(...))
    new("duration", res)
}
