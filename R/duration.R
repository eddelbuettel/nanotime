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


## subset/subassign

setMethod("[",
          signature("duration"),
          function (x, i, j, ..., drop=FALSE) {
              new("duration", callNextMethod())
          })

setMethod("[<-",
          signature("duration"),
          function (x, i, j, ..., value) {
              new("duration", callNextMethod())
          })

c.duration <- function(...) {
    res <- do.call(c.integer64, list(...))
    new("duration", res)
}

setMethod("names<-",
          signature("duration"),
          function(x, value) {
              names(S3Part(x, strictS3=TRUE)) <- value
              x
          })
