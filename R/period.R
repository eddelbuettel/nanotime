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
              new("period", x)
          })

setMethod("as.period",
          "numeric",
          function(x) {
              new("period", as.integer64(x))
          })

setMethod("as.period",
          "integer",
          function(x) {
              new("period", as.integer64(x))
          })

setMethod("show",
          signature("period"),
          function(object) print(object))

setMethod("print",
          "period",
          function(x, ...) {
              print(.Call('period_to_string', x))
          })

## needed? LLL
as.integer64.period <- function(x, ...) {
    S3Part(x, strictS3=TRUE)
}

setMethod("as.character",
          signature("period"),
          function(x) {
              .Call('period_to_string', x)
          })

## ------------ `-`

setMethod("-", c("period", "period"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) - S3Part(e2, strictS3=TRUE))
          })

setMethod("-", c("period", "integer64"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) - e2)
          })

setMethod("-", c("period", "integer"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) - e2)
          })

setMethod("-", c("period", "numeric"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) - e2)
          })

setMethod("-", c("period", "ANY"),
          function(e1, e2) {
              if (missing(e2)) {
                  new("period", -S3Part(e1, strictS3=TRUE))
              }
              else {
                  stop("invalid operand types")
              }
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

setMethod("+", c("period", "integer64"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) + e2)
          })

setMethod("+", c("period", "numeric"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) + e2)
          })

setMethod("+", c("ANY", "period"),
          function(e1, e2) {
              stop("invalid operand types")
          })

setMethod("+", c("integer64", "period"),
          function(e1, e2) {
              new("period", e1 + S3Part(e2, strictS3=TRUE))
          })

setMethod("+", c("numeric", "period"),
          function(e1, e2) {
              new("period", e1 + S3Part(e2, strictS3=TRUE))
          })

## ----------- `*`

setMethod("*", c("period", "integer64"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) * e2)
          })

setMethod("*", c("period", "numeric"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) * e2)
          })

## ----------- `/`

setMethod("/", c("period", "integer64"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) / e2)
          })

setMethod("/", c("period", "numeric"),
          function(e1, e2) {
              new("period", S3Part(e1, strictS3=TRUE) / e2)
          })
