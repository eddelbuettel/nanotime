## .parseISO8601 . .
## align.time . . . .
## apply.monthly . .
## as.environment.xts
## as.xts . . . . . .
## as.xts.methods . .
## axTicksByTime . .
## CLASS . . . . . .
## - coredata.xts . . .
## diff.xts
## dimnames.xts . . .
## endpoints . . . .
## first . . . . . .
## firstof . . . . .
## indexClass . . . .
## indexTZ
## - index
## isOrdered . . . .
## make.index.unique
## merge.xts . . . .
## na.locf.xts . . .
## ndays . . . . . .
## period.apply . . .
## period.max . . . .
## period.min . . . .
## period.prod . . .
## period.sum . . . .
## periodicity . . .
## plot.xts . . . . .
## - rbind.xts
## sample_matrix . .
## split.xts . . . .
## timeBased . . . .
## timeBasedSeq . . .
## to.period . . . .
## xts . . . . . . .
## xtsAPI . . . . . .
## xtsAttributes . .
## xtsInternals . . .
## [.xts . . . . . .

if (FALSE &&                            	# for now, never use this
    requireNamespace("xts", quietly=TRUE) &&
    packageVersion("xts") >= "0.12.0") {   	# hypothetical future version with nanotime support

    .setUp <- function() {
        library(xts)
    }

    test_coredata.xts <- function() {
        i1 <- nanotime(Sys.time()) + as.integer64(1:10)
        a1 <- matrix(1:30, 10, 3)
        x1 <- xts(a1, i1)
        all.equal(coredata(x1), a1)
    }
    test_index.xts <- function() {
        i1 <- nanotime(Sys.time()) + as.integer64(1:10)
        a1 <- matrix(1:30, 10, 3)
        x1 <- xts(a1, i1)
        ## don't check attributes, as xts adds 'tzone' and 'tclass'
        all.equal(index(x1), i1, check.attributes = FALSE)
    }
    test_rbind.xts <- function() {
        i1 <- nanotime(Sys.time()) + as.integer64(1:10)
        i2 <- nanotime(Sys.time()) + as.integer64(11:20)
        x1 <- xts(matrix(1:30, 10, 3), i1)
        x2 <- xts(matrix(1:30, 10, 3), i2)
        exp <- xts(rbind(matrix(1:30, 10, 3), matrix(1:30, 10, 3)), nanotime(c(i1,i2)))
        all.equal(rbind(x1, x2), exp)
    }
    test_c.xts <- function() {
        i1 <- nanotime(Sys.time())+ as.integer64(1:10)
        i2 <- nanotime(Sys.time())+ as.integer64(11:20)
        x1 <- xts(matrix(1:30, 10, 3), i1)
        x2 <- xts(matrix(1:30, 10, 3), i2)
        exp <- xts(rbind(matrix(1:30, 10, 3), matrix(1:30, 10, 3)), nanotime(c(i1,i2)))
        all.equal(c(x1, x2), exp)
    }
    test_cbind.xts <- function() {
        i1 <- nanotime(Sys.time())+ as.integer64(1:10)
        i2 <- nanotime(Sys.time())+ as.integer64(11:20)
        dimnames1 <-list(NULL, c("a1","b1","c1"))
        dimnames2 <-list(NULL, c("a2","b2","c2"))
        a1 <- matrix(1:30, 10, 3, dimnames=dimnames1)
        a2 <- matrix(1:30, 10, 3, dimnames=dimnames2)
        x1 <- xts(a1, i1)
        x2 <- xts(a2, i2)

        exp <- xts(rbind(cbind(a1, matrix(NA, 10, 3, dimnames=dimnames2)),
                         cbind(matrix(NA, 10, 3, dimnames=dimnames1), a2)),
                   nanotime(c(i1,i2)))
        all.equal(cbind(x1, x2), exp)
    }
    ## test_plot.xts <- function()
    ## {
    ##   ## plot doesn't work properly
    ##   data(sample_matrix)
    ##   sample.xts <- as.xts(sample_matrix)
    ##   x <- xts(coredata(sample.xts), nanotime(index(sample.xts)))
    ##   plot(x[,"Close"])
    ## }
    test_dimnames.xts <- function() {
        dimnames1 <-list(NULL, c("a1","b1","c1"))
        i1 <- nanotime(Sys.time())+ as.integer64(1:10)
        a1 <- matrix(1:30, 10, 3, dimnames=dimnames1)
        x1 <- xts(a1, i1)
        all.equal(dimnames(x1), dimnames1)
    }
    test_align.time <- function() {
        i1 <- nanotime(Sys.time()) + as.integer64(1e9*1:200)
        a1 <- matrix(1:600, 200, 3)
        x1 <- xts(a1, i1)
        xx1 <- xts(a1, as.POSIXct(i1))
        all.equal(align.time(x1, n=as.integer64(60*1e9)), a1)
    }
    test_endpoints.xts <- function() {
        data(sample_matrix)
        sample.xts <- as.xts(sample_matrix)
        x <- xts(coredata(sample.xts), nanotime(index(sample.xts)))
        all.equal(endpoints(x), c(0, 30, 58, 89, 119, 150, 180))
    }
}
