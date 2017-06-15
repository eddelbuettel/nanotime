
pkg <- "nanotime"

if (!requireNamespace("RUnit", quietly = TRUE)) {
    cat("R package 'RUnit' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else if (!requireNamespace("nanotime", quietly = TRUE)) {
    cat("R package 'nanotime' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else if (!requireNamespace("bit64", quietly = TRUE)) {
    cat("R package 'bit64' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else {

    library(RUnit)
    library(nanotime)
    library(bit64)

    ## Define tests
    testSuite <- defineTestSuite(name=paste(pkg, "Unit Tests"),
                                 dirs=system.file("unitTests", package=pkg),
                                 testFileRegexp = "^test_.+\\.[rR]$",
                                 testFuncRegexp = "^[Tt]est+")

    ## Run tests
    tests <- runTestSuite(testSuite)

    ## Print results
    printTextProtocol(tests)

    ## Return success or failure to R CMD CHECK
    if (getErrors(tests)$nFail > 0) {
        print(getErrors(tests)$nFail)
        stop("TEST FAILED!")
    }
    if (getErrors(tests)$nErr > 0) {
        print(getErrors(tests)$nErr)
        stop("TEST HAD ERRORS!")
    }
    if (getErrors(tests)$nTestFunc < 1) {
        print(getErrors(tests)$nTestFunc)
        stop("NO TEST FUNCTIONS RUN!")
    }

}

