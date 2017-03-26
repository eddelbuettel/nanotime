pkg <- "nanotime"


doTest <- function(results, path, file) {
  results[[file]] <- runTestFile(paste0(path, file))
  results
}


checkForErrors <- function(l) {
  for (nm in names(l)) {
    if (getErrors(l[[nm]])$nErr || getErrors(l[[nm]])$nFail) {
      stop(paste("error or failure in", nm, ": ", l[[nm]]))
    }
  }
}

    
if (!require("RUnit", quietly = TRUE)) {
  cat("R package 'RUnit' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else if (!require("nanotime", quietly = TRUE)) {
  cat("R package 'nanotime' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else if (!require("bit64", quietly = TRUE)) {
  cat("R package 'bit64' cannot be loaded -- no unit tests run for package", pkg, "\n")
} else {

  path <- paste0("../", pkg, "/unitTests/")

  results <- list()
  results <- doTest(results, path, "test_nanotime.R")
  results <- doTest(results, path, "test_ops.R")
  results <- doTest(results, path, "test_data.frame.R")

  if (require("zoo", quietly = TRUE)) {
    results <- doTest(results, path, "test_zoo.R")
  }
##  if (require("xts", quietly = TRUE)) {
##    results <- doTest(results, path, "test_xts.R")
##  }
    
  checkForErrors(results)
}

