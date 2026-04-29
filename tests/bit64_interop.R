
v <- system(r"[ Rscript -e 'suppressMessages({library(nanotime); library(bit64)}); res <- tinytest::run_test_file("include/test_setops.R", verbose=0); s <- sapply(res, "["); cat(sum(s))' ]", intern=TRUE)
stopifnot(as.integer(v) == 31L)

v <- system(r"[ Rscript -e 'suppressMessages({library(nanotime)                }); res <- tinytest::run_test_file("include/test_setops.R", verbose=0); s <- sapply(res, "["); cat(sum(s))' ]", intern=TRUE)
stopifnot(as.integer(v) == 31L)

v <- system(r"[ Rscript -e 'suppressMessages({library(bit64); library(nanotime)}); res <- tinytest::run_test_file("include/test_setops.R", verbose=0); s <- sapply(res, "["); cat(sum(s))' ]", intern=TRUE)
stopifnot(as.integer(v) == 31L)
