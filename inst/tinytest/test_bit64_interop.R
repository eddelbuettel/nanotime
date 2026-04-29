
val1 <- system(r"[ Rscript -e 'suppressMessages({library(nanotime); library(bit64)}); res <- tinytest::run_test_file("include/test_setops.R", verbose=1); s <- sapply(res, "["); q(save="no", status=sum(s))' ]")
val2 <- system(r"[ Rscript -e 'suppressMessages({library(nanotime)                }); res <- tinytest::run_test_file("include/test_setops.R", verbose=1); s <- sapply(res, "["); q(save="no", status=sum(s))' ]")
val3 <- system(r"[ Rscript -e 'suppressMessages({library(bit64); library(nanotime)}); res <- tinytest::run_test_file("include/test_setops.R", verbose=1); s <- sapply(res, "["); q(save="no", status=sum(s))' ]")

n <- 31L  # number of tests expected to pass
expect_equal(as.integer(val1), n)
expect_equal(as.integer(val2), n)
expect_equal(as.integer(val3), n)
