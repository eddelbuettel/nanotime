# As long as tinytest does always load the package first with `test_package()`, we have to do it separately to achieve that bit64 is attached before nanotime in some test cases.
if (requireNamespace("tinytest", quietly=TRUE)) {
  # Create as many nodes as files exist, such that each node gets its own test file to run. Otheriwse more than one testfile would be run in a node and share a seesion.
  path <- file.path("..", "00_pkg_src", "nanotime", "inst", "tinytest")
  if (!dir.exists(path))
    path <- file.path("nanotime", "inst", "tinytest")
  # The cores are usually limited to 2 on CRAN, but with two we cannot guarantee that each test file is run in a separate session, so we need to remove the limit for the tests to run properly. We reset it afterwards to not interfere with other tests.
  oldVal <- Sys.getenv("_R_CHECK_LIMIT_CORES_")
  Sys.setenv("_R_CHECK_LIMIT_CORES_"="")
  cl <- parallel::makeCluster(length(dir(path)), outfile="")
  tinytest::run_test_dir(dir=path, cluster=cl)
  parallel::stopCluster(cl)
  Sys.setenv("_R_CHECK_LIMIT_CORES_"=oldVal)
}