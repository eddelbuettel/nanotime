
rver <- getRversion()
si <- Sys.info()
if (rver < "4.3.0" || (si["sysname"] == "Darwin" && si["machine"] == "x86_64")) {
    ## for R versions prior to 4.3.0 we need to set the standard, and also on
    ## the r-release-macos-x86_64 machine which is not as C++20-ready as the rest
    cxxstd <- "CXX_STD = CXX17"
} else {
    ## for R 4.3.0 C++17 is standard so no need to set it
    cxxstd <- "#CXX_STD = CXX17"
}
win <- if (si["sysname"] == "Windows") ".win" else ""
infile <- file.path("src", paste0("Makevars", win, ".in"))
outfile <- file.path("src", paste0("Makevars", win))
lines <- readLines(infile)
lines <- gsub("@CXXSTD@", cxxstd, lines)
writeLines(lines, outfile)
