
rver <- getRversion()
if (rver < "4.3.0") {
    ## for R versions prior to 4.3.0 we need to set the standard
    cxxstd <- "CXX_STD = CXX17"
} else {
    ## for R 4.3.0 C++17 is standard so no need to set it
    cxxstd <- "#CXX_STD = CXX17"
}
win <- if (Sys.info()[["sysname"]] == "Windows") ".win" else ""
infile <- file.path("src", paste0("Makevars", win, ".in"))
outfile <- file.path("src", paste0("Makevars", win))
lines <- readLines(infile)
lines <- gsub("@CXXSTD@", cxxstd, lines)
writeLines(lines, outfile)
