
library(nanotime)

isSolaris <- Sys.info()[["sysname"]] == "SunOS"

## nanotime constructors
#test_nanotime_generic <- function() {
expect_equal(S3Part(nanotime(1), strict=TRUE), as.integer64(1))
expect_equal(nanotime(1), new("nanotime", as.integer64(1)))

#test_nanotime_character <- function() {
if (!isSolaris) {
    expect_equal(nanotime("1970-01-01T00:00:00.000000001+00:00"), nanotime(1))
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    ## check that the format and time zone is picked up from the global options:
    options(nanotimeFormat="%Y-%m-%d %H:%M")
    options(nanotimeTz="America/New_York")
    expect_equal(nanotime("1970-01-01 00:00"), nanotime(18000000000000))
    ## check they are overridden by the parameters:
    expect_equal(nanotime("1970-01-01 00:00:01", format="%Y-%m-%d %H:%M:%S", tz="Europe/Paris"),
                 nanotime(-3599000000000))
    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

#test_nanotime_matrix <- function() {
if (!isSolaris) {
    m <- matrix(c(10*24*3600+0:9, 9897654321+0:9), 10, 2)
    expect_equal(nanotime.matrix(m), nanotime("1970-01-11T00:00:00.987654321+00:00")+0:9)
}

#test_nanotime_POSIXct <- function() {
p <- as.POSIXct("1970-01-01 00:00:00", tz="America/New_York")
expect_equal(nanotime(p), nanotime("1970-01-01T00:00:00.000000000-05:00"))

#test_nanotime_POSIXlt <- function() {
l <- as.POSIXlt("1970-01-01 00:00:00", tz="America/New_York")
expect_equal(nanotime(l), nanotime("1970-01-01T00:00:00.000000000-05:00"))

#test_nanotime_Date <- function() {
d <- as.Date(10, origin="1970-01-01")
expect_equal(nanotime(d), nanotime("1970-01-11T00:00:00.000000000-05:00"))

#test_nanotime_numeric_keep_names <- function() {
n <- nanotime(c(a=1, b=2))
expect_equal(names(n), c("a","b"))

#test_nanotime_character_keep_names <- function() {
if (!isSolaris) {
    n <- nanotime(c(a="1970-01-01T00:00:00.000000001+00:00",
                    b="1970-01-01T00:00:00.000000001+00:00"))
    expect_equal(names(n), c("a","b"))
}

#test_nanotime_POSIXct_keep_names <- function() {
p <- as.POSIXct("1970-01-01 00:00:00", tz="America/New_York")
n <- nanotime(c(a=p, b=p))
expect_equal(names(n), c("a","b"))

#test_nanotime_POSIXlt_keep_names <- function() {
l <- as.POSIXlt("1970-01-01 00:00:00", tz="America/New_York")
n <- nanotime(c(a=l, b=l))
expect_equal(names(n), c("a","b"))


## format
#test_format_default <- function() {
oldFormat <- getOption("nanotimeFormat")
oldTz <- getOption("nanotimeTz")
options(nanotimeFormat=NULL)
options(nanotimeTz=NULL)
expect_equal(format(nanotime("1970-01-01T00:00:00.000000000+00:00")),
             "1970-01-01T00:00:00.000000000+00:00")
expect_equal(format(nanotime("1680-07-17T00:00:01.000000000+00:00")),
             "1680-07-17T00:00:01.000000000+00:00")
expect_equal(format(nanotime("2120-01-01T00:00:59.987654321+00:00")),
             "2120-01-01T00:00:59.987654321+00:00")
options(nanotimeFormat=oldFormat)
options(nanotimeTz=oldTz)

#test_format_tz <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)

    a_utc = "1970-01-01T00:00:00.000000000+00:00"
    nt <- nanotime(a_utc)
    a_ny <- "1969-12-31T19:00:00.000000000-05:00"

    options(nanotimeTz=NULL)
    expect_equal(format(nt, tz="America/New_York"), a_ny)

    options(nanotimeTz="UTC")
    expect_equal(format(nt, tz="America/New_York"), a_ny)

    attr(nt, "tzone") <- "UTC"
    expect_equal(format(nt, tz="America/New_York"), a_ny)
    expect_equal(format(nt, tz=""), a_utc)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

#test_format_tzone <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)

    a <- nanotime("1970-01-01T00:00:00.000000000+00:00")
    attr(a, "tzone") <- "America/New_York"
    a_ny <- "1969-12-31T19:00:00.000000000-05:00"

    expect_equal(format(a), a_ny)

    options(nanotimeTz="UTC")
    expect_equal(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

#test_format_tz_from_options <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)
    options(nanotimeTz="America/New_York")

    a <- nanotime("1970-01-01T00:00:00.000000000+00:00")
    a_ny <- "1969-12-31T19:00:00.000000000-05:00"

    expect_equal(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

#test_format_fmt_default <- function() {
oldFormat <- getOption("nanotimeFormat")
oldTz <- getOption("nanotimeTz")
options(nanotimeFormat=NULL)
options(nanotimeTz=NULL)

a_str <- "1970-01-01T00:00:00.000000000+00:00"
a <- nanotime(a_str)

expect_equal(format(a), a_str)

options(nanotimeFormat=oldFormat)
options(nanotimeTz=oldTz)

#test_format_fmt_from_options <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat="%Y/%m/%dT%H:%M:%E9S%Ez")
    options(nanotimeTz="America/New_York")

    a <- nanotime("1970/01/01T00:00:00.000000000+00:00")
    a_ny <- "1969/12/31T19:00:00.000000000-05:00"

    expect_equal(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

#test_format_fmt_from_parameter <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat="%Y/%m/%dT%H:%M:%E9S%Ez")
    options(nanotimeTz="America/New_York")

    a <- nanotime("1970-01-01 00:00:00.000000000+00:00", format="%Y-%m-%d %H:%M:%E9S%Ez")
                                        # the result of format on a is taken from the global option:
    a_ny <- "1969/12/31T19:00:00.000000000-05:00"

    expect_equal(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

#test_format_na <- function() {
if (!isSolaris) {
    f <- format(nanotime(c(1, NA, 2, NaN)))
    exp <- c("1970-01-01T00:00:00.000000001+00:00",
             as.character(NA),
             "1970-01-01T00:00:00.000000002+00:00",
             as.character(NA))
    expect_equal(f, exp)
}

## conversions
#test_as_POSIXct <- function() {
a <- nanotime(0)
attr(a, "tzone") <- "America/New_York"
p <- as.POSIXct(a)
expect_equivalent(p, as.POSIXct("1969-12-31 19:00:00", tz="America/New_York"))

oldTz <- getOption("nanotimeTz")
options(nanotimeTz="America/New_York")
b <- nanotime(0)
p <- as.POSIXct(b)
expect_equal(p, as.POSIXct("1969-12-31 19:00:00", tz="America/New_York"))

options(nanotimeTz=NULL)
c <- nanotime(0)
p <- as.POSIXct(c)
expect_equal(p, as.POSIXct("1970-01-01 00:00:00", tz="UTC"))
options(nanotimeTz=oldTz)

#test_as_POSIXct <- function() {
#a <- nanotime(0)
#attr(a, "tzone") <- "America/New_York"
#p <- as.POSIXlt(a)
#expect_equal(p, as.POSIXlt("1969-12-31 19:00:00", tz="America/New_York"))

#oldTz <- getOption("nanotimeTz")
#options(nanotimeTz="America/New_York")
#b <- nanotime(0)
#p <- as.POSIXlt(b)
#expect_equal(p, as.POSIXlt("1969-12-31 19:00:00", tz="America/New_York"))

#options(nanotimeTz=NULL)
#c <- nanotime(0)
#p <- as.POSIXlt(c)
#expect_equal(p, as.POSIXlt("1970-01-01 00:00:00", tz="UTC"))
#options(nanotimeTz=oldTz)

#test_as_Date <- function() {
expect_equal(as.Date(nanotime(0)), as.Date("1970-01-01"))

## c, subset, subassign and binds
#test_c <- function() {
a <- c(nanotime(1), nanotime(2))
expect_equal(a, nanotime(1:2))

a <- c(nanotime(1:2), nanotime(3:4))
expect_equal(a, nanotime(1:4))

#test_subset <- function() {
a <- nanotime(1:10)
expect_equal(a[3], nanotime(3))
expect_equal(a[1:10], a)

#test_subsassign <- function() {
a <- nanotime(1:10)
a[3] <- nanotime(13)
expect_equal(a[3], nanotime(13))
a[1:10] <- nanotime(10:1)
expect_equal(a[1:10], nanotime(10:1))

## summary
#test_summary <- function() {
expected <- nanotime(c(1,2,2,2,3,4))
names(expected) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
expect_equal(summary(nanotime(1:4)), expected)

expected <- nanotime(c(1,2,3,3,4,5))
names(expected) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
expect_equal(summary(nanotime(1:5)), expected)

## operation exceptions
#test_unary_minus <- function() {
expect_error(-nanotime(1), info="-nanotime(1) : unary '-' is not defined for \"nanotime\" objects")

#test_unary_plus <- function() {
expect_error(-nanotime(1), info="+nanotime(1) : unary '+' is not defined for \"nanotime\" objects")

#test_binary_mul <- function() {
#expect_error(nanotime(1) * nanotime(2), info="operation is not defined for \"nanotime\" objects")

#test_ANY_minus_nano <- function() {
expect_error(1 - nanotime(2), info="invalid operand types")

#test_nano_minus_ANY <- function() {
expect_error(nanotime(1) - "a", info="invalid operand types")

#test_ANY_plus_nano <- function() {
expect_error("a" + nanotime(2), info="invalid operand types")

#test_nano_plus_ANY <- function() {
expect_error(nanotime(1) + "a", info="invalid operand types")

#test_nano_mul_nano <- function() {
#expect_error(nanotime(1) * nanotime(1), info="operation not defined for \"nanotime\" objects")

#test_nano_mul_ANY <- function() {
expect_error(nanotime(1) * 1, info="operation not defined for \"nanotime\" objects")

#test_nano_div_ANY <- function() {
## div is in the same 'Arith' group as mul:
expect_error(nanotime(1) / 1, info="operation not defined for \"nanotime\" objects")

#test_nano_Logic_ANY <- function() {
expect_error(nanotime(1) | 1, info="operations are possible only for numeric, logical or complex types")

#test_ANY_Logic_nano <- function() {
expect_error(1 | nanotime(1), info="operations are possible only for numeric, logical or complex types")

#test_Math_nano <- function() {
expect_error(sin(nanotime(1)), info="non-numeric argument to mathematical function")

#test_Math2_nano <- function() {
expect_error(round(nanotime(1)), info="non-numeric argument to mathematical function")

## miscellaneous
#test_is.na <- function() {
expect_equal(is.na(nanotime(NA)), TRUE)
expect_equal(is.na(nanotime(NaN)), TRUE)
#}

## test square bracket (#44)
#test_square_bracket <- function() {
times <- c(nanotime('2011-12-05 08:30:00.000',format ="%Y-%m-%d %H:%M:%E9S",  tz ="GMT"),
           nanotime('2011-12-05 08:30:00.100',format ="%Y-%m-%d %H:%M:%E9S",  tz ="GMT"),
           nanotime('2011-12-05 08:30:00.825',format ="%Y-%m-%d %H:%M:%E9S",  tz ="GMT"))
expect_equal(times[1], times[[1]])
expect_equal(times[2], times[[2]])
expect_equal(times[3], times[[3]])
