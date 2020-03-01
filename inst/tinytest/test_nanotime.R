
library(nanotime)


isSolaris <- Sys.info()[["sysname"]] == "SunOS"

## nanotime constructors
##test_nanotime_generic <- function() {
expect_identical(S3Part(nanotime(1), strict=TRUE), as.integer64(1))
expect_identical(nanotime(1), new("nanotime", as.integer64(1)))
expect_identical(nanotime(as.integer64(1)), new("nanotime", as.integer64(1)))
expect_identical(as.nanotime(1), new("nanotime", as.integer64(1)))
expect_identical(as.nanotime(as.integer64(1)), new("nanotime", as.integer64(1)))


##test_nanotime_character_first_pass <- function() {
## we do a first pass parsing, which is faster than the parsing
## with the format string, and is also more forgiving
if (!isSolaris) {
    expect_identical(nanotime("2018-01-01T05:00:00.99 Europe/London"),             nanotime(as.integer64("1514782800990000000")))
    expect_identical(nanotime("2018-01-01T05:00:00.999_999_999 America/New_York"), nanotime(as.integer64("1514800800999999999")))
    expect_identical(nanotime("2018-01-01T05:00:00.999_999 America/New_York"),     nanotime(as.integer64("1514800800999999000")))
    expect_identical(nanotime("2018-01-01T05:00:00.999 America/New_York"),         nanotime(as.integer64("1514800800999000000")))
    expect_identical(nanotime("2018-01-01T05:00:00 America/New_York"),             nanotime(as.integer64("1514800800000000000")))
    expect_identical(nanotime("2018-01-01 05:00:00 America/New_York"),             nanotime(as.integer64("1514800800000000000")))
    expect_identical(nanotime("2018-01-01 05:00:00 America/New_York"),             nanotime(as.integer64("1514800800000000000")))
    expect_identical(nanotime("2018-01-01 America/New_York"),                      nanotime(as.integer64("1514782800000000000")))

    expect_identical(nanotime("2018-01-01T05:00:00.99+00:00"),                     nanotime(as.integer64("1514782800990000000")))
    expect_identical(nanotime("2018-01-01T05:00:00.99-00:00"),                     nanotime(as.integer64("1514782800990000000")))
    expect_identical(nanotime("2018-01-01T05:00:00.999_999_999+05:00"),            nanotime(as.integer64("1514764800999999999")))
    expect_identical(nanotime("2018-01-01T05:00:00.999_999+05:00"),                nanotime(as.integer64("1514764800999999000")))
    expect_identical(nanotime("2018-01-01T05:00:00.999+05:00"),                    nanotime(as.integer64("1514764800999000000")))
    expect_identical(nanotime("2018-01-01T05:00:00+05:00"),                        nanotime(as.integer64("1514764800000000000")))
    expect_identical(nanotime("2018-01-01 05:00:00+05:00"),                        nanotime(as.integer64("1514764800000000000")))
    expect_identical(nanotime("2018-01-01 05:00:00+05:00"),                        nanotime(as.integer64("1514764800000000000")))
    expect_identical(nanotime("2018-01-01   05:00:00+05:00"),                      nanotime(as.integer64("1514764800000000000")))
    expect_identical(nanotime("2018-01-01+05:00"),                                 nanotime(as.integer64("1514746800000000000")))

    expect_identical(nanotime("2018/01/01T05:00:00.99 Europe/London"),             nanotime(as.integer64("1514782800990000000")))
    expect_identical(nanotime("2018 01 01T05:00:00.99 Europe/London"),             nanotime(as.integer64("1514782800990000000")))   

    ## test the alias 'as.nanotime':
    expect_identical(as.nanotime("2018-01-01T05:00:00.99 Europe/London"),             nanotime(as.integer64("1514782800990000000")))
    expect_identical(as.nanotime("2018-01-01T05:00:00.999_999_999 America/New_York"), nanotime(as.integer64("1514800800999999999")))
    expect_identical(as.nanotime("2018-01-01T05:00:00.999_999 America/New_York"),     nanotime(as.integer64("1514800800999999000")))
    expect_identical(as.nanotime("2018-01-01T05:00:00.999 America/New_York"),         nanotime(as.integer64("1514800800999000000")))
    expect_identical(as.nanotime("2018-01-01T05:00:00 America/New_York"),             nanotime(as.integer64("1514800800000000000")))
    expect_identical(as.nanotime("2018-01-01 05:00:00 America/New_York"),             nanotime(as.integer64("1514800800000000000")))
    expect_identical(as.nanotime("2018-01-01 05:00:00 America/New_York"),             nanotime(as.integer64("1514800800000000000")))
    expect_identical(as.nanotime("2018-01-01 America/New_York"),                      nanotime(as.integer64("1514782800000000000")))

    expect_identical(as.nanotime("2018-01-01T05:00:00.99+00:00"),                     nanotime(as.integer64("1514782800990000000")))
    expect_identical(as.nanotime("2018-01-01T05:00:00.99-00:00"),                     nanotime(as.integer64("1514782800990000000")))
    expect_identical(as.nanotime("2018-01-01T05:00:00.999_999_999+05:00"),            nanotime(as.integer64("1514764800999999999")))
    expect_identical(as.nanotime("2018-01-01T05:00:00.999_999+05:00"),                nanotime(as.integer64("1514764800999999000")))
    expect_identical(as.nanotime("2018-01-01T05:00:00.999+05:00"),                    nanotime(as.integer64("1514764800999000000")))
    expect_identical(as.nanotime("2018-01-01T05:00:00+05:00"),                        nanotime(as.integer64("1514764800000000000")))
    expect_identical(as.nanotime("2018-01-01 05:00:00+05:00"),                        nanotime(as.integer64("1514764800000000000")))
    expect_identical(as.nanotime("2018-01-01 05:00:00+05:00"),                        nanotime(as.integer64("1514764800000000000")))
    expect_identical(as.nanotime("2018-01-01   05:00:00+05:00"),                      nanotime(as.integer64("1514764800000000000")))
    expect_identical(as.nanotime("2018-01-01+05:00"),                                 nanotime(as.integer64("1514746800000000000")))

    expect_identical(as.nanotime("2018/01/01T05:00:00.99 Europe/London"),             nanotime(as.integer64("1514782800990000000")))
    expect_identical(as.nanotime("2018 01 01T05:00:00.99 Europe/London"),             nanotime(as.integer64("1514782800990000000")))   
}
expect_identical(as.nanotime(NULL), nanotime())
expect_identical(as.nanotime(NULL), as.nanotime())

##test_nanotime_character_first_pass_fail <- function() {
## none of these should parse
expect_error(nanotime("2018-01-01T05:00:00.99 America/New_dYork"),  "Cannot retrieve timezone")
expect_error(nanotime("2018--01-01T05:00:00.99 America/New_York"),  "Parse error")
expect_error(nanotime("2018-01-01T05:00:00.99 America/New_York s"), "Parse error")
expect_error(nanotime("2018-01-01T05:00:s0.99 America/New_York"),   "Parse error")
expect_error(nanotime("2018-01-01T05:00:00.a99 America/New_York"), "Parse error")
expect_error(nanotime("2018-01-01T05:00:00.0000000000 America/New_York"), "Parse error")
expect_error(nanotime("201"), "Parse error")
expect_error(nanotime("2018-13-01T05:00:00.99 Europe/London"), "Parse error")
expect_error(nanotime("2018"), "Parse error")
expect_error(nanotime("2018-1"), "Parse error")
expect_error(nanotime("2018-01-32T05:00:00.99 Europe/London"), "Parse error")
expect_error(nanotime("2018-01-01T25:00:00.99 Europe/London"), "Parse error")
expect_error(nanotime("2018-01-01T05:61:00.99 Europe/London"), "Parse error")
expect_error(nanotime("2018-01-01T05:00:61.99 Europe/London"), "Parse error")
expect_error(nanotime("2018-01-01T05:00:00.99999999999 Europe/London"), "Parse error")
expect_error(nanotime("2018-01-01T05:00:00.99 Europe/London%"), "Parse error")
expect_error(nanotime("2018-01-01T05:00:00.99 %"), "Parse error")

##test_nanotime_character_second_pass  <- function() {
## if the parsing above has failed, then we use a second parsing
## that is based on the format string that is provided
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    ## check that the format and time zone is picked up from the global options:
    options(nanotimeFormat="%Y|%m|%d %H:%M")
    options(nanotimeTz="America/New_York")
    expect_identical(nanotime("1970|01|01 00:00"), nanotime(18000000000000))
    ## check they are overridden by the parameters:
    expect_identical(nanotime("1970|01|01 00:00:01", format="%Y|%m|%d %H:%M:%S", tz="Europe/Paris"),
                     nanotime(-3599000000000))
    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}
##test_nanotime_character_second_pass_fail  <- function() {
oldFormat <- getOption("nanotimeFormat")
oldTz <- getOption("nanotimeTz")
## check that the format and time zone is picked up from the global options:
options(nanotimeFormat="%Y|%m|%d %H:%M")
options(nanotimeTz="America/New_York")
expect_error(nanotime("1970-01-01 00:00"), "Parse error on 1970-01-01 00:00")
## check they are overridden by the parameters:
expect_error(nanotime("1970-01-01 00:00", format="%Y|%m|%d %H:%M:%S", tz="Europe/Paris"), "Parse error")
options(nanotimeFormat=oldFormat)
options(nanotimeTz=oldTz)


##test_nanotime_matrix <- function() {
if (!isSolaris) {
    m <- matrix(c(10*24*3600+0:9, 987654321+0:9), 10, 2)
    expect_identical(nanotime.matrix(m),
                     nanotime("1970-01-11T00:00:00.987654321+00:00") + seq(0, 9e9, by=1e9) + 0:9)
}

##test_nanotime_POSIXct <- function() {
p <- as.POSIXct("1970-01-01 00:00:00", tz="America/New_York")
expect_identical(nanotime(p), nanotime("1970-01-01T00:00:00.000000000-05:00"))
expect_identical(as.nanotime(p), nanotime("1970-01-01T00:00:00.000000000-05:00"))

##test_nanotime_POSIXlt <- function() {
l <- as.POSIXlt("1970-01-01 00:00:00", tz="America/New_York")
expect_identical(nanotime(l), nanotime("1970-01-01T00:00:00.000000000-05:00"))
expect_identical(as.nanotime(l), nanotime("1970-01-01T00:00:00.000000000-05:00"))

##test_nanotime_Date <- function() {
d <- as.Date(10, origin="1970-01-01")
expect_identical(nanotime(d), nanotime("1970-01-11T00:00:00.000000000-00:00"))
expect_identical(as.nanotime(d), nanotime("1970-01-11T00:00:00.000000000-00:00"))

##test_nanotime_numeric_keep_names <- function() {
n <- nanotime(c(a=1, b=2))
expect_identical(names(n), c("a","b"))

##test_nanotime_character_keep_names <- function() {
if (!isSolaris) {
    n <- nanotime(c(a="1970-01-01T00:00:00.000000001+00:00",
                    b="1970-01-01T00:00:00.000000001+00:00"))
    expect_identical(names(n), c("a","b"))
}

##test_nanotime_POSIXct_keep_names <- function() {
p <- as.POSIXct("1970-01-01 00:00:00", tz="America/New_York")
n <- nanotime(c(a=p, b=p))
expect_identical(names(n), c("a","b"))

##test_nanotime_POSIXlt_keep_names <- function() {
l <- as.POSIXlt("1970-01-01 00:00:00", tz="America/New_York")
n <- nanotime(c(a=l, b=l))
expect_identical(names(n), c("a","b"))


## format
##test_format_default <- function() {
oldFormat <- getOption("nanotimeFormat")
oldTz <- getOption("nanotimeTz")
options(nanotimeFormat=NULL)
options(nanotimeTz=NULL)
expect_identical(format(nanotime("1970-01-01T00:00:00.000000000+00:00")),
                 "1970-01-01T00:00:00+00:00")
expect_identical(format(nanotime("1680-07-17T00:00:01.000000000+00:00")),
                 "1680-07-17T00:00:01+00:00")
expect_identical(format(nanotime("2120-01-01T00:00:59.987654321+00:00")),
                 "2120-01-01T00:00:59.987654321+00:00")
expect_identical(format(nanotime(NULL)), "nanotime(0)")
options(nanotimeFormat=oldFormat)
options(nanotimeTz=oldTz)

##test_format_tz <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)

    a_utc = "1970-01-01T00:00:00+00:00"
    nt <- nanotime(a_utc)
    a_ny <- "1969-12-31T19:00:00-05:00"

    options(nanotimeTz=NULL)
    expect_identical(format(nt, tz="America/New_York"), a_ny)

    options(nanotimeTz="UTC")
    expect_identical(format(nt, tz="America/New_York"), a_ny)

    attr(nt, "tzone") <- "UTC"
    expect_identical(format(nt, tz="America/New_York"), a_ny)
    expect_identical(format(nt, tz=""), a_utc)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

##test_format_tzone <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)

    a <- nanotime("1970-01-01T00:00:00.001000000+00:00")
    attr(a, "tzone") <- "America/New_York"
    a_ny <- "1969-12-31T19:00:00.001-05:00"

    expect_identical(format(a), a_ny)

    options(nanotimeTz="UTC")
    expect_identical(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

##test_format_tz_from_options <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)
    options(nanotimeTz="America/New_York")

    a <- nanotime("1970-01-01T00:00:00.000001000+00:00")
    a_ny <- "1969-12-31T19:00:00.000001-05:00"

    expect_identical(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

##test_format_fmt_default <- function() {
oldFormat <- getOption("nanotimeFormat")
oldTz <- getOption("nanotimeTz")
options(nanotimeFormat=NULL)
options(nanotimeTz=NULL)

a_str <- "1970-01-01T00:00:00+00:00"
a <- nanotime(a_str)

expect_identical(format(a), a_str)

options(nanotimeFormat=oldFormat)
options(nanotimeTz=oldTz)

##test_format_fmt_from_options <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat="%Y/%m/%dT%H:%M:%E9S%Ez")
    options(nanotimeTz="America/New_York")

    a <- nanotime("1970/01/01T00:00:00.000000000+00:00")
    a_ny <- "1969/12/31T19:00:00.000000000-05:00"

    expect_identical(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

##test_format_fmt_from_parameter <- function() {
if (!isSolaris) {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat="%Y/%m/%dT%H:%M:%E9S%Ez")
    options(nanotimeTz="America/New_York")

    a <- nanotime("1970-01-01 00:00:00.000000000+00:00", format="%Y-%m-%d %H:%M:%E9S%Ez")
                                        # the result of format on a is taken from the global option:
    a_ny <- "1969/12/31T19:00:00.000000000-05:00"

    expect_identical(format(a), a_ny)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}

##test_format_na <- function() {
if (!isSolaris) {
    f <- format(nanotime(c(1, NA, 2, NaN)))
    exp <- c("1970-01-01T00:00:00.000000001+00:00",
             as.character(NA),
             "1970-01-01T00:00:00.000000002+00:00",
             as.character(NA))
    expect_identical(f, exp)
}


## conversions
##test_as_POSIXct <- function() {
a <- nanotime(0)
attr(a, "tzone") <- "America/New_York"
p <- as.POSIXct(a)
expect_identical(p, as.POSIXct("1969-12-31 19:00:00", tz="America/New_York"))

oldTz <- getOption("nanotimeTz")
options(nanotimeTz="America/New_York")
b <- nanotime(0)
p <- as.POSIXct(b)
## LLL: not fully satisfactory here; nanotime will not have 'tz' set from the environment:
expect_equal(p, as.POSIXct("1969-12-31 19:00:00", tz="America/New_York"))

options(nanotimeTz=NULL)
c <- nanotime(0)
p <- as.POSIXct(c)
expect_equal(p, as.POSIXct("1970-01-01 00:00:00", tz="UTC"))

options(nanotimeTz=oldTz)

##test_as_Date <- function() {
expect_identical(as.Date(nanotime(0)), as.Date("1970-01-01"))


## c, subset, subassign and binds
##test_c <- function() {
a <- c(nanotime(1), nanotime(2))
expect_identical(a, nanotime(1:2))

a <- c(nanotime(1:2), nanotime(3:4))
expect_identical(a, nanotime(1:4))

##test_subset <- function() {
a <- nanotime(1:10)
expect_identical(a[3], nanotime(3))
expect_identical(a[1:10], a)

##test_subsassign <- function() {
a <- nanotime(1:10)
a[3] <- nanotime(13)
expect_identical(a[3], nanotime(13))
a[1:10] <- nanotime(10:1)
expect_identical(a[1:10], nanotime(10:1))


## summary
##test_summary <- function() {
expected <- nanotime(c(1,2,2,2,3,4))
names(expected) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
expect_identical(summary(nanotime(1:4)), expected)

expected <- nanotime(c(1,2,3,3,4,5))
names(expected) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
expect_identical(summary(nanotime(1:5)), expected)


## Arith
##test_unary_minus <- function() {
expect_error(-nanotime(1), "unary '-' is not defined for \\\"nanotime\\\" objects")

##test_unary_plus <- function() {
expect_error(+nanotime(1), "unary '\\+' is not defined for \\\"nanotime\\\" objects")

##test_ANY_minus_nano <- function() {
expect_error(1 - nanotime(2), "invalid operand types")

##test_nano_minus_nanoperiod <- function() {
msg <- paste0("binary '-' is not defined for \"nanotime\" and ",
              "\"nanoperiod\" objects; instead use \"minus\\(e1, e2, tz\\)\"")
expect_error(nanotime(1) - as.nanoperiod("2m"), msg) 

##test_nano_minus_ANY <- function() {
expect_error(nanotime(1) - "a", "invalid operand types")

##test_nano_minus_nanoduration <- function() {
nt <- nanotime(2000)
expect_identical(nt - as.nanoduration(2000), nanotime(0))

## +
##test_ANY_plus_nano <- function() {
expect_error("a" + nanotime(2), "invalid operand types")

##test_nano_plus_ANY <- function() {
expect_error(nanotime(1) + "a", "invalid operand types")

##test_nano_plus_nanoperiod <- function() {
msg <- paste0("binary '\\+' is not defined for \"nanotime\" and ",
              "\"nanoperiod\" objects; instead use \"plus\\(e1, e2, tz\\)\"")
expect_error(nanotime(1) + as.nanoperiod("2m"), msg) 

##test_nano_plus_nanoduration <- function() {
nt <- nanotime(2000)
expect_identical(nt + as.nanoduration(2000), nanotime(4000))

## *
##test_nano_mul_nano <- function() {
expect_error(nanotime(1) * nanotime(1), "operation not defined for \"nanotime\" objects")

##test_nano_mul_nano <- function() {
expect_error(nanotime(1) * nanotime(2), "operation not defined for \"nanotime\" objects")

##test_nano_mul_ANY <- function() {
expect_error(nanotime(1) * 1, "operation not defined for \"nanotime\" objects")

##test_nano_div_ANY <- function() {
## div is in the same 'Arith' group as mul:
expect_error(nanotime(1) / 1, "operation not defined for \"nanotime\" objects")

##test_nano_Logic_ANY <- function() {
expect_error(nanotime(1) | 1,
             "operations are possible only for numeric, logical or complex types")

##test_ANY_Logic_nano <- function() {
expect_error(1 | nanotime(1),
             "operations are possible only for numeric, logical or complex types")

##test_Math_nano <- function() {
expect_error(sin(nanotime(1)),
             "non-numeric argument to mathematical function")

##test_Math2_nano <- function() {
expect_error(round(nanotime(1)),
             "non-numeric argument to mathematical function")


## miscellaneous
##test_is.na <- function() {
expect_true(is.na(nanotime(NA)))
expect_true(is.na(nanotime(NaN)))
nn <- nanotime(1:10)
is.na(nn) <- 1:3
expect_true(all(is.na(nn[1:3])))
expect_true(!any(is.na(nn[4:10])))
expect_true(is.na(NA_nanotime_))


## test square bracket (#44)
##test_square_bracket <- function() {
times <- c(nanotime('2011-12-05 08:30:00.000',format ="%Y-%m-%d %H:%M:%E9S",  tz ="GMT"),
           nanotime('2011-12-05 08:30:00.100',format ="%Y-%m-%d %H:%M:%E9S",  tz ="GMT"),
           nanotime('2011-12-05 08:30:00.825',format ="%Y-%m-%d %H:%M:%E9S",  tz ="GMT"))
expect_identical(times[1], times[[1]])
expect_identical(times[2], times[[2]])
expect_identical(times[3], times[[3]])

## test compare
expect_true(nanotime(1) == nanotime(1))
expect_true(nanotime(1) != nanotime(2))
expect_true(nanotime(1) <= nanotime(1))
expect_true(nanotime(1) >= nanotime(1))
expect_true(nanotime(1) <= nanotime(2))
expect_false(nanotime(1) >= nanotime(2))
expect_true(nanotime(1) < nanotime(2))
expect_false(nanotime(1) > nanotime(2))

## with character
expect_true(nanotime(1) == nanotime("1970-01-01T00:00:00.000000001+00"))
expect_true(nanotime("1970-01-01T00:00:00.000000001+00") != nanotime(2))
expect_true(nanotime("1970-01-01T00:00:00.000000001+00") <= nanotime(1))
expect_true(nanotime("1970-01-01T00:00:00.000000001+00") >= nanotime(1))
expect_true(nanotime(1) <= nanotime("1970-01-01T00:00:00.000000002+00"))
expect_false(nanotime("1970-01-01T00:00:00.000000001+00") >= nanotime(2))
expect_true(nanotime(1) < nanotime("1970-01-01T00:00:00.000000002+00"))
expect_false(nanotime("1970-01-01T00:00:00.000000001+00") > nanotime(2))

## with POSIXt
pt1 <- as.POSIXct(1, origin="1970-01-01", tz="UTC")
pt2 <- as.POSIXct(2, origin="1970-01-01", tz="UTC")
expect_true(pt1 == nanotime("1970-01-01T00:00:01+00"))
expect_true(nanotime("1970-01-01T00:00:01+00") != pt2)
expect_true(nanotime("1970-01-01T00:00:01+00") <= pt1)
expect_true(nanotime("1970-01-01T00:00:01+00") >= pt1)
expect_true(nanotime(1) <= nanotime("1970-01-01T00:00:02+00"))
expect_false(nanotime("1970-01-01T00:00:01+00") >= pt2)
expect_true(nanotime(1) < nanotime("1970-01-01T00:00:00.000000002+00"))
expect_false(nanotime("1970-01-01T00:00:01+00") > pt2)


## all.equal
##test_all.equal_nanotime_any <- function() {
expect_true(!isTRUE(all.equal(nanotime(1), "a")))

##test_all.equal_any_nanotime <- function() {
expect_true(!isTRUE(all.equal("a", nanotime(1))))

##test_all.equal_nanotime_nanotime <- function() {
expect_true(isTRUE(all.equal(nanotime(1), nanotime(1))))
## LLL waiting for a fix from 'bit64'
## expect_true(!isTRUE(all.equal(nanotime(1), nanotime(2))))  


## test S4 conversions:

## to 'nanotime':
expect_identical(as("2018-01-01T05:00:00.99 Europe/London", "nanotime"), nanotime(as.integer64("1514782800990000000")))
expect_identical(as(as.Date(10, origin="1970-01-01"), "nanotime"), nanotime("1970-01-11T00:00:00.000000000-00:00"))
expect_identical(as(as.integer64("1514782800990000000"), "nanotime"), nanotime("2018-01-01T05:00:00.99 Europe/London"))
expect_identical(as(as.POSIXct(1514782800, origin="1970-01-01", tz="UTC"), "nanotime"), nanotime("2018-01-01T05:00:00 Europe/London"))
expect_identical(as(as.POSIXlt(as.POSIXct(1514782800, origin="1970-01-01", tz="UTC")), "nanotime"), nanotime("2018-01-01T05:00:00 Europe/London"))

## from 'nanotime':
expect_identical(as(nanotime("1970-01-11T00:00:00.000000000-00:00"), "Date"), as.Date("1970-01-11"))
## don't run this one, it's only right when run in America/New_York timezone!
## expect_identical(as(nanotime("1970-01-11T00:00:00.000000000-00:00"), "POSIXct"), as.POSIXct("1970-01-10 19:00:00"))
expect_identical(as(nanotime("1970-01-11T00:00:00.000000000-00:00"), "integer64"), as.integer64("864000000000000"))
expect_identical(as(nanotime("1970-01-11T00:00:00.000000000-00:00"), "nanoduration"), as.nanoduration("240:00:00"))


## test nanotime sequences:

## straightforward with integer64 and nanoduration1
expect_identical(seq(as.nanotime(1), by=1e9, length.out=10), as.nanotime(seq(1, by=1e9, length.out=10)))
expect_identical(seq(as.nanotime(1), by=1e9, along.with=1:10), as.nanotime(seq(1, by=1e9, length.out=10)))
expect_identical(seq(from=as.nanotime(1), to=as.nanotime(10e9), by=1e9), as.nanotime(seq(1, 10e9, by=1e9)))
expect_identical(seq(from=as.nanotime(1), to=as.nanotime(9e9+1), length.out=10), as.nanotime(seq(1, 10e9, by=1e9)))
expect_identical(seq(as.nanotime(1), by=as.nanoduration(1e9), length.out=10), as.nanotime(seq(1, by=1e9, length.out=10)))
expect_identical(seq(as.nanotime(1), as.nanotime(10e9), by=as.nanoduration(1e9)), as.nanotime(seq(1, 10e9, by=1e9)))

## increment with a period:
expect_identical(seq(as.nanotime(0), by=as.nanoperiod("1m"), length.out=4, tz="UTC"),
                 as.nanotime(c("1970-01-01T00:00:00+00:00", "1970-02-01T00:00:00+00:00",
                               "1970-03-01T00:00:00+00:00", "1970-04-01T00:00:00+00:00")))
expect_identical(seq(as.nanotime(0), by=as.nanoperiod("1m"), length.out=0, tz="UTC"),
                 nanotime())
expect_identical(seq(as.nanotime(0), to=as.nanotime("1970-04-01T00:00:00+00:00"),
                     by=as.nanoperiod("1m"), tz="UTC"),
                 as.nanotime(c("1970-01-01T00:00:00+00:00", "1970-02-01T00:00:00+00:00",
                               "1970-03-01T00:00:00+00:00", "1970-04-01T00:00:00+00:00")))

## other seq errors or warnings:
expect_warning(seq(as.nanotime(1), by=1e9, length.out=10:12), "first element used of 'length.out' argument")
expect_error(seq(as.nanotime(0), by=as.nanoperiod(c("1m","2m")), length.out=4, tz="UTC"),
             "'by' must be of length 1")
expect_error(seq(as.nanotime(0), by=as.nanoperiod("1m"), length.out=-1, tz="UTC"),
             "'length.out' must be a non-negative number")
expect_error(seq(from=as.nanotime(1)), "'seq.nanotime' cannot be called with only one argument")
expect_error(seq(from=as.nanotime(1), to=as.nanotime(2), length.out=as.integer()), "argument 'length.out' must be of length 1")
expect_error(seq(from=as.nanotime(1), length.out=1),
             "at least two of 'to', 'by', and 'length.out' or 'along.with' must be specified")
expect_error(seq(from=as.nanotime(1), to=as.nanoduration(10e9), by=1e9),
             "'to' must be a 'nanotime'")
expect_error(seq(from=as.nanotime(1:2), to=as.nanotime(1), by=1e9),
             "'from' must be of length 1")
expect_error(seq(from=as.nanotime(1), to=as.nanotime(1:2), by=1e9),
             "'to' must be of length 1")          
expect_error(seq(from=as.nanotime(1), to=as.nanotime(2), by=1e9, length.out=1, too_much=2),
             "too many arguments")
expect_error(seq(as.nanotime(1), by=c(1e9,2), length.out=10), "'by' must be of length 1")
expect_error(seq(as.nanotime(1), to=as.nanotime(10), by=c(1e9,2)), "'by' must be of length 1")
expect_error(seq(as.nanotime(1), by=as.nanoperiod("1d"), length.out=10), "'tz' is a mandatory argument for sequences with a 'period' step")
expect_error(seq(as.nanotime(1), to=as.nanotime(10), by=as.nanoperiod("1d")), "'tz' is a mandatory argument for sequences with a 'period' step")
