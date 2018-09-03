
isSolaris <- Sys.info()[["sysname"]] == "SunOS"

## nanotime constructors
test_nanotime_generic <- function() {
    checkEquals(S3Part(nanotime(1), strict=TRUE), as.integer64(1))
    checkEquals(nanotime(1), new("nanotime", as.integer64(1)))
}
test_nanotime_character <- function() {
    if (!isSolaris) {
        checkEquals(nanotime("1970-01-01T00:00:00.000000001+00:00"), nanotime(1))
        oldFormat <- getOption("nanotimeFormat")
        oldTz <- getOption("nanotimeTz")
        ## check that the format and time zone is picked up from the global options:
        options(nanotimeFormat="%Y-%m-%d %H:%M")
        options(nanotimeTz="America/New_York")
        checkEquals(nanotime("1970-01-01 00:00"), nanotime(18000000000000))
        ## check they are overridden by the parameters:
        checkEquals(nanotime("1970-01-01 00:00:01", format="%Y-%m-%d %H:%M:%S", tz="Europe/Paris"),
                    nanotime(-3599000000000))
        options(nanotimeFormat=oldFormat)
        options(nanotimeTz=oldTz)
    }
}
test_nanotime_matrix <- function() {
    if (!isSolaris) {
        m <- matrix(c(10*24*3600+0:9, 9897654321+0:9), 10, 2)
        checkEquals(nanotime.matrix(m), nanotime("1970-01-11T00:00:00.987654321+00:00")+0:9)
    }
}
test_nanotime_POSIXct <- function() {
    p <- as.POSIXct("1970-01-01 00:00:00", tz="America/New_York")
    checkEquals(nanotime(p), nanotime("1970-01-01T00:00:00.000000000-05:00"))
}
test_nanotime_POSIXlt <- function() {
    l <- as.POSIXlt("1970-01-01 00:00:00", tz="America/New_York")
    checkEquals(nanotime(l), nanotime("1970-01-01T00:00:00.000000000-05:00"))
}
test_nanotime_Date <- function() {
    d <- as.Date(10, origin="1970-01-01")
    checkEquals(nanotime(d), nanotime("1970-01-11T00:00:00.000000000-05:00"))
}
test_nanotime_numeric_keep_names <- function() {
    n <- nanotime(c(a=1, b=2))
    checkEquals(names(n), c("a","b"))
}
test_nanotime_character_keep_names <- function() {
    if (!isSolaris) {
        n <- nanotime(c(a="1970-01-01T00:00:00.000000001+00:00",
                        b="1970-01-01T00:00:00.000000001+00:00"))
        checkEquals(names(n), c("a","b"))
    }
}
test_nanotime_POSIXct_keep_names <- function() {
    p <- as.POSIXct("1970-01-01 00:00:00", tz="America/New_York")
    n <- nanotime(c(a=p, b=p))
    checkEquals(names(n), c("a","b"))
}
test_nanotime_POSIXlt_keep_names <- function() {
    l <- as.POSIXlt("1970-01-01 00:00:00", tz="America/New_York")
    n <- nanotime(c(a=l, b=l))
    checkEquals(names(n), c("a","b"))
}

## format
test_format_default <- function() {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)
    options(nanotimeTz=NULL)
    checkEquals(format(nanotime("1970-01-01T00:00:00.000000000+00:00")),
                "1970-01-01T00:00:00.000000000+00:00")
    checkEquals(format(nanotime("1680-07-17T00:00:01.000000000+00:00")),
                "1680-07-17T00:00:01.000000000+00:00")
    checkEquals(format(nanotime("2120-01-01T00:00:59.987654321+00:00")),
                "2120-01-01T00:00:59.987654321+00:00")
    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}
test_format_tz <- function() {
    if (!isSolaris) {
        oldFormat <- getOption("nanotimeFormat")
        oldTz <- getOption("nanotimeTz")
        options(nanotimeFormat=NULL)

        a_utc = "1970-01-01T00:00:00.000000000+00:00"
        nt <- nanotime(a_utc)
        a_ny <- "1969-12-31T19:00:00.000000000-05:00"

        options(nanotimeTz=NULL)
        checkEquals(format(nt, tz="America/New_York"), a_ny)

        options(nanotimeTz="UTC")
        checkEquals(format(nt, tz="America/New_York"), a_ny)

        attr(nt, "tzone") <- "UTC"
        checkEquals(format(nt, tz="America/New_York"), a_ny)
        checkEquals(format(nt, tz=""), a_utc)

        options(nanotimeFormat=oldFormat)
        options(nanotimeTz=oldTz)
    }
}
test_format_tzone <- function() {
    if (!isSolaris) {
        oldFormat <- getOption("nanotimeFormat")
        oldTz <- getOption("nanotimeTz")
        options(nanotimeFormat=NULL)

        a <- nanotime("1970-01-01T00:00:00.000000000+00:00")
        attr(a, "tzone") <- "America/New_York"
        a_ny <- "1969-12-31T19:00:00.000000000-05:00"

        checkEquals(format(a), a_ny)

        options(nanotimeTz="UTC")
        checkEquals(format(a), a_ny)

        options(nanotimeFormat=oldFormat)
        options(nanotimeTz=oldTz)
    }
}
test_format_tz_from_options <- function() {
    if (!isSolaris) {
        oldFormat <- getOption("nanotimeFormat")
        oldTz <- getOption("nanotimeTz")
        options(nanotimeFormat=NULL)
        options(nanotimeTz="America/New_York")

        a <- nanotime("1970-01-01T00:00:00.000000000+00:00")
        a_ny <- "1969-12-31T19:00:00.000000000-05:00"

        checkEquals(format(a), a_ny)

        options(nanotimeFormat=oldFormat)
        options(nanotimeTz=oldTz)
    }
}
test_format_fmt_default <- function() {
    oldFormat <- getOption("nanotimeFormat")
    oldTz <- getOption("nanotimeTz")
    options(nanotimeFormat=NULL)
    options(nanotimeTz=NULL)

    a_str <- "1970-01-01T00:00:00.000000000+00:00"
    a <- nanotime(a_str)

    checkEquals(format(a), a_str)

    options(nanotimeFormat=oldFormat)
    options(nanotimeTz=oldTz)
}
test_format_fmt_from_options <- function() {
    if (!isSolaris) {
        oldFormat <- getOption("nanotimeFormat")
        oldTz <- getOption("nanotimeTz")
        options(nanotimeFormat="%Y/%m/%dT%H:%M:%E9S%Ez")
        options(nanotimeTz="America/New_York")

        a <- nanotime("1970/01/01T00:00:00.000000000+00:00")
        a_ny <- "1969/12/31T19:00:00.000000000-05:00"

        checkEquals(format(a), a_ny)

        options(nanotimeFormat=oldFormat)
        options(nanotimeTz=oldTz)
    }
}
test_format_fmt_from_parameter <- function() {
    if (!isSolaris) {
        oldFormat <- getOption("nanotimeFormat")
        oldTz <- getOption("nanotimeTz")
        options(nanotimeFormat="%Y/%m/%dT%H:%M:%E9S%Ez")
        options(nanotimeTz="America/New_York")

        a <- nanotime("1970-01-01 00:00:00.000000000+00:00", format="%Y-%m-%d %H:%M:%E9S%Ez")
                                        # the result of format on a is taken from the global option:
        a_ny <- "1969/12/31T19:00:00.000000000-05:00"

        checkEquals(format(a), a_ny)

        options(nanotimeFormat=oldFormat)
        options(nanotimeTz=oldTz)
    }
}
test_format_na <- function() {
    if (!isSolaris) {
        f <- format(nanotime(c(1, NA, 2, NaN)))
        exp <- c("1970-01-01T00:00:00.000000001+00:00",
                 as.character(NA),
                 "1970-01-01T00:00:00.000000002+00:00",
                 as.character(NA))
        checkEquals(f, exp)
    }
}

## conversions
test_as_POSIXct <- function() {
    a <- nanotime(0)
    attr(a, "tzone") <- "America/New_York"
    p <- as.POSIXct(a)
    checkEquals(p, as.POSIXct("1969-12-31 19:00:00", tz="America/New_York"))

    oldTz <- getOption("nanotimeTz")
    options(nanotimeTz="America/New_York")
    b <- nanotime(0)
    p <- as.POSIXct(b)
    checkEquals(p, as.POSIXct("1969-12-31 19:00:00", tz="America/New_York"))

    options(nanotimeTz=NULL)
    c <- nanotime(0)
    p <- as.POSIXct(c)
    checkEquals(p, as.POSIXct("1970-01-01 00:00:00", tz="UTC"))

    options(nanotimeTz=oldTz)
}
test_as_POSIXct <- function() {
    a <- nanotime(0)
    attr(a, "tzone") <- "America/New_York"
    p <- as.POSIXlt(a)
    checkEquals(p, as.POSIXlt("1969-12-31 19:00:00", tz="America/New_York"))

    oldTz <- getOption("nanotimeTz")
    options(nanotimeTz="America/New_York")
    b <- nanotime(0)
    p <- as.POSIXlt(b)
    checkEquals(p, as.POSIXlt("1969-12-31 19:00:00", tz="America/New_York"))

    options(nanotimeTz=NULL)
    c <- nanotime(0)
    p <- as.POSIXlt(c)
    checkEquals(p, as.POSIXlt("1970-01-01 00:00:00", tz="UTC"))

    options(nanotimeTz=oldTz)
}
test_as_Date <- function() {
    checkEquals(as.Date(nanotime(0)), as.Date("1970-01-01"))
}

## c, subset, subassign and binds
test_c <- function() {
    a <- c(nanotime(1), nanotime(2))
    checkEquals(a, nanotime(1:2))

    a <- c(nanotime(1:2), nanotime(3:4))
    checkEquals(a, nanotime(1:4))
}
test_subset <- function() {
    a <- nanotime(1:10)
    checkEquals(a[3], nanotime(3))
    checkEquals(a[1:10], a)
}
test_subsassign <- function() {
    a <- nanotime(1:10)
    a[3] <- nanotime(13)
    checkEquals(a[3], nanotime(13))
    a[1:10] <- nanotime(10:1)
    checkEquals(a[1:10], nanotime(10:1))
}

## summary
test_summary <- function() {
    expected <- nanotime(c(1,2,2,2,3,4))
    names(expected) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    checkEquals(summary(nanotime(1:4)), expected)

    expected <- nanotime(c(1,2,3,3,4,5))
    names(expected) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    checkEquals(summary(nanotime(1:5)), expected)
}

## operation exceptions
test_unary_minus <- function() {
    checkException(-nanotime(1),
                   msg="-nanotime(1) : unary '-' is not defined for \"nanotime\" objects")
}
test_unary_plus <- function() {
    checkException(-nanotime(1),
                   msg="+nanotime(1) : unary '+' is not defined for \"nanotime\" objects")
}
test_binary_mul <- function() {
    checkException(nanotime(1) * nanotime(2),
                   msg="operation is not defined for \"nanotime\" objects")
}
test_ANY_minus_nano <- function() {
    checkException(1 - nanotime(2), msg="invalid operand types")
}
test_nano_minus_ANY <- function() {
    checkException(nanotime(1) - "a", msg="invalid operand types")
}
test_ANY_plus_nano <- function() {
    checkException("a" + nanotime(2), msg="invalid operand types")
}
test_nano_plus_ANY <- function() {
    checkException(nanotime(1) + "a", msg="invalid operand types")
}
test_nano_mul_nano <- function() {
    checkException(nanotime(1) * nanotime(1),
                   msg="operation not defined for \"nanotime\" objects")
}
test_nano_mul_ANY <- function() {
    checkException(nanotime(1) * 1, msg="operation not defined for \"nanotime\" objects")
}
test_nano_div_ANY <- function() {
    ## div is in the same 'Arith' group as mul:
    checkException(nanotime(1) / 1, msg="operation not defined for \"nanotime\" objects")
}
test_nano_Logic_ANY <- function() {
    checkException(nanotime(1) | 1,
                   msg="operations are possible only for numeric, logical or complex types")
}
test_ANY_Logic_nano <- function() {
    checkException(1 | nanotime(1),
                   msg="operations are possible only for numeric, logical or complex types")
}
test_Math_nano <- function() {
    checkException(sin(nanotime(1)),
                   msg="non-numeric argument to mathematical function")
}
test_Math2_nano <- function() {
    checkException(round(nanotime(1)),
                   msg="non-numeric argument to mathematical function")
}

## miscellaneous
test_is.na <- function() {
    checkEquals(is.na(nanotime(NA)), TRUE)
    checkEquals(is.na(nanotime(NaN)), TRUE)
}
