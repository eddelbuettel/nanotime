
<div role="main">

## Nanosecond resolution datetime functionality

### Description

Functions to operate on nanosecond time resolution using integer64 bit
representation. Conversion functions for several standard R types are
provided, and more will be added as needed.

### Usage

``` R
nanotime(from, ...)

as.nanotime(from, ...)

## S4 method for signature 'character'
nanotime(from, format = "", tz = "")

## S4 method for signature 'character'
as.nanotime(from, format = "", tz = "")

nanotime.matrix(x)

## S4 method for signature 'POSIXct'
nanotime(from, accurate = TRUE)

## S4 method for signature 'POSIXct'
as.nanotime(from, accurate = TRUE)

## S4 method for signature 'POSIXlt'
nanotime(from)

## S4 method for signature 'POSIXlt'
as.nanotime(from)

## S4 method for signature 'Date'
nanotime(from)

## S4 method for signature 'Date'
as.nanotime(from)

## S4 method for signature 'nanotime'
print(x, format = "", tz = "", quote = FALSE, ...)

## S4 method for signature 'nanotime'
show(object)

## S3 method for class 'nanotime'
format(x, format = "", tz = "", ...)

## S3 method for class 'nanotime'
index2char(x, ...)

## S3 method for class 'nanotime'
as.POSIXct(x, tz = "", ...)

## S3 method for class 'nanotime'
as.POSIXlt(x, tz = "", ...)

## S3 method for class 'nanotime'
as.Date(x, ...)

## S3 method for class 'nanotime'
as.data.frame(x, ...)

## S3 method for class 'nanotime'
as.integer64(x, ...)

## S4 method for signature 'nanotime,character'
e1 - e2

## S4 method for signature 'nanotime,nanotime'
e1 - e2

## S4 method for signature 'nanotime,integer64'
e1 - e2

## S4 method for signature 'nanotime,numeric'
e1 - e2

## S4 method for signature 'ANY,nanotime'
e1 - e2

## S4 method for signature 'nanotime,ANY'
e1 - e2

## S4 method for signature 'nanotime,ANY'
e1 + e2

## S4 method for signature 'nanotime,integer64'
e1 + e2

## S4 method for signature 'nanotime,numeric'
e1 + e2

## S4 method for signature 'ANY,nanotime'
e1 + e2

## S4 method for signature 'integer64,nanotime'
e1 + e2

## S4 method for signature 'numeric,nanotime'
e1 + e2

## S4 method for signature 'nanotime,nanotime'
e1 + e2

## S4 method for signature 'nanotime,nanotime'
Arith(e1, e2)

## S4 method for signature 'nanotime,ANY'
Arith(e1, e2)

## S4 method for signature 'ANY,nanotime'
Arith(e1, e2)

## S4 method for signature 'nanotime,character'
Compare(e1, e2)

## S4 method for signature 'character,nanotime'
Compare(e1, e2)

## S4 method for signature 'nanotime,POSIXt'
Compare(e1, e2)

## S4 method for signature 'POSIXt,nanotime'
Compare(e1, e2)

## S4 method for signature 'nanotime,ANY'
Compare(e1, e2)

## S4 method for signature 'nanotime,ANY'
Logic(e1, e2)

## S4 method for signature 'ANY,nanotime'
Logic(e1, e2)

## S4 method for signature 'nanotime'
Math(x)

## S4 method for signature 'nanotime'
Math2(x, digits)

## S4 method for signature 'nanotime'
Summary(x, ..., na.rm = FALSE)

## S4 method for signature 'nanotime'
min(x, ..., na.rm = FALSE)

## S4 method for signature 'nanotime'
max(x, ..., na.rm = FALSE)

## S4 method for signature 'nanotime'
range(x, ..., na.rm = FALSE)

## S4 method for signature 'nanotime'
Complex(z)

## S4 method for signature 'nanotime'
x[[i, j, ..., drop = FALSE]]

## S4 method for signature 'nanotime,numeric'
x[i, j, ..., drop = FALSE]

## S4 method for signature 'nanotime,logical'
x[i, j, ..., drop = FALSE]

## S4 method for signature 'nanotime,character'
x[i, j, ..., drop = FALSE]

## S4 method for signature 'nanotime,ANY'
x[i, j, ..., drop = FALSE]

## S4 replacement method for signature 'nanotime,ANY,ANY,ANY'
x[i, j, ...] <- value

## S3 method for class 'nanotime'
c(...)

## S4 replacement method for signature 'nanotime'
names(x) <- value

## S4 method for signature 'nanotime'
is.na(x)

NA_nanotime_

## S3 method for class 'nanotime'
as.character(x, ...)

## S3 method for class 'nanoduration'
as.data.frame(x, ...)
```

### Arguments

|             |                                                                                                                                                                                                                                                                                                         |
|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `...`       | further arguments passed to or from methods.                                                                                                                                                                                                                                                            |
| `format`    | A character string. Can also be set via `options("nanotimeFormat")` and uses ‘%Y-%m-%dT%H:%M:%E9S%Ez’ as a default and fallback                                                                                                                                                                         |
| `tz`        | character specifying a timezone which is required for `as.POSIXct`, `as.POSIXlt` and can be specified for `as.nanotime`, `format` and `print`; it can also be set via `options("nanotimeTz")` and uses ‘UTC’ as a default and fallback                                                                  |
| `x`, `from` | `nanotime` objects                                                                                                                                                                                                                                                                                      |
| `accurate`  | in the conversion from `POSIXct` to `nanotime`, indicates if one wants to preserve the maximum precision possible; the default is `TRUE`, but in most situations the loss of precision is negligible, and setting this parameter to `TRUE` will make the conversion nearly an order of magnitude faster |
| `quote`     | indicates if the output of `print` should be quoted                                                                                                                                                                                                                                                     |
| `object`    | argument for method `show`                                                                                                                                                                                                                                                                              |
| `e1`        | Operand of class `nanotime`                                                                                                                                                                                                                                                                             |
| `e2`        | Operand of class `nanotime`                                                                                                                                                                                                                                                                             |
| `digits`    | Required for `Math2` signature but ignored here                                                                                                                                                                                                                                                         |
| `na.rm`     | a logical indicating whether missing values should be removed.                                                                                                                                                                                                                                          |
| `z`         | Required for `Complex` signature but ignored here                                                                                                                                                                                                                                                       |
| `i`         | index specifying elements to extract or replace.                                                                                                                                                                                                                                                        |
| `j`         | Required for `[` signature but ignored here                                                                                                                                                                                                                                                             |
| `drop`      | Required for `[` signature but ignored here                                                                                                                                                                                                                                                             |
| `value`     | argument for `nanotime-class`                                                                                                                                                                                                                                                                           |

### Format

An object of class `nanotime` of length 1.

### Details

Notice that the conversion from POSIXct explicitly sets the last three
digits to zero. Nanosecond time stored in a 64-bit integer has nineteen
digits precision where doubles (which are used internally for POSIXct as
well) only have sixteen digits. So rather than showing three more
(essentially *random*) digits it is constructed such that these three
additional digits are zeros.

### Value

A nanotime object

### Caveats

Working with dates and times is *difficult*. One needs a representation
of both *time points* and *time duration*. In R, think of `Date` or
`POSIXct` objects for the former, and `difftime` for the later. Here we
have time points `nanotime`, an interval type `nanoival` and two flavors
of duration which are a simple count of nanoseconds `nanoduration` and a
calendar duration that is able to track concepts such as months and days
`nanoperiod`. Point in time and intervals are all based on durations
relative to the epoch of January 1, 1970.

### Input and Output Format

Formatting and character conversion for `nanotime` objects is done by
functions from the <span class="pkg">RcppCCTZ</span> package relying on
code from its embedded `CCTZ` library. The default format is ISO3339
compliant: `%Y-%m-%dT%H:%M:%E9S%Ez`. It specifies a standard ISO 8601
part for date and time — as well as nine digits of precision for
fractional seconds (down to nanoseconds) and on offset (typically zero
as we default to UTC). It can be overriden by using `options()` with the
key of `nanotimeFormat` and a suitable value. Similarly, `nanotimeTz`
can be used to select a different timezone.

For input, some slack it cut, and various shortened formats are accepted
by default such as `2020-03-10` or `2020-03-10 18:16:00`, or
`2020-03-10 18:16:00.001` (and the ‘T’ separator is optional.

### `tz` parameter usage in constructors

The `tz` parameter is allowed only when constructing a `nanotime` from a
`character`. This is because any `numeric`, `Date` and `POSIXct` is de
facto considered an offset since the epoch. On the contrary, a
`character` is considered interpretable and hence if it does not contain
a timezone in its representation, it is possible to specify the `tz`
argument to specify in which timezone it should be interpreted. This is
useful in particular if one wants to convert a `Date` to be aligned to
the beginning of the day in a specific timezone; in this case one should
convert the `Date` to a `character` before calling the `nanotime`
constructor with the desired timezone.

### Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

### See Also

`nanoival`, `nanoduration`, `nanoperiod`, `seq.nanotime` as well as the
documentation in package <span class="pkg">RcppCCTZ</span>.

### Examples

``` R
## Not run: 
x <- nanotime(1)
print(x)
as.nanotime("1970-01-01T00:00:00.000000001+00:00")
as.nanotime("2020-03-10 Europe/Berlin")
as.nanotime("2020-03-10 18:31:23.001", tz="America/New_York")
as.nanotime("2020-03-10T040947190301440", format="%Y-%m-%dT%H%M%S%E*f")
x <- x + 1
print(x)
format(x)
x <- x + 10
print(x)
format(x)
nanotime(Sys.time()) + 1:3  # three elements each 1 ns apart
seq(x, by=as.nanoperiod("1d"), length.out=5, tz="Asia/Tokyo")

## End(Not run)
```


