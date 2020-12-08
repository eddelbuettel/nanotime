## Period type with nanosecond precision

### Description

`nanoperiod` is a length of time type (implemented as an S4 class) with
nanosecond precision. It differs from `nanoduration` because it is
capable of representing calendar months and days. It can thus represent
years (12 months) and weeks (7 days). A period is a somewhat abstract
representation of time: it is only when anchored to a point in time and
in a specific time zone that it is possible to convert it to a specific
duration. This means that many of the operations involving periods need
the additional argument `tz`.

### Usage

    nanoperiod(months = 0, days = 0, duration = as.nanoduration(0))

    ## S4 method for signature 'character'
    as.nanoperiod(x)

    ## S4 method for signature 'integer64'
    as.nanoperiod(x)

    ## S4 method for signature 'numeric'
    as.nanoperiod(x)

    ## S4 method for signature 'integer'
    as.nanoperiod(x)

    ## S4 method for signature 'nanoduration'
    as.nanoperiod(x)

    ## S4 method for signature ''NULL''
    as.nanoperiod(x)

    ## S4 method for signature 'missing'
    as.nanoperiod(x)

    ## S4 method for signature 'nanoperiod'
    show(object)

    ## S4 method for signature 'nanoperiod'
    print(x, quote = FALSE, ...)

    ## S3 method for class 'nanoperiod'
    format(x, ...)

    ## S4 method for signature 'nanoperiod'
    as.character(x)

    ## S4 method for signature 'nanoperiod'
    is.na(x)

    ## S4 replacement method for signature 'nanoperiod'
    is.na(x) <- value

    ## S4 method for signature 'nanoperiod'
    x[[i, j, ..., drop = FALSE]]

    ## S4 method for signature 'nanoperiod,numeric'
    x[i, j, ..., drop = FALSE]

    ## S4 method for signature 'nanoperiod,logical'
    x[i, j, ..., drop = FALSE]

    ## S4 method for signature 'nanoperiod,character'
    x[i, j, ..., drop = FALSE]

    ## S4 method for signature 'nanoperiod,ANY'
    x[i, j, ..., drop = FALSE]

    ## S4 replacement method for signature 'nanoperiod,ANY,ANY,ANY'
    x[i, j, ...] <- value

    ## S3 method for class 'nanoperiod'
    c(...)

    ## S4 method for signature 'nanoperiod'
    names(x)

    ## S4 replacement method for signature 'nanoperiod'
    names(x) <- value

    ## S4 method for signature 'nanoperiod,ANY'
    e1 - e2

    ## S4 method for signature 'nanoperiod,nanoperiod'
    e1 - e2

    ## S4 method for signature 'nanoperiod,nanoduration'
    e1 - e2

    ## S4 method for signature 'nanoperiod,integer64'
    e1 - e2

    ## S4 method for signature 'nanoperiod,numeric'
    e1 - e2

    ## S4 method for signature 'nanoduration,nanoperiod'
    e1 - e2

    ## S4 method for signature 'integer64,nanoperiod'
    e1 - e2

    ## S4 method for signature 'numeric,nanoperiod'
    e1 - e2

    ## S4 method for signature 'nanoperiod,ANY'
    e1 + e2

    ## S4 method for signature 'nanoperiod,nanoperiod'
    e1 + e2

    ## S4 method for signature 'nanoperiod,nanoduration'
    e1 + e2

    ## S4 method for signature 'nanoperiod,integer64'
    e1 + e2

    ## S4 method for signature 'nanoperiod,nanotime'
    e1 + e2

    ## S4 method for signature 'nanoival,nanoperiod'
    e1 + e2

    ## S4 method for signature 'nanoperiod,nanoival'
    e1 + e2

    ## S4 method for signature 'nanotime,nanoperiod'
    e1 + e2

    ## S4 method for signature 'nanoperiod,numeric'
    e1 + e2

    ## S4 method for signature 'nanoduration,nanoperiod'
    e1 + e2

    ## S4 method for signature 'integer64,nanoperiod'
    e1 + e2

    ## S4 method for signature 'numeric,nanoperiod'
    e1 + e2

    ## S4 method for signature 'nanoperiod,integer64'
    e1 * e2

    ## S4 method for signature 'nanoperiod,numeric'
    e1 * e2

    ## S4 method for signature 'integer64,nanoperiod'
    e1 * e2

    ## S4 method for signature 'numeric,nanoperiod'
    e1 * e2

    ## S4 method for signature 'nanoperiod,integer64'
    e1 / e2

    ## S4 method for signature 'nanoperiod,numeric'
    e1 / e2

    ## S4 method for signature 'nanoperiod,nanoperiod'
    e1 == e2

    ## S4 method for signature 'nanoperiod,nanoperiod'
    e1 != e2

    ## S4 method for signature 'nanotime,nanoperiod,character'
    plus(e1, e2, tz)

    ## S4 method for signature 'nanoperiod,nanotime,character'
    plus(e1, e2, tz)

    ## S4 method for signature 'nanotime,nanoperiod,character'
    minus(e1, e2, tz)

    ## S4 method for signature 'nanoperiod,nanotime,character'
    minus(e1, e2, tz)

    ## S4 method for signature 'nanoival,nanoperiod,character'
    plus(e1, e2, tz)

    ## S4 method for signature 'nanoperiod,nanoival,character'
    plus(e1, e2, tz)

    ## S4 method for signature 'nanoival,nanoperiod,character'
    minus(e1, e2, tz)

    NA_nanoperiod_

### Arguments

| Argument   | Description                                                                    |
|------------|--------------------------------------------------------------------------------|
| `months`   | Used in the constructor to indicate the number of months of the `nanoperiod`   |
| `days`     | Used in the constructor to indicate the number of days of the `nanoperiod`     |
| `duration` | Used in the constructor to indicate the duration component of the `nanoperiod` |
| `x, value` | An object of class `nanoperiod`                                                |
| `object`   | argument for method `show`                                                     |
| `quote`    | indicates if the output of `print` should be quoted                            |
| `...`      | further arguments                                                              |
| `i`        | index specifying elements to extract or replace.                               |
| `j`        | Required for `[` signature but ignored here                                    |
| `drop`     | Required for `[` signature but ignored here                                    |
| `e1`       | Operand of class `nanoperiod`                                                  |
| `e2`       | Operand of class `nanoperiod`                                                  |
| `tz`       | `character` indicating a timezone                                              |

### Format

An object of class `nanoperiod` of length 1.

### Constructors

The true constructor is

### Output Format

A `nanoperiod` is displayed as months, days, and `nanoduration` like
this: `10m2d/10:12:34.123_453_000`.

### Details

Adding or subtracting `nanoperiod` and `nanotime` require a timezone as
third argument. For this reason it is not possible to use the binary
operator 'code+'. Instead the functions '`plus`' and '`minus`' are
defined.

### Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

### See Also

`nanotime`, `nanoduration`, `nanoival`,
`nanoperiod.month,nanoperiod-method`

### Examples

    ## Not run: 
    p <- nanoperiod(months=12, days=7, duration="01:00:00")
    print(p)

    #  when adding a \code{nanoperiod} to a \code{nanotime} or to a
    # \code{nanoival}, a time zone must be specified:
    y <- nanotime("1970-01-01T00:00:00+00:00")
    plus(y, p, tz="America/Chicago")

    ## End(Not run)
