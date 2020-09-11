## Interval type with nanosecond precision

### Description

`nanoival` is a time interval type (an S4 class) with nanosecond
precision. One of its purposes is to allow quick subsetting of a
`nanotime` vector. `nanoival` is composed of a `nanotime` pair which
defines the start and end of the time interval. Additionally, it has a
pair of logical values which determine if the start and end of the time
interval are open (true) or closed (false).

### Usage

    nanoival(start, end, sopen = FALSE, eopen = TRUE)
    
    ## S4 method for signature 'nanoival'
    nanoival.start(x)
    
    ## S4 method for signature 'nanoival'
    nanoival.end(x)
    
    ## S4 method for signature 'nanoival'
    nanoival.sopen(x)
    
    ## S4 method for signature 'nanoival'
    nanoival.eopen(x)
    
    ## S3 method for class 'nanoival'
    format(x, ...)
    
    ## S4 method for signature 'nanoival'
    print(x, quote = FALSE, ...)
    
    ## S4 method for signature 'nanoival'
    show(object)
    
    ## S4 method for signature 'character'
    as.nanoival(from, format = "", tz = "")
    
    ## S4 method for signature ''NULL''
    as.nanoival(from, format = "", tz = "")
    
    ## S4 method for signature 'missing'
    as.nanoival(from, format = "", tz = "")
    
    ## S4 method for signature 'nanoival'
    is.na(x)
    
    ## S4 replacement method for signature 'nanoival'
    is.na(x) <- value
    
    ## S4 method for signature 'nanoival,nanoival'
    e1 < e2
    
    ## S4 method for signature 'nanoival,nanoival'
    e1 <= e2
    
    ## S4 method for signature 'nanoival,nanoival'
    e1 > e2
    
    ## S4 method for signature 'nanoival,nanoival'
    e1 >= e2
    
    ## S4 method for signature 'nanoival,nanoival'
    e1 == e2
    
    ## S4 method for signature 'nanoival,nanoival'
    e1 != e2
    
    ## S4 method for signature 'nanoival,integer64'
    e1 - e2
    
    ## S4 method for signature 'nanoival,numeric'
    e1 - e2
    
    ## S4 method for signature 'nanoival,integer64'
    e1 + e2
    
    ## S4 method for signature 'nanoival,numeric'
    e1 + e2
    
    ## S4 method for signature 'integer64,nanoival'
    e1 + e2
    
    ## S4 method for signature 'numeric,nanoival'
    e1 + e2
    
    ## S4 method for signature 'nanoival'
    x[[i, j, ..., drop = FALSE]]
    
    ## S4 method for signature 'nanoival,logical'
    x[i, j, ..., drop = FALSE]
    
    ## S4 method for signature 'nanoival,numeric'
    x[i, j, ..., drop = FALSE]
    
    ## S4 method for signature 'nanoival,character'
    x[i, j, ..., drop = FALSE]
    
    ## S4 method for signature 'nanoival,ANY'
    x[i, j, ..., drop = FALSE]
    
    ## S4 replacement method for signature 'nanoival,logical,ANY,nanoival'
    x[i, j, ...] <- value
    
    ## S3 method for class 'nanoival'
    c(...)
    
    ## S4 method for signature 'nanoival'
    t(x)
    
    ## S4 method for signature 'nanotime,nanoival'
    x[i, j, ..., drop = TRUE]
    
    NA_nanoival_

### Arguments

| Argument  | Description                                                                                                                     |
| --------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `start`   | `nanotime` start of interval                                                                                                    |
| `end`     | `nanotime` end of interval                                                                                                      |
| `sopen`   | logical indicating if the start of the interval is open                                                                         |
| `eopen`   | logical indicating if the end of the interval is open                                                                           |
| `x, from` | a `nanoival` object                                                                                                             |
| `...`     | further arguments passed to or from methods.                                                                                    |
| `quote`   | indicates if the output of `print` should be quoted                                                                             |
| `object`  | argument for method `show`                                                                                                      |
| `format`  | A character string. Can also be set via `options("nanotimeFormat")` and uses ‘%Y-%m-%dT%H:%M:%E9S%Ez’ as a default and fallback |
| `tz`      | `character` indicating a timezone                                                                                               |
| `value`   | argument for `nanoival-class`                                                                                                   |
| `e1`      | Operand of class `nanoival`                                                                                                     |
| `e2`      | Operand of class `nanoival`                                                                                                     |
| `i`       | index specifying elements to extract or replace.                                                                                |
| `j`       | Required for `[` signature but ignored here                                                                                     |
| `drop`    | Required for `[` signature but ignored here                                                                                     |

### Format

An object of class `nanoival` of length 1.

### Details

An interval object can be constructed with the constructor `nanoival`
which takes as arguments two `nanotime` objects that define the start
and the end of the interval, together with two `logical` arguments that
define if the start and the end of the interval are open (true) or
closed (false) (note that these objects can all be vector, and therefore
the interval object is not necessarily scalar). Alternatively, an
interval can be constructed with a `character`: the format follows that
of `nanotime`; the start time is preceeded by either `-` or `+`
indicating if the interval start is open (-) or closed (+); the start
and end times are separated by an arrow `->`; the end is folloed by
either `-` or `+` which have the same semantics as the start time.

The most important set of methods defined for `interval` are set
functions `intersect`, `union` and `setdiff`.

Additionally, `interval` allows the subsetting into a `nanotime` vector.
Note that subsetting is allowed only if the `nanotime` vector is sorted.

Finally, accessors are provided to get the interval start (`start`), the
end (`end`), the open/close status of the start (`sopen`) and the
open/close status of the end (`eopen`). The former return a `nanotime`
while the latter return a `logical`.

### Value

A nanoival object

### Output Format

Formatting and character conversion for `nanoival` objects is identical
to `nanotime` objects. The default format is ISO3339 compliant:
`%Y-%m-%dT%H:%M:%E9S%Ez`. It specifies a standard ISO 8601 part for date
and time — as well as nine digits of precision for fractional seconds
(down to nanoseconds) and on offset (typically zero as we default to
UTC). It can be overriden by using `options()` with the key of
`nanotimeFormat` and a suitable value. Similarly, `nanotimeTz` can be
used to select a different timezone.

### Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

### See Also

`intersect.idx`, `setdiff.idx`,

### Examples

    ## Not run: 
    ## creating a \code{nanoival}, with the start time included ('+') and the end
    ## time excluded ('-')
    as.nanoival("+2012-03-01T21:21:00.000000001+00:00->2015-01-01T21:22:00.000000999+04:00-")
    
    ## a \code{nanoival} can also be created with a pair of \code{nanotime} objects, a start
    ## and an end, and optionally two logicals determining if the interval start(end) are open
    ## or closed; by default the start is closed and end is open:
    start <- nanotime("2012-03-01T21:21:00.000000001+00:00")
    end <- nanotime("2013-03-01T21:21:00.000000001+00:00")
    nanoival(start, end)
    
    ## a vector of 'nanotime' can be subsetted by a 'nanoival':
    one_second <- 1e9
    a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
    idx <- c(as.nanoival("-2012-12-12 12:12:10+00:00 -> 2012-12-12 12:12:14+00:00-"),
             as.nanoival("+2012-12-12 12:12:18+00:00 -> 2012-12-12 12:12:20+00:00+"))
    a[idx]
    
    ## End(Not run)
