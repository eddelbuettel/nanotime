

## Duration type with nanosecond precision

### Description

The type `nanoduration` is a length of time (implemented as an S4 class)
with nanosecond precision. It is a count of nanoseconds and may be
negative. The expected arithmetic operations are provided, including
sequence generation.

### Usage

``` R
nanoduration(hours = 0L, minutes = 0L, seconds = 0L, nanoseconds = 0L)

## S4 method for signature 'character'
as.nanoduration(x)

## S4 method for signature 'integer64'
as.nanoduration(x)

## S4 method for signature 'numeric'
as.nanoduration(x)

## S4 method for signature 'integer'
as.nanoduration(x)

## S4 method for signature 'difftime'
as.nanoduration(x)

## S4 method for signature ''NULL''
as.nanoduration(x)

## S4 method for signature 'missing'
as.nanoduration(x)

## S4 method for signature 'nanoduration'
show(object)

## S4 method for signature 'nanoduration'
print(x, quote = FALSE, ...)

## S3 method for class 'nanoduration'
format(x, ...)

## S3 method for class 'nanoduration'
as.integer64(x, ...)

## S4 method for signature 'nanoduration'
as.character(x)

## S4 method for signature 'nanoduration'
is.na(x)

## S4 method for signature 'nanoduration,nanoduration'
e1 - e2

## S4 method for signature 'nanoduration,integer64'
e1 - e2

## S4 method for signature 'nanoduration,integer'
e1 - e2

## S4 method for signature 'nanoduration,numeric'
e1 - e2

## S4 method for signature 'nanoduration,difftime'
e1 - e2

## S4 method for signature 'nanoduration,ANY'
e1 - e2

## S4 method for signature 'nanotime,nanoduration'
e1 - e2

## S4 method for signature 'nanotime,difftime'
e1 - e2

## S4 method for signature 'integer64,nanoduration'
e1 - e2

## S4 method for signature 'integer,nanoduration'
e1 - e2

## S4 method for signature 'numeric,nanoduration'
e1 - e2

## S4 method for signature 'difftime,nanoduration'
e1 - e2

## S4 method for signature 'ANY,nanoduration'
e1 - e2

## S4 method for signature 'nanoduration,ANY'
e1 + e2

## S4 method for signature 'nanoduration,nanoduration'
e1 + e2

## S4 method for signature 'nanoduration,integer64'
e1 + e2

## S4 method for signature 'nanoduration,numeric'
e1 + e2

## S4 method for signature 'nanoduration,difftime'
e1 + e2

## S4 method for signature 'nanotime,nanoduration'
e1 + e2

## S4 method for signature 'nanotime,difftime'
e1 + e2

## S4 method for signature 'nanoduration,nanotime'
e1 + e2

## S4 method for signature 'difftime,nanotime'
e1 + e2

## S4 method for signature 'nanoival,nanoduration'
e1 + e2

## S4 method for signature 'nanoival,nanoduration'
e1 - e2

## S4 method for signature 'nanoduration,nanoival'
e1 + e2

## S4 method for signature 'nanoival,difftime'
e1 + e2

## S4 method for signature 'nanoival,difftime'
e1 - e2

## S4 method for signature 'difftime,nanoival'
e1 + e2

## S4 method for signature 'integer64,nanoduration'
e1 + e2

## S4 method for signature 'numeric,nanoduration'
e1 + e2

## S4 method for signature 'difftime,nanoduration'
e1 + e2

## S4 method for signature 'nanoduration,numeric'
e1 * e2

## S4 method for signature 'nanoduration,integer64'
e1 * e2

## S4 method for signature 'numeric,nanoduration'
e1 * e2

## S4 method for signature 'integer64,nanoduration'
e1 * e2

## S4 method for signature 'nanoduration,nanoduration'
e1 / e2

## S4 method for signature 'nanoduration,integer64'
e1 / e2

## S4 method for signature 'nanoduration,numeric'
e1 / e2

## S4 method for signature 'nanoduration,ANY'
Arith(e1, e2)

## S4 method for signature 'nanoduration,character'
Compare(e1, e2)

## S4 method for signature 'character,nanoduration'
Compare(e1, e2)

## S4 method for signature 'nanoduration,ANY'
Compare(e1, e2)

## S4 method for signature 'nanoduration'
abs(x)

## S4 method for signature 'nanoduration'
sign(x)

## S4 method for signature 'nanoduration'
sum(x, ..., na.rm = FALSE)

## S4 method for signature 'nanoduration'
min(x, ..., na.rm = FALSE)

## S4 method for signature 'nanoduration'
max(x, ..., na.rm = FALSE)

## S4 method for signature 'nanoduration'
range(x, ..., na.rm = FALSE)

## S4 method for signature 'nanoduration'
x[[i, j, ..., drop = FALSE]]

## S4 method for signature 'nanoduration,numeric'
x[i, j, ..., drop = FALSE]

## S4 method for signature 'nanoduration,logical'
x[i, j, ..., drop = FALSE]

## S4 method for signature 'nanoduration,character'
x[i, j, ..., drop = FALSE]

## S4 method for signature 'nanoduration,ANY'
x[i, j, ..., drop = FALSE]

## S4 replacement method for signature 'nanoduration,ANY,ANY,ANY'
x[i, j, ...] <- value

## S3 method for class 'nanoduration'
c(...)

NA_nanoduration_
```

### Arguments

|               |                                                     |
|---------------|-----------------------------------------------------|
| `hours`       | number of hours                                     |
| `minutes`     | number of minutes                                   |
| `seconds`     | number of seconds                                   |
| `nanoseconds` | number of nanoseconds                               |
| `x`           | a `nanoduration` object                             |
| `object`      | argument for method `show`                          |
| `quote`       | indicates if the output of `print` should be quoted |
| `...`         | further arguments passed to or from methods.        |
| `e1`          | Operand of class `nanoival`                         |
| `e2`          | Operand of class `nanoival`                         |
| `na.rm`       | if `TRUE` NA values are removed for the computation |
| `i`           | index specifying elements to extract or replace.    |
| `j`           | Required for `[` signature but ignored here         |
| `drop`        | Required for `[` signature but ignored here         |
| `value`       | argument for `nanoduration-class`                   |

### Format

An object of class `nanoduration` of length 1.

### Details

A `nanoduration` can be constructed with the function `as.nanoduration`
which can take the types `integer64`, `integer` and `numeric` (all
indicating the count in nanosecond units) or the type `character`.

It can also be constructed by specifying with individual arguments the
hours, minutes, seconds and nanoseconds with a call to `nanoduration`.

A `nanoduration` is displayed as hours, minutes, seconds and nanoseconds
like this: `110:12:34.123_453_001`. The nanosecond precision displayed
is adjusted as necessary, so e.g. 1 second is displayed as `00:00:01`.

### Value

A nanoduration object

### Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

### See Also

`nanotime`

### Examples

``` R
## constructors:
nanoduration(hours=10, minutes=3, seconds=2, nanoseconds=999999999)
as.nanoduration("10:03:02.999_999_999")
as.nanoduration(36182999999999)

## arithmetic:
as.nanoduration(10e9) - as.nanoduration(9e9)
as.nanoduration(10e9) + as.nanoduration(-9e9)
as.nanoduration("24:00:00") / 2
as.nanoduration("24:00:00") / as.nanoduration("12:00:00")

## comparison:
as.nanoduration("10:03:02.999_999_999") == 36182999999999
as.nanoduration("10:03:02.999_999_999") > as.nanoduration("10:03:02.999_999_998")
as.nanoduration("10:03:02.999_999_998") < "10:03:02.999_999_999"
```


