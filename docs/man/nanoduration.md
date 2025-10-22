

# Duration type with nanosecond precision

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

The type <code>nanoduration</code> is a length of time (implemented as
an S4 class) with nanosecond precision. It is a count of nanoseconds and
may be negative. The expected arithmetic operations are provided,
including sequence generation.

## Usage

<pre><code class='language-R'>nanoduration(hours = 0L, minutes = 0L, seconds = 0L, nanoseconds = 0L)

# S4 method for signature 'character'
as.nanoduration(x)

# S4 method for signature 'integer64'
as.nanoduration(x)

# S4 method for signature 'numeric'
as.nanoduration(x)

# S4 method for signature 'integer'
as.nanoduration(x)

# S4 method for signature 'difftime'
as.nanoduration(x)

# S4 method for signature ''NULL''
as.nanoduration(x)

# S4 method for signature 'missing'
as.nanoduration(x)

# S4 method for signature 'nanoduration'
show(object)

# S4 method for signature 'nanoduration'
print(x, quote = FALSE, ...)

# S3 method for class 'nanoduration'
format(x, ...)

# S3 method for class 'nanoduration'
as.integer64(x, ...)

# S4 method for signature 'nanoduration'
as.character(x)

# S4 method for signature 'nanoduration'
is.na(x)

# S4 method for signature 'nanoduration,nanoduration'
e1 - e2

# S4 method for signature 'nanoduration,integer64'
e1 - e2

# S4 method for signature 'nanoduration,integer'
e1 - e2

# S4 method for signature 'nanoduration,numeric'
e1 - e2

# S4 method for signature 'nanoduration,difftime'
e1 - e2

# S4 method for signature 'nanoduration,ANY'
e1 - e2

# S4 method for signature 'nanotime,nanoduration'
e1 - e2

# S4 method for signature 'nanotime,difftime'
e1 - e2

# S4 method for signature 'integer64,nanoduration'
e1 - e2

# S4 method for signature 'integer,nanoduration'
e1 - e2

# S4 method for signature 'numeric,nanoduration'
e1 - e2

# S4 method for signature 'difftime,nanoduration'
e1 - e2

# S4 method for signature 'ANY,nanoduration'
e1 - e2

# S4 method for signature 'nanoduration,ANY'
e1 + e2

# S4 method for signature 'nanoduration,nanoduration'
e1 + e2

# S4 method for signature 'nanoduration,integer64'
e1 + e2

# S4 method for signature 'nanoduration,numeric'
e1 + e2

# S4 method for signature 'nanoduration,difftime'
e1 + e2

# S4 method for signature 'nanotime,nanoduration'
e1 + e2

# S4 method for signature 'nanotime,difftime'
e1 + e2

# S4 method for signature 'nanoduration,nanotime'
e1 + e2

# S4 method for signature 'difftime,nanotime'
e1 + e2

# S4 method for signature 'nanoival,nanoduration'
e1 + e2

# S4 method for signature 'nanoival,nanoduration'
e1 - e2

# S4 method for signature 'nanoduration,nanoival'
e1 + e2

# S4 method for signature 'nanoival,difftime'
e1 + e2

# S4 method for signature 'nanoival,difftime'
e1 - e2

# S4 method for signature 'difftime,nanoival'
e1 + e2

# S4 method for signature 'integer64,nanoduration'
e1 + e2

# S4 method for signature 'numeric,nanoduration'
e1 + e2

# S4 method for signature 'difftime,nanoduration'
e1 + e2

# S4 method for signature 'nanoduration,numeric'
e1 * e2

# S4 method for signature 'nanoduration,integer64'
e1 * e2

# S4 method for signature 'numeric,nanoduration'
e1 * e2

# S4 method for signature 'integer64,nanoduration'
e1 * e2

# S4 method for signature 'nanoduration,nanoduration'
e1 / e2

# S4 method for signature 'nanoduration,integer64'
e1 / e2

# S4 method for signature 'nanoduration,numeric'
e1 / e2

# S4 method for signature 'nanoduration,ANY'
Arith(e1, e2)

# S4 method for signature 'nanoduration,character'
Compare(e1, e2)

# S4 method for signature 'character,nanoduration'
Compare(e1, e2)

# S4 method for signature 'nanoduration,ANY'
Compare(e1, e2)

# S4 method for signature 'nanoduration'
abs(x)

# S4 method for signature 'nanoduration'
sign(x)

# S4 method for signature 'nanoduration'
sum(x, ..., na.rm = FALSE)

# S4 method for signature 'nanoduration'
min(x, ..., na.rm = FALSE)

# S4 method for signature 'nanoduration'
max(x, ..., na.rm = FALSE)

# S4 method for signature 'nanoduration'
range(x, ..., na.rm = FALSE)

# S4 method for signature 'nanoduration'
x[[i, j, ..., drop = FALSE]]

# S4 method for signature 'nanoduration,numeric'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoduration,logical'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoduration,character'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoduration,ANY'
x[i, j, ..., drop = FALSE]

# S4 replacement method for signature 'nanoduration,ANY,ANY,ANY'
x[i, j, ...] &lt;- value

# S3 method for class 'nanoduration'
c(...)

NA_nanoduration_
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="hours">hours</code>
</td>
<td>
number of hours
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="minutes">minutes</code>
</td>
<td>
number of minutes
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="seconds">seconds</code>
</td>
<td>
number of seconds
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="nanoseconds">nanoseconds</code>
</td>
<td>
number of nanoseconds
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
a <code>nanoduration</code> object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="object">object</code>
</td>
<td>
argument for method <code>show</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="quote">quote</code>
</td>
<td>
indicates if the output of <code>print</code> should be quoted
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
further arguments passed to or from methods.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="e1">e1</code>
</td>
<td>
Operand of class <code>nanoival</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="e2">e2</code>
</td>
<td>
Operand of class <code>nanoival</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="na.rm">na.rm</code>
</td>
<td>
if <code>TRUE</code> NA values are removed for the computation
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="i">i</code>
</td>
<td>
index specifying elements to extract or replace.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="j">j</code>
</td>
<td>
Required for <code>\[</code> signature but ignored here
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="drop">drop</code>
</td>
<td>
Required for <code>\[</code> signature but ignored here
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="value">value</code>
</td>
<td>
argument for <code>nanoduration-class</code>
</td>
</tr>
</table>

## Format

An object of class <code>nanoduration</code> of length 1.

## Details

A <code>nanoduration</code> can be constructed with the function
<code>as.nanoduration</code> which can take the types
<code>integer64</code>, <code>integer</code> and <code>numeric</code>
(all indicating the count in nanosecond units) or the type
<code>character</code>.

It can also be constructed by specifying with individual arguments the
hours, minutes, seconds and nanoseconds with a call to
<code>nanoduration</code>.

A <code>nanoduration</code> is displayed as hours, minutes, seconds and
nanoseconds like this: <code>110:12:34.123_453_001</code>. The
nanosecond precision displayed is adjusted as necessary, so e.g. 1
second is displayed as <code>00:00:01</code>.

## Value

A nanoduration object

## Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

## See Also

<code>nanotime</code>

## Examples

``` r
library("nanotime")

# constructors:
nanoduration(hours=10, minutes=3, seconds=2, nanoseconds=999999999)
```

    [1] 10:03:02.999_999_999

``` r
as.nanoduration("10:03:02.999_999_999")
```

    [1] 10:03:02.999_999_999

``` r
as.nanoduration(36182999999999)
```

    [1] 10:03:02.999_999_999

``` r
# arithmetic:
as.nanoduration(10e9) - as.nanoduration(9e9)
```

    [1] 00:00:01

``` r
as.nanoduration(10e9) + as.nanoduration(-9e9)
```

    [1] 00:00:01

``` r
as.nanoduration("24:00:00") / 2
```

    [1] 12:00:00

``` r
as.nanoduration("24:00:00") / as.nanoduration("12:00:00")
```

    [1] 2

``` r
# comparison:
as.nanoduration("10:03:02.999_999_999") == 36182999999999
```

    [1] TRUE

``` r
as.nanoduration("10:03:02.999_999_999") > as.nanoduration("10:03:02.999_999_998")
```

    [1] TRUE
    attr(,".S3Class")
    [1] "integer64"

``` r
as.nanoduration("10:03:02.999_999_998") < "10:03:02.999_999_999"
```

    [1] TRUE
    attr(,".S3Class")
    [1] "integer64"
