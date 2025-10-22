

# Period type with nanosecond precision

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

<code>nanoperiod</code> is a length of time type (implemented as an S4
class) with nanosecond precision. It differs from
<code>nanoduration</code> because it is capable of representing calendar
months and days. It can thus represent years (12 months) and weeks (7
days). A period is a somewhat abstract representation of time: it is
only when anchored to a point in time and in a specific time zone that
it is possible to convert it to a specific duration. This means that
many of the operations involving periods need the additional argument
<code>tz</code>.

## Usage

<pre><code class='language-R'>nanoperiod(months = 0, days = 0, duration = as.nanoduration(0))

# S4 method for signature 'character'
as.nanoperiod(x)

# S4 method for signature 'integer64'
as.nanoperiod(x)

# S4 method for signature 'numeric'
as.nanoperiod(x)

# S4 method for signature 'integer'
as.nanoperiod(x)

# S4 method for signature 'nanoduration'
as.nanoperiod(x)

# S4 method for signature ''NULL''
as.nanoperiod(x)

# S4 method for signature 'missing'
as.nanoperiod(x)

# S4 method for signature 'nanoperiod'
show(object)

# S4 method for signature 'nanoperiod'
print(x, quote = FALSE, ...)

# S3 method for class 'nanoperiod'
format(x, ...)

# S4 method for signature 'nanoperiod'
as.character(x)

# S4 method for signature 'nanoperiod'
is.na(x)

# S4 replacement method for signature 'nanoperiod'
is.na(x) &lt;- value

# S4 method for signature 'nanoperiod'
x[[i, j, ..., drop = FALSE]]

# S4 method for signature 'nanoperiod,numeric'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoperiod,logical'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoperiod,character'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoperiod,ANY'
x[i, j, ..., drop = FALSE]

# S4 replacement method for signature 'nanoperiod,ANY,ANY,ANY'
x[i, j, ...] &lt;- value

# S3 method for class 'nanoperiod'
c(...)

# S4 method for signature 'nanoperiod'
names(x)

# S4 replacement method for signature 'nanoperiod'
names(x) &lt;- value

# S4 method for signature 'nanoperiod,ANY'
e1 - e2

# S4 method for signature 'nanoperiod,nanoperiod'
e1 - e2

# S4 method for signature 'nanoperiod,nanoduration'
e1 - e2

# S4 method for signature 'nanoperiod,integer64'
e1 - e2

# S4 method for signature 'nanoperiod,numeric'
e1 - e2

# S4 method for signature 'nanoduration,nanoperiod'
e1 - e2

# S4 method for signature 'integer64,nanoperiod'
e1 - e2

# S4 method for signature 'numeric,nanoperiod'
e1 - e2

# S4 method for signature 'nanoperiod,ANY'
e1 + e2

# S4 method for signature 'nanoperiod,nanoperiod'
e1 + e2

# S4 method for signature 'nanoperiod,nanoduration'
e1 + e2

# S4 method for signature 'nanoperiod,integer64'
e1 + e2

# S4 method for signature 'nanoperiod,nanotime'
e1 + e2

# S4 method for signature 'nanoival,nanoperiod'
e1 + e2

# S4 method for signature 'nanoperiod,nanoival'
e1 + e2

# S4 method for signature 'nanotime,nanoperiod'
e1 + e2

# S4 method for signature 'nanoperiod,numeric'
e1 + e2

# S4 method for signature 'nanoduration,nanoperiod'
e1 + e2

# S4 method for signature 'integer64,nanoperiod'
e1 + e2

# S4 method for signature 'numeric,nanoperiod'
e1 + e2

# S4 method for signature 'nanoperiod,integer64'
e1 * e2

# S4 method for signature 'nanoperiod,numeric'
e1 * e2

# S4 method for signature 'integer64,nanoperiod'
e1 * e2

# S4 method for signature 'numeric,nanoperiod'
e1 * e2

# S4 method for signature 'nanoperiod,integer64'
e1 / e2

# S4 method for signature 'nanoperiod,numeric'
e1 / e2

# S4 method for signature 'nanoperiod,nanoperiod'
e1 == e2

# S4 method for signature 'nanoperiod,nanoperiod'
e1 != e2

# S4 method for signature 'nanotime,nanoperiod,character'
plus(e1, e2, tz)

# S4 method for signature 'nanoperiod,nanotime,character'
plus(e1, e2, tz)

# S4 method for signature 'nanotime,nanoperiod,character'
minus(e1, e2, tz)

# S4 method for signature 'nanoperiod,nanotime,character'
minus(e1, e2, tz)

# S4 method for signature 'nanoival,nanoperiod,character'
plus(e1, e2, tz)

# S4 method for signature 'nanoperiod,nanoival,character'
plus(e1, e2, tz)

# S4 method for signature 'nanoival,nanoperiod,character'
minus(e1, e2, tz)

NA_nanoperiod_
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="months">months</code>
</td>
<td>
Used in the constructor to indicate the number of months of the
<code>nanoperiod</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="days">days</code>
</td>
<td>
Used in the constructor to indicate the number of days of the
<code>nanoperiod</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="duration">duration</code>
</td>
<td>
Used in the constructor to indicate the duration component of the
<code>nanoperiod</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>, <code id="value">value</code>
</td>
<td>
An object of class <code>nanoperiod</code>
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
further arguments
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
<code id="e1">e1</code>
</td>
<td>
Operand of class <code>nanoperiod</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="e2">e2</code>
</td>
<td>
Operand of class <code>nanoperiod</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tz">tz</code>
</td>
<td>
<code>character</code> indicating a timezone
</td>
</tr>
</table>

## Format

An object of class <code>nanoperiod</code> of length 1.

## Constructors

The true constructor is

## Output Format

A <code>nanoperiod</code> is displayed as months, days, and
<code>nanoduration</code> like this:
<code>10m2d/10:12:34.123_453_000</code>.

## Details

Adding or subtracting <code>nanoperiod</code> and <code>nanotime</code>
require a timezone as third argument. For this reason it is not possible
to use the binary operator ‘<code>+</code>’. Instead the functions
‘<code>plus</code>’ and ‘<code>minus</code>’ are defined. These
functions attempt to keep the same offset within a day in the specified
timezone: this means for instance that adding a day when that day
crosses a time zone adjustment such as a daylight saving time, results
in a true time increment of less or more than 24 hours to preserve the
offset. Preserving the offset works for increments that are smaller than
a day too, provided the increment results in a datetime where the
timezone adjustment is valid. When this is not the case, adding a
‘nanoperiod’ behaves in the same way as adding a ‘nanoduration’.

## Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

## See Also

<code>nanotime</code>, <code>nanoduration</code>, <code>nanoival</code>,
<code>nanoperiod.month,nanoperiod-method</code>

## Examples

``` r
library("nanotime")

p <- nanoperiod(months=12, days=7, duration="01:00:00")
print(p)

#  when adding a \code{nanoperiod} to a \code{nanotime} or to a
# \code{nanoival}, a time zone must be specified:
y <- nanotime("1970-01-01T00:00:00+00:00")
plus(y, p, tz="America/Chicago")
```
