

# Nanosecond resolution datetime functionality

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

Functions to operate on nanosecond time resolution using integer64 bit
representation. Conversion functions for several standard R types are
provided, and more will be added as needed.

## Usage

<pre><code class='language-R'>nanotime(from, ...)

as.nanotime(from, ...)

# S4 method for signature 'character'
nanotime(from, format = "", tz = "")

# S4 method for signature 'character'
as.nanotime(from, format = "", tz = "")

nanotime.matrix(x)

# S4 method for signature 'POSIXct'
nanotime(from, accurate = TRUE)

# S4 method for signature 'POSIXct'
as.nanotime(from, accurate = TRUE)

# S4 method for signature 'POSIXlt'
nanotime(from)

# S4 method for signature 'POSIXlt'
as.nanotime(from)

# S4 method for signature 'Date'
nanotime(from)

# S4 method for signature 'Date'
as.nanotime(from)

# S4 method for signature 'nanotime'
print(x, format = "", tz = "", quote = FALSE, ...)

# S4 method for signature 'nanotime'
show(object)

# S3 method for class 'nanotime'
format(x, format = "", tz = "", ...)

# S3 method for class 'nanotime'
index2char(x, ...)

# S3 method for class 'nanotime'
as.POSIXct(x, tz = "", ...)

# S3 method for class 'nanotime'
as.POSIXlt(x, tz = "", ...)

# S3 method for class 'nanotime'
as.Date(x, ...)

# S3 method for class 'nanotime'
as.data.frame(x, ...)

# S3 method for class 'nanotime'
as.integer64(x, ...)

# S4 method for signature 'nanotime,character'
e1 - e2

# S4 method for signature 'nanotime,nanotime'
e1 - e2

# S4 method for signature 'nanotime,integer64'
e1 - e2

# S4 method for signature 'nanotime,numeric'
e1 - e2

# S4 method for signature 'ANY,nanotime'
e1 - e2

# S4 method for signature 'nanotime,ANY'
e1 - e2

# S4 method for signature 'nanotime,ANY'
e1 + e2

# S4 method for signature 'nanotime,integer64'
e1 + e2

# S4 method for signature 'nanotime,numeric'
e1 + e2

# S4 method for signature 'ANY,nanotime'
e1 + e2

# S4 method for signature 'integer64,nanotime'
e1 + e2

# S4 method for signature 'numeric,nanotime'
e1 + e2

# S4 method for signature 'nanotime,nanotime'
e1 + e2

# S4 method for signature 'nanotime,nanotime'
Arith(e1, e2)

# S4 method for signature 'nanotime,ANY'
Arith(e1, e2)

# S4 method for signature 'ANY,nanotime'
Arith(e1, e2)

# S4 method for signature 'nanotime,character'
Compare(e1, e2)

# S4 method for signature 'character,nanotime'
Compare(e1, e2)

# S4 method for signature 'nanotime,POSIXt'
Compare(e1, e2)

# S4 method for signature 'POSIXt,nanotime'
Compare(e1, e2)

# S4 method for signature 'nanotime,ANY'
Compare(e1, e2)

# S4 method for signature 'nanotime,ANY'
Logic(e1, e2)

# S4 method for signature 'ANY,nanotime'
Logic(e1, e2)

# S4 method for signature 'nanotime'
Math(x)

# S4 method for signature 'nanotime'
Math2(x, digits)

# S4 method for signature 'nanotime'
Summary(x, ..., na.rm = FALSE)

# S4 method for signature 'nanotime'
min(x, ..., na.rm = FALSE)

# S4 method for signature 'nanotime'
max(x, ..., na.rm = FALSE)

# S4 method for signature 'nanotime'
range(x, ..., na.rm = FALSE)

# S4 method for signature 'nanotime'
Complex(z)

# S4 method for signature 'nanotime'
x[[i, j, ..., drop = FALSE]]

# S4 method for signature 'nanotime,numeric'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanotime,logical'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanotime,character'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanotime,ANY'
x[i, j, ..., drop = FALSE]

# S4 replacement method for signature 'nanotime,ANY,ANY,ANY'
x[i, j, ...] &lt;- value

# S3 method for class 'nanotime'
c(...)

# S4 replacement method for signature 'nanotime'
names(x) &lt;- value

# S4 method for signature 'nanotime'
is.na(x)

NA_nanotime_

# S3 method for class 'nanotime'
as.character(x, ...)

# S3 method for class 'nanoduration'
as.data.frame(x, ...)
</code></pre>

## Arguments

<table role="presentation">
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
<code id="format">format</code>
</td>
<td>
A character string. Can also be set via
<code>options(“nanotimeFormat”)</code> and uses ‘%Y-%m-%dT%H:%M:%E9S%Ez’
as a default and fallback
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tz">tz</code>
</td>
<td>
character specifying a timezone which is required for
<code>as.POSIXct</code>, <code>as.POSIXlt</code> and can be specified
for <code>as.nanotime</code>, <code>format</code> and
<code>print</code>; it can also be set via
<code>options(“nanotimeTz”)</code> and uses ‘UTC’ as a default and
fallback
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>, <code id="from">from</code>
</td>
<td>
<code>nanotime</code> objects
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="accurate">accurate</code>
</td>
<td>
in the conversion from <code>POSIXct</code> to <code>nanotime</code>,
indicates if one wants to preserve the maximum precision possible; the
default is <code>TRUE</code>, but in most situations the loss of
precision is negligible, and setting this parameter to <code>TRUE</code>
will make the conversion nearly an order of magnitude faster
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
<code id="object">object</code>
</td>
<td>
argument for method <code>show</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="e1">e1</code>
</td>
<td>
Operand of class <code>nanotime</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="e2">e2</code>
</td>
<td>
Operand of class <code>nanotime</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="digits">digits</code>
</td>
<td>
Required for <code>Math2</code> signature but ignored here
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="na.rm">na.rm</code>
</td>
<td>
a logical indicating whether missing values should be removed.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="z">z</code>
</td>
<td>
Required for <code>Complex</code> signature but ignored here
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
argument for <code>nanotime-class</code>
</td>
</tr>
</table>

## Format

An object of class <code>nanotime</code> of length 1.

## Details

Notice that the conversion from POSIXct explicitly sets the last three
digits to zero. Nanosecond time stored in a 64-bit integer has nineteen
digits precision where doubles (which are used internally for POSIXct as
well) only have sixteen digits. So rather than showing three more
(essentially <em>random</em>) digits it is constructed such that these
three additional digits are zeros.

## Value

A nanotime object

## Caveats

Working with dates and times is <em>difficult</em>. One needs a
representation of both <em>time points</em> and <em>time duration</em>.
In R, think of <code>Date</code> or <code>POSIXct</code> objects for the
former, and <code>difftime</code> for the later. Here we have time
points <code>nanotime</code>, an interval type <code>nanoival</code> and
two flavors of duration which are a simple count of nanoseconds
<code>nanoduration</code> and a calendar duration that is able to track
concepts such as months and days <code>nanoperiod</code>. Point in time
and intervals are all based on durations relative to the epoch of
January 1, 1970.

## Input and Output Format

Formatting and character conversion for <code>nanotime</code> objects is
done by functions from the <span class="pkg">RcppCCTZ</span> package
relying on code from its embedded <code>CCTZ</code> library. The default
format is ISO3339 compliant: <code>%Y-%m-%dT%H:%M:%E9S%Ez</code>. It
specifies a standard ISO 8601 part for date and time — as well as nine
digits of precision for fractional seconds (down to nanoseconds) and on
offset (typically zero as we default to UTC). It can be overriden by
using <code>options()</code> with the key of <code>nanotimeFormat</code>
and a suitable value. Similarly, <code>nanotimeTz</code> can be used to
select a different timezone.

For input, some slack it cut, and various shortened formats are accepted
by default such as <code>2020-03-10</code> or <code>2020-03-10
18:16:00</code>, or <code>2020-03-10 18:16:00.001</code> (and the ‘T’
separator is optional.

## <code>tz</code> parameter usage in constructors

The <code>tz</code> parameter is allowed only when constructing a
<code>nanotime</code> from a <code>character</code>. This is because any
<code>numeric</code>, <code>Date</code> and <code>POSIXct</code> is de
facto considered an offset since the epoch. On the contrary, a
<code>character</code> is considered interpretable and hence if it does
not contain a timezone in its representation, it is possible to specify
the <code>tz</code> argument to specify in which timezone it should be
interpreted. This is useful in particular if one wants to convert a
<code>Date</code> to be aligned to the beginning of the day in a
specific timezone; in this case one should convert the <code>Date</code>
to a <code>character</code> before calling the <code>nanotime</code>
constructor with the desired timezone.

## Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

## See Also

<code>nanoival</code>, <code>nanoduration</code>,
<code>nanoperiod</code>, <code>seq.nanotime</code> as well as the
documentation in package <span class="pkg">RcppCCTZ</span>.

## Examples

``` r
library("nanotime")

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
```
