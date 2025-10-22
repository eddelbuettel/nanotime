

# Interval type with nanosecond precision

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

<code>nanoival</code> is a time interval type (an S4 class) with
nanosecond precision. One of its purposes is to allow quick subsetting
of a <code>nanotime</code> vector. <code>nanoival</code> is composed of
a <code>nanotime</code> pair which defines the start and end of the time
interval. Additionally, it has a pair of logical values which determine
if the start and end of the time interval are open (true) or closed
(false).

## Usage

<pre><code class='language-R'>nanoival(start, end, sopen = FALSE, eopen = TRUE)

# S4 method for signature 'nanoival'
nanoival.start(x)

# S4 method for signature 'nanoival'
nanoival.end(x)

# S4 method for signature 'nanoival'
nanoival.sopen(x)

# S4 method for signature 'nanoival'
nanoival.eopen(x)

# S3 method for class 'nanoival'
format(x, ...)

# S4 method for signature 'nanoival'
print(x, quote = FALSE, ...)

# S4 method for signature 'nanoival'
show(object)

# S4 method for signature 'character'
as.nanoival(from, format = "", tz = "")

# S4 method for signature ''NULL''
as.nanoival(from, format = "", tz = "")

# S4 method for signature 'missing'
as.nanoival(from, format = "", tz = "")

# S4 method for signature 'nanoival'
is.na(x)

# S4 replacement method for signature 'nanoival'
is.na(x) &lt;- value

# S4 method for signature 'nanoival,nanoival'
e1 &lt; e2

# S4 method for signature 'nanoival,nanoival'
e1 &lt;= e2

# S4 method for signature 'nanoival,nanoival'
e1 &gt; e2

# S4 method for signature 'nanoival,nanoival'
e1 &gt;= e2

# S4 method for signature 'nanoival,nanoival'
e1 == e2

# S4 method for signature 'nanoival,nanoival'
e1 != e2

# S4 method for signature 'nanoival,integer64'
e1 - e2

# S4 method for signature 'nanoival,numeric'
e1 - e2

# S4 method for signature 'nanoival,integer64'
e1 + e2

# S4 method for signature 'nanoival,numeric'
e1 + e2

# S4 method for signature 'integer64,nanoival'
e1 + e2

# S4 method for signature 'numeric,nanoival'
e1 + e2

# S4 method for signature 'nanoival'
x[[i, j, ..., drop = FALSE]]

# S4 method for signature 'nanoival,logical'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoival,numeric'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoival,character'
x[i, j, ..., drop = FALSE]

# S4 method for signature 'nanoival,ANY'
x[i, j, ..., drop = FALSE]

# S4 replacement method for signature 'nanoival,logical,ANY,nanoival'
x[i, j, ...] &lt;- value

# S3 method for class 'nanoival'
c(...)

# S4 method for signature 'nanoival'
t(x)

# S4 method for signature 'nanotime,nanoival'
x[i, j, ..., drop = TRUE]

NA_nanoival_

# S3 method for class 'nanoival'
as.character(x, ...)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="start">start</code>
</td>
<td>
<code>nanotime</code> start of interval
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="end">end</code>
</td>
<td>
<code>nanotime</code> end of interval
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sopen">sopen</code>
</td>
<td>
logical indicating if the start of the interval is open
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="eopen">eopen</code>
</td>
<td>
logical indicating if the end of the interval is open
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>, <code id="from">from</code>
</td>
<td>
a <code>nanoival</code> object
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
<code>character</code> indicating a timezone
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="value">value</code>
</td>
<td>
argument for <code>nanoival-class</code>
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
</table>

## Format

An object of class <code>nanoival</code> of length 1.

## Details

An interval object can be constructed with the constructor
<code>nanoival</code> which takes as arguments two <code>nanotime</code>
objects that define the start and the end of the interval, together with
two <code>logical</code> arguments that define if the start and the end
of the interval are open (true) or closed (false) (note that these
objects can all be vector, and therefore the interval object is not
necessarily scalar). Alternatively, an interval can be constructed with
a <code>character</code>: the format follows that of
<code>nanotime</code>; the start time is preceeded by either
<code>-</code> or <code>+</code> indicating if the interval start is
open (-) or closed (+); the start and end times are separated by an
arrow <code>-\></code>; the end is folloed by either <code>-</code> or
<code>+</code> which have the same semantics as the start time.

The most important set of methods defined for <code>interval</code> are
set functions <code>intersect</code>, <code>union</code> and
<code>setdiff</code>.

Additionally, <code>interval</code> allows the subsetting into a
<code>nanotime</code> vector. Note that subsetting is allowed only if
the <code>nanotime</code> vector is sorted.

Finally, accessors are provided to get the interval start
(<code>start</code>), the end (<code>end</code>), the open/close status
of the start (<code>sopen</code>) and the open/close status of the end
(<code>eopen</code>). The former return a <code>nanotime</code> while
the latter return a <code>logical</code>.

## Value

A nanoival object

## Output Format

Formatting and character conversion for <code>nanoival</code> objects is
identical to <code>nanotime</code> objects. The default format is
ISO3339 compliant: <code>%Y-%m-%dT%H:%M:%E9S%Ez</code>. It specifies a
standard ISO 8601 part for date and time — as well as nine digits of
precision for fractional seconds (down to nanoseconds) and on offset
(typically zero as we default to UTC). It can be overriden by using
<code>options()</code> with the key of <code>nanotimeFormat</code> and a
suitable value. Similarly, <code>nanotimeTz</code> can be used to select
a different timezone.

## Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

## See Also

<code>intersect.idx</code>, <code>setdiff.idx</code>,

## Examples

``` r
library("nanotime")

# creating a \code{nanoival}, with the start time included ('+') and the end
# time excluded ('-')
as.nanoival("+2012-03-01T21:21:00.000000001+00:00->2015-01-01T21:22:00.000000999+04:00-")

# a \code{nanoival} can also be created with a pair of \code{nanotime} objects, a start
# and an end, and optionally two logicals determining if the interval start(end) are open
# or closed; by default the start is closed and end is open:
start <- nanotime("2012-03-01T21:21:00.000000001+00:00")
end <- nanotime("2013-03-01T21:21:00.000000001+00:00")
nanoival(start, end)

# a vector of 'nanotime' can be subsetted by a 'nanoival':
one_second <- 1e9
a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
idx <- c(as.nanoival("-2012-12-12 12:12:10+00:00 -> 2012-12-12 12:12:14+00:00-"),
         as.nanoival("+2012-12-12 12:12:18+00:00 -> 2012-12-12 12:12:20+00:00+"))
a[idx]
```
