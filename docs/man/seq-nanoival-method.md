

# Sequence Generation

## Description

Generate a sequence of <code>nanoival</code>

## Usage

<pre><code class='language-R'>## S4 method for signature 'nanoival'
seq(from, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="from">from</code>, <code id="to">to</code>
</td>
<td>
the starting and (maximal) end values of the sequence
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="by">by</code>
</td>
<td>
<code>nanoduration</code> or <code>nanoperiod</code> increment of the
sequence; note that if the class is <code>nanoperiod</code> the
additional argument <code>tz</code> must be speficied and is of
<code>character</code> type indicating a timezone
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="length.out">length.out</code>
</td>
<td>
an integer desired length of the sequence
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="along.with">along.with</code>
</td>
<td>
take the length from the length of this argument.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
arguments passed to or from methods; the only interesting additional
argument is <code>tz</code> where the <code>to</code> argument is of
type <code>nanoperiod</code>
</td>
</tr>
</table>

## Examples

``` r
library("nanotime")

from <- as.nanoival("-2018-01-14T13:00:00+00:00 -> 2018-01-14T15:00:00+00:00+")
seq(from, by=as.nanoperiod("1m"), length.out=5, tz="America/New_York")
```
