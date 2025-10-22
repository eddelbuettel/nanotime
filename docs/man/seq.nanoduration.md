

# Sequence Generation

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

Generate a sequence of <code>nanoduration</code>

## Usage

<pre><code class='language-R'>## S3 method for class 'nanoduration'
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
the increment of the sequence
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="length.out">length.out</code>
</td>
<td>
integer indicating the desired length of the sequence
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
arguments passed to or from methods
</td>
</tr>
</table>

## Examples

``` r
library("nanotime")

seq(from=as.nanoduration(0), by=as.nanoduration("01:00:00"), length.out=10)
```

     [1] 00:00:00 01:00:00 02:00:00 03:00:00 04:00:00 05:00:00 06:00:00 07:00:00
     [9] 08:00:00 09:00:00
