

# Nanoperiod accessors

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

These functions allow access to the components of a
<code>nanoperiod</code>

## Usage

<pre><code class='language-R'>## S4 method for signature 'nanoperiod'
nanoperiod.month(x)

# S4 method for signature 'nanoperiod'
nanoperiod.day(x)

# S4 method for signature 'nanoperiod'
nanoperiod.nanoduration(x)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
A <code>nanoperiod</code>
</td>
</tr>
</table>

## Value

<code>nanoperiod.month</code> and <code>nanoperiod.day</code> return an
<code>integer64</code> whereas <code>nanoperiod.nanoduration</code>
returns a <code>nanoduration</code>

## Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

## See Also

<code>nanoduration</code>

## Examples

``` r
library("nanotime")

p <- as.nanoperiod("2y1m1d/12:00:00")
nanoperiod.month(p)
```

    [1] 25

``` r
nanoperiod.day(p)
```

    [1] 1

``` r
nanoperiod.nanoduration(p)
```

    [1] 12:00:00
