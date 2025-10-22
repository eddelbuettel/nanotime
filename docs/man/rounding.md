

# Rounding down or up a <code>nanotime</code> type

## Description

The functions <code>nano_floor</code> and <code>nano_ceiling</code>
round down or up, respectively. Although the underlying implementation
of <code>nanotime</code> has negative numbers for values before
1970-01-01 UTC, the rounding is always done backward in time for
<code>nano_floor</code> and forward in time for
<code>nano_ceiling</code>. The functions take a <code>nanotime</code>
argument <code>x</code> which is the instance to round, together with a
second argument <code>precision</code> which indicates an arbitrary
precision to which the rounding should be performed. This argument can
be either a <code>nanoduration</code> or or a <code>nanoperiod</code>.
In the latter case, the argument <code>tz</code> must also be specified
in order to give the <code>nanoperiod</code> a meaning. Finally, the
<code>nanotime</code> argument <code>origin</code> can be optionally
specified to fix the rounding to a specific point in time.

## Usage

<pre><code class='language-R'>nano_ceiling(x, precision, ...)

nano_floor(x, precision, ...)

# S4 method for signature 'nanotime,nanoduration'
nano_ceiling(x, precision, origin = nanotime())

# S4 method for signature 'nanotime,nanoduration'
nano_floor(x, precision, origin = nanotime())

# S4 method for signature 'nanotime,nanoperiod'
nano_ceiling(x, precision, origin = nanotime(), tz)

# S4 method for signature 'nanotime,nanoperiod'
nano_floor(x, precision, origin = nanotime(), tz)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
a <code>nanotime</code> object which must be sorted
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="precision">precision</code>
</td>
<td>
a <code>nanoduration</code> or <code>nanoperiod</code> object indicating
the rounding precision
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
for future additional arguments
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="origin">origin</code>
</td>
<td>
a <code>nanotime</code> scalar indicating the origin at which the
rounding is considered
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tz">tz</code>
</td>
<td>
a <code>character</code> scalar indicating the time zone in which to
conduct the rounding
</td>
</tr>
</table>

## Details

This flexible rounding must be understood in the context of a vector.
The rounding precision can then be considered as an interval that
defines a grid over which the elements are either assigned to the
starting value of the interval to which they belong
(<code>nano_floor</code>) or the ending value of the interval to which
they belong (<code>nano_ceiling</code>). This allows for a grouping of a
<code>nanotime</code> vector on which a statistic may then be run. In
the examples below, such a use case is shown in the context of a
<code>data.table</code> object.

If "business" concepts such as month or days are needed, the
<code>argument</code> precision must be of type <code>period</code>. It
is then mandatory to specify the timezone argument <code>tz</code> as
this ensures timezone correctness of the intervals including for example
for the rare hourly transitions of some countries going from a timezone
with a whole hour difference with UTC to one with a fractional hour
difference. In the case of a <code>period</code>, the functions align
the rounding if the precision is an integer divisor of a larger
quantity. For instance, if one specifies a rounding of 6 hours, a
divisor of a day, the hours are aligned on days and the rounding is made
to a grid at hours 0, 6, 12 and 18 in the specified timezone. If the
precision is not a divisor, the grid is aligned to the nearest hour
before the first element of the vector to round.

The argument <code>origin</code> controls the reference point of the
rounding, allowing arbitrary specification of the reference point of the
rounding.

## Examples

``` r
library("nanotime")

# "classic" rounding:
nano_floor(as.nanotime("2010-10-10 11:12:15 UTC"), as.nanoduration("01:00:00"))
# rounding with arbitrary precision:
nano_floor(as.nanotime("2010-10-10 11:12:15 UTC"), as.nanoduration("06:00:00"))
nano_floor(as.nanotime("2010-10-10 11:23:15 UTC"), as.nanoduration("00:15:00"))
nano_ceiling(as.nanotime("2010-10-10 11:23:15 UTC"), as.nanoduration("01:15:23"))
# controlling the reference point via the 'origin' argument:
nano_ceiling(as.nanotime("2010-10-10 11:23:15 UTC"),
             as.nanoduration("01:15:23"),
             origin=as.nanotime("2010-10-10 11:23:15 UTC"))
# using business concepts and rounding across a daylight saving change:
v <- seq(as.nanotime("2020-03-08 America/New_York"),
         by=as.nanoperiod("06:00:00"), length.out=8, tz="America/New_York")
print(nano_floor(v, as.nanoperiod("1d"), tz="America/New_York"), tz="America/New_York")
# using the concept in a 'data.table':
library(data.table)
n <- 3 * 24
idx <- seq(as.nanotime("2020-03-07 America/New_York"),
           by=as.nanoperiod("01:00:00"), length.out=n, tz="America/New_York")
dt <- data.table(idx, a=1:n, b=2:(n+1))
dt_mean <- dt[, list(mean = mean(a)),
              by=nano_ceiling(idx, as.nanoperiod("1d"), tz="America/New_York")]
```
