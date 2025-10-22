

# Get a component of a date time

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

Get a component of a date time. <code>nano_wday</code> returns the
numeric position in a week, with Sunday == 0. <code>nano_mday</code>
returns the numeric day (i.e. a value from 1 to 31).
<code>nano_month</code> returns the month (i.e. a value from 1 to 12).
<code>nano_year</code> returns the year.

## Usage

<pre><code class='language-R'>nano_wday(x, tz)

nano_mday(x, tz)

nano_month(x, tz)

nano_year(x, tz)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
a <code>nanotime</code> object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tz">tz</code>
</td>
<td>
<code>character</code> a string representing a timezone
</td>
</tr>
</table>

## Details

Note that the <code>tz</code> parameter is mandatory because the day
boundary is different depending on the time zone and
<code>nanotime</code> does not store the timezone as it is just an
offset in nanoseconds from the epoch.

## Examples

``` r
library("nanotime")

nano_wday(as.nanotime("2020-03-14 23:32:00-04:00"), "America/New_York")
nano_wday(as.nanotime("2020-03-14 23:32:00 America/New_York"), "Europe/Paris")
nano_mday(as.nanotime("2020-03-14 23:32:00-04:00"), "America/New_York")
nano_mday(as.nanotime("2020-03-14 23:32:00 America/New_York"), "Europe/Paris")
nano_month(as.nanotime("2020-12-31 23:32:00-04:00"), "America/New_York")
nano_month(as.nanotime("2020-12-31 23:32:00 America/New_York"), "Europe/Paris")
nano_year(as.nanotime("2020-12-31 23:32:00-04:00"), "America/New_York")
nano_year(as.nanotime("2020-12-31 23:32:00 America/New_York"), "Europe/Paris")
```
