

# Test if Two Objects are (Nearly) Equal

[**Source code**](https://github.com/eddelbuettel/nanotime/tree/master/R/#L)

## Description

Compare <code>target</code> and <code>current</code> testing ‘near
equality’. If they are different, comparison is still made to some
extent, and a report of the differences is returned. Do not use
<code>all.equal</code> directly in <code>if</code> expressions—either
use <code>isTRUE(all.equal(….))</code> or <code>identical</code> if
appropriate.

## Usage

<pre><code class='language-R'>## S3 method for class 'nanotime'
all.equal(
  target,
  current,
  tolerance = sqrt(.Machine\$double.eps),
  scale = NULL,
  countEQ = FALSE,
  formatFUN = function(err, what) format(err),
  ...,
  check.attributes = TRUE
)

# S4 method for signature 'nanotime'
all.equal(
  target,
  current,
  tolerance = sqrt(.Machine\$double.eps),
  scale = NULL,
  countEQ = FALSE,
  formatFUN = function(err, what) format(err),
  ...,
  check.attributes = TRUE
)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="target">target</code>, <code id="current">current</code>
</td>
<td>
<code>nanotime</code> arguments to be compared
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="tolerance">tolerance</code>
</td>
<td>
numeric \>= 0. Differences smaller than <code>tolerance</code> are not
reported. The default value is close to <code>1.5e-8</code>.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="scale">scale</code>
</td>
<td>
<code>NULL</code> or numeric \> 0, typically of length 1 or
<code>length(target)</code>. See ‘Details’.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="countEQ">countEQ</code>
</td>
<td>
logical indicating if the <code>target == current</code> cases should be
counted when computing the mean (absolute or relative) differences. The
default, <code>FALSE</code> may seem misleading in cases where
<code>target</code> and <code>current</code> only differ in a few
places; see the extensive example.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="formatFUN">formatFUN</code>
</td>
<td>
a <code>function</code> of two arguments, <code>err</code>, the
relative, absolute or scaled error, and <code>what</code>, a character
string indicating the *kind* of error; maybe used, e.g., to format
relative and absolute errors differently.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
further arguments for different methods
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="check.attributes">check.attributes</code>
</td>
<td>
logical indicating if the <code>attributes</code> of <code>target</code>
and <code>current</code> (other than the names) should be compared.
</td>
</tr>
</table>

## See Also

<code>identical</code>, <code>isTRUE</code>, <code>==</code>, and
<code>all</code> for exact equality testing.
