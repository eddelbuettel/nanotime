

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

<pre><code class='language-R'>## S3 method for class 'nanoival'
all.equal(target, current, ..., check.attributes = TRUE)

# S4 method for signature 'nanoival'
all.equal(target, current, ..., check.attributes = TRUE)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="target">target</code>, <code id="current">current</code>
</td>
<td>
<code>nanoival</code> arguments to be compared
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
