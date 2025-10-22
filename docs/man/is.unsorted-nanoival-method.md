

# Test if a <code>nanoival</code> vector is Not Sorted

## Description

Test if an object is not sorted (in increasing order), without the cost
of sorting it.

## Usage

<pre><code class='language-R'>## S4 method for signature 'nanoival'
is.unsorted(x, na.rm = FALSE, strictly = FALSE)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
a <code>nanoival</code> vector
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="na.rm">na.rm</code>
</td>
<td>
logical. Should missing values be removed before checking?
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="strictly">strictly</code>
</td>
<td>
logical indicating if the check should be for *strictly* increasing
values.
</td>
</tr>
</table>

## See Also

<code>sort</code>
