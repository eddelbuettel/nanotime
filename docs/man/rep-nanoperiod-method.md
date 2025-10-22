

# Replicate Elements

## Description

Replicates the values in ‘x’ similarly to the default method.

## Usage

<pre><code class='language-R'>## S4 method for signature 'nanoperiod'
rep(x, ...)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
a vector of <code>nanoperiod</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>

further arguments:

‘times’ an integer-valued vector giving the (non-negative) number of
times to repeat each element if of length ‘length(x)’, or to repeat the
whole vector if of length 1. Negative or ‘NA’ values are an error. A
‘double’ vector is accepted, other inputs being coerced to an integer or
double vector.

‘length.out’ non-negative integer. The desired length of the output
vector. Other inputs will be coerced to a double vector and the first
element taken. Ignored if ‘NA’ or invalid.

‘each’ non-negative integer. Each element of ‘x’ is repeated ‘each’
times. Other inputs will be coerced to an integer or double vector and
the first element taken. Treated as ‘1’ if ‘NA’ or invalid.
</td>
</tr>
</table>
