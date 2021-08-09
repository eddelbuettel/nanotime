## Replicate Elements

### Description

Replicates the values in 'x' similarly to the default method.

### Usage

    ## S4 method for signature 'nanoival'
    rep(x, ...)

### Arguments

<table data-summary="R argblock">
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr class="header">
<th>Argument</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd" data-valign="top">
<td><code>x</code></td>
<td><p>a vector of <code>nanoival</code></p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>...</code></td>
<td><p>further arguments:</p>
<p>'times' an integer-valued vector giving the (non-negative) number of times to repeat each element if of length 'length(x)', or to repeat the whole vector if of length 1. Negative or 'NA' values are an error. A 'double' vector is accepted, other inputs being coerced to an integer or double vector.</p>
<p>'length.out' non-negative integer. The desired length of the output vector. Other inputs will be coerced to a double vector and the first element taken. Ignored if 'NA' or invalid.</p>
<p>'each' non-negative integer. Each element of 'x' is repeated 'each' times. Other inputs will be coerced to an integer or double vector and the first element taken. Treated as '1' if 'NA' or invalid.</p></td>
</tr>
</tbody>
</table>
