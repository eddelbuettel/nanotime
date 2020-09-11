## Test if a `nanoival` vector is Not Sorted

### Description

Test if an object is not sorted (in increasing order), without the cost
of sorting it.

### Usage

    ## S4 method for signature 'nanoival'
    is.unsorted(x, na.rm = FALSE, strictly = FALSE)

### Arguments

| Argument   | Description                                                                   |
| ---------- | ----------------------------------------------------------------------------- |
| `x`        | a `nanoival` vector                                                           |
| `na.rm`    | logical. Should missing values be removed before checking?                    |
| `strictly` | logical indicating if the check should be for \_strictly\_ increasing values. |

### See Also

`sort`
