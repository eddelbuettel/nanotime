

## Test if Two Objects are (Nearly) Equal

### Description

Compare `target` and `current` testing ‘near equality’. If they are
different, comparison is still made to some extent, and a report of the
differences is returned. Do not use `all.equal` directly in `if`
expressions—either use `isTRUE(all.equal(....))` or `identical` if
appropriate.

### Usage

``` R
## S3 method for class 'nanoduration'
all.equal(
  target,
  current,
  tolerance = sqrt(.Machine$double.eps),
  scale = NULL,
  countEQ = FALSE,
  formatFUN = function(err, what) format(err),
  ...,
  check.attributes = TRUE
)

## S4 method for signature 'nanoduration'
all.equal(
  target,
  current,
  tolerance = sqrt(.Machine$double.eps),
  scale = NULL,
  countEQ = FALSE,
  formatFUN = function(err, what) format(err),
  ...,
  check.attributes = TRUE
)
```

### Arguments

|                     |                                                                                                                                                                                                                                                                          |
|---------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `target`, `current` | `nanoduration` arguments to be compared                                                                                                                                                                                                                                  |
| `tolerance`         | numeric \>= 0. Differences smaller than `tolerance` are not reported. The default value is close to `1.5e-8`.                                                                                                                                                            |
| `scale`             | `NULL` or numeric \> 0, typically of length 1 or `length(target)`. See ‘Details’.                                                                                                                                                                                        |
| `countEQ`           | logical indicating if the `target == current` cases should be counted when computing the mean (absolute or relative) differences. The default, `FALSE` may seem misleading in cases where `target` and `current` only differ in a few places; see the extensive example. |
| `formatFUN`         | a `function` of two arguments, `err`, the relative, absolute or scaled error, and `what`, a character string indicating the \_kind\_ of error; maybe used, e.g., to format relative and absolute errors differently.                                                     |
| `...`               | further arguments for different methods                                                                                                                                                                                                                                  |
| `check.attributes`  | logical indicating if the `attributes` of `target` and `current` (other than the names) should be compared.                                                                                                                                                              |

### See Also

`identical`, `isTRUE`, `==`, and `all` for exact equality testing.


