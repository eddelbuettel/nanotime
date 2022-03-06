
## Test if Two Objects are (Nearly) Equal

### Description

Compare `target` and `current` testing ‘near equality’. If they are
different, comparison is still made to some extent, and a report of the
differences is returned. Do not use `all.equal` directly in `if`
expressions—either use `isTRUE(all.equal(....))` or `identical` if
appropriate.

### Usage

    ## S3 method for class 'nanoperiod'
    all.equal(target, current, ..., check.attributes = TRUE)

    ## S4 method for signature 'nanoperiod'
    all.equal(target, current, ..., check.attributes = TRUE)

### Arguments

| Argument           | Description                                                                                                 |
|--------------------|-------------------------------------------------------------------------------------------------------------|
| `target, current`  | `nanoperiod` arguments to be compared                                                                       |
| `...`              | further arguments for different methods                                                                     |
| `check.attributes` | logical indicating if the `attributes` of `target` and `current` (other than the names) should be compared. |

### See Also

`identical`, `isTRUE`, `==`, and `all` for exact equality testing.

