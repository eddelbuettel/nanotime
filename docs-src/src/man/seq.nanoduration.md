## Sequence Generation

### Description

Generate a sequence of `nanoduration`

### Usage

    ## S3 method for class 'nanoduration'
    seq(from, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)

### Arguments

| Argument     | Description                                           |
|--------------|-------------------------------------------------------|
| `from, to`   | the starting and (maximal) end values of the sequence |
| `by`         | the increment of the sequence                         |
| `length.out` | integer indicating the desired length of the sequence |
| `along.with` | take the length from the length of this argument.     |
| `...`        | arguments passed to or from methods                   |

### Examples

    seq(from=as.nanoduration(0), by=as.nanoduration("01:00:00"), length.out=10)
