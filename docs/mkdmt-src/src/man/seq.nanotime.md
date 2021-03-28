## Sequence Generation

### Description

Generate a sequence of `nanotime`

### Usage

    ## S3 method for class 'nanotime'
    seq(from, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)

    ## S4 method for signature 'nanotime'
    seq(from, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)

### Arguments

| Argument     | Description                                                                                                                                                                                      |
|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `from, to`   | the starting and (maximal) end values of the sequence                                                                                                                                            |
| `by`         | `nanoduration` or `nanoperiod` increment of the sequence; note that if the class is `nanoperiod` the additional argument `tz` must be speficied and is of `character` type indicating a timezone |
| `length.out` | integer indicating the desired length of the sequence                                                                                                                                            |
| `along.with` | take the length from the length of this argument.                                                                                                                                                |
| `...`        | arguments passed to or from methods; the only interesting additional argument is `tz` where the `to` argument is of type `nanoperiod`                                                            |

### Examples

    ## Not run: 
    from <- as.nanotime("2018-01-14T12:44:00+00:00")
    to   <- as.nanotime("2019-01-14T12:44:00+00:00")
    seq(from, to, by=as.nanoperiod("1m"), tz="America/New_York")
    seq(from, by=as.nanoperiod("1y"), length.out=4, tz="Europe/London")

    ## End(Not run)
