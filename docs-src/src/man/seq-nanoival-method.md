## Sequence Generation

### Description

Generate a sequence of `nanoival`

### Usage

    ## S4 method for signature 'nanoival'
    seq(from, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)

### Arguments

| Argument     | Description                                                                                                                                                                                      |
| ------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `from, to`   | the starting and (maximal) end values of the sequence                                                                                                                                            |
| `by`         | `nanoduration` or `nanoperiod` increment of the sequence; note that if the class is `nanoperiod` the additional argument `tz` must be speficied and is of `character` type indicating a timezone |
| `length.out` | an integer desired length of the sequence                                                                                                                                                        |
| `along.with` | take the length from the length of this argument.                                                                                                                                                |
| `...`        | arguments passed to or from methods; the only interesting additional argument is `tz` where the `to` argument is of type `nanoperiod`                                                            |

### Examples

    ## Not run: 
    from <- as.nanoival("-2018-01-14T13:00:00+00:00 -> 2018-01-14T15:00:00+00:00+")
    seq(from, by=as.nanoperiod("1m"), length.out=5, tz="America/New_York")
    
    ## End(Not run)
