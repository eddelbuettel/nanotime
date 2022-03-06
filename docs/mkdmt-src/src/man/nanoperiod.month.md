
## Nanoperiod accessors

### Description

These functions allow access to the components of a `nanoperiod`

### Usage

    ## S4 method for signature 'nanoperiod'
    nanoperiod.month(x)

    ## S4 method for signature 'nanoperiod'
    nanoperiod.day(x)

    ## S4 method for signature 'nanoperiod'
    nanoperiod.nanoduration(x)

### Arguments

| Argument | Description    |
|----------|----------------|
| `x`      | A `nanoperiod` |

### Value

`nanoperiod.month` and `nanoperiod.day` return an `integer64` whereas
`nanoperiod.nanoduration` returns a `nanoduration`

### Author(s)

Dirk Eddelbuettel

Leonardo Silvestri

### See Also

`nanoduration`

### Examples

    p <- as.nanoperiod("2y1m1d/12:00:00")
    nanoperiod.month(p)
    nanoperiod.day(p)
    nanoperiod.nanoduration(p)

