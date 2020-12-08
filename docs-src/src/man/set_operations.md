## Set operations

### Description

Performs set intersection, union and difference between vectors of
temporal types from the `nanotime` package.

### Usage

    ## S4 method for signature 'nanoival,nanoival'
    intersect(x, y)

    ## S4 method for signature 'nanoival,nanoival'
    union(x, y)

    ## S4 method for signature 'nanoival,nanoival'
    setdiff(x, y)

    ## S4 method for signature 'nanotime,nanoival'
    intersect.idx(x, y)

    ## S3 method for class 'nanotime'
    x %in% table

    ## S4 method for signature 'nanotime,nanoival'
    intersect(x, y)

    ## S4 method for signature 'nanotime,nanoival'
    setdiff(x, y)

    ## S4 method for signature 'nanotime,nanoival'
    setdiff.idx(x, y)

    ## S4 method for signature 'nanotime,nanotime'
    intersect(x, y)

    ## S4 method for signature 'nanotime,nanotime'
    union(x, y)

    ## S4 method for signature 'nanotime,nanotime'
    setdiff(x, y)

### Arguments

| Argument | Description                |
|----------|----------------------------|
| `x, y`   | a temporal type            |
| `table`  | `nanoival`: used in `%in%` |

### Details

Set operations between `nanoival` operands allow the construction of
complex interval vectors (i.e. a `nanoival` vector can specify any
number of inclusions and exclusions of time). Set operations between
`nanotime` and `nanoival` allow to subset time vectors with interval
vectors. In addition to the generic set functions, the function
`intersect.idx` is defined which returns the indices of the
intersection, and the operator `%in%` is overloaded for
`nanotime-nanoival` which returns a logical vector that indicates which
elements belong to the interval vector.

### Value

`intersect`, `union`, `setdiff` return temporal types that are the
result of the intersection. For instance, set operations on two
`nanoival` return a `nanoival`, whereas intersection between a
`nanoival` and a `nanotime` returns a `nanotime`. `intersect.idx` return
a list of vectors representing the element indices that intersect and
`setdiff.idx` returns a vector representing the element indices to be
removed.

### Examples

    ## Not run: 
    ## a vector of 'nanotime' can be subsetted by a 'nanoival' which is equivalent to 'intersect':
    one_second <- 1e9
    a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
    idx <- c(as.nanoival("-2012-12-12 12:12:10+00:00 -> 2012-12-12 12:12:14+00:00-"),
             as.nanoival("+2012-12-12 12:12:18+00:00 -> 2012-12-12 12:12:20+00:00+"))
    a[idx]
    intersect(a, idx)

    ## 'nanoival' also has the set operations 'union', 'intersect', 'setdiff':
    a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
    i <- as.nanoival("-2012-12-12 12:12:14+00:00 -> 2012-12-12 12:12:18+00:00-")
    setdiff(a, i)

    i1 <- as.nanoival("+2012-12-12 12:12:14+00:00 -> 2012-12-12 12:12:17+00:00-")
    i2 <- as.nanoival("+2012-12-12 12:12:16+00:00 -> 2012-12-12 12:12:18+00:00-")
    union(i1, i2)

    ## 'intersect.idx' returns the indices of the intersection:
    a <- seq(nanotime("2012-12-12 12:12:12+00:00"), length.out=10, by=one_second)
    idx <- as.nanoival("+2012-12-12 12:12:14+00:00 -> 2012-12-12 12:12:19+00:00+")
    idx_intersect <- intersect.idx(a, idx)

    ## Intersection can be performed using these indices:
    a[idx_intersect$x]

    ## which is equivalent to:
    a[idx]

    ## The logical vector indicating intersection can be obtained like this:
    a %in% idx

    ## End(Not run)
