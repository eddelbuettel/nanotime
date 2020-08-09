
library(nanotime)

set.seed(42)
N <- 300
shine <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4))
rain <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4) + round(runif(N+1)*25))

if (requireNamespace("data.table", quietly=TRUE)) {
    suppressMessages(library(data.table))
    suppressMessages(library(bit64))
    raw <- data.table(shine=shine, rain=rain)

    df <- data.frame(val=c(rain,shine), key=rep(c("rain", "shine"), each=N+1))
    head(df)

    ## now on differences
    ddf <- data.frame(val=as.numeric(c(diff(rain),diff(shine))), # need to cast to numeric after diffs
                      key=rep(c("rain", "shine"), each=N))
    head(ddf)

    ## simpler data.frame, inserts 'formatted'
    newdf <- data.frame(rain=rain, shine=shine)
    head(newdf)
}
