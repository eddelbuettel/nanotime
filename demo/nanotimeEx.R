library(nanotime)

set.seed(42)
N <- 300
shine <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4))
rain <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4) + round(runif(N+1)*50))

library(data.table)
library(bit64)
raw <- data.table(shine=shine, rain=rain)
fwrite(raw, file="/tmp/raw.csv")
cooked <- fread("/tmp/raw.csv")

df <- data.frame(val=as.numeric(c(diff(rain),diff(shine))), # need to cast to numeric after diffs
                 key=rep(c("rain", "shine"), each=N))
head(df)

## now on differences
ddf <- data.frame(val=as.numeric(c(diff(rain),diff(shine))), # need to cast to numeric after diffs
                    key=rep(c("rain", "shine"), each=N))
head(df)
library(ggplot2)
ggplot(ddf, aes(key, val)) + geom_violin(aes(fill=key)) + coord_flip()
