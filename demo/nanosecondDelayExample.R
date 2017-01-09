library(nanotime)
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

set.seed(42)
N <- 300
rainyday <- ISOdatetime(2016,9,28,8,30,0) # made up
shinyday <- ISOdatetime(2016,9,21,8,30,0) # made up too
rdsent <- nanotime(rainyday) + cumsum(10*rpois(N, lambda=4)) 	# random sent time 
sdsent <- nanotime(shinyday) + cumsum(10*rpois(N, lambda=4))	# identical sent process for both
rdrecv <- rdsent + 10*rlnorm(N, 0.30, 0.25)                     # postulate higher mean and sd
sdrecv <- sdsent + 10*rlnorm(N, 0.10, 0.20)		            	# for rainy than shiny
raw <- data.table(rdsent, rdrecv, sdsent, sdrecv)
raw[, `:=`(rainy=as.numeric(rdrecv-rdsent),
           shiny=as.numeric(sdrecv-sdsent))]

## melt into long format
plotdata <- melt(raw[,.(rainy,shiny)], measure.vars=1:2, variable.name="day", value.name="time")
## and plot
ggplot(plotdata, aes(day, time)) + geom_violin(aes(fill=day)) + coord_flip() + 
    ylab("Message Time in Nanoseconds") + xlab("Weather Conditions") +
    ggtitle("Nanosecond Delay", "Under Different Weather Conditions")

tfile <- tempfile(pattern="raw", fileext=".csv")
fwrite(raw, file=tfile)
cooked <- fread(tfile)
## csv files are not 'typed' so need to recover types explicitly
cooked[, `:=`(rdsent=nanotime(rdsent),
              rdrecv=nanotime(rdrecv),
              sdsent=nanotime(sdsent),
              sdrecv=nanotime(sdrecv))]
## now saved and restrored data are identical
all.equal(raw, cooked)

