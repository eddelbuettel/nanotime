library(nanotime)

## nanotime
##test_nanotime_data_frame_constructor <- function() {
## simpler data.frame, inserts 'formatted'
set.seed(42)
N <- 300
shine <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4))
rain  <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4) + round(runif(N+1)*25))
newdf <- data.frame(rain=rain, shine=shine)
expect_identical(newdf$rain,  rain)
expect_identical(newdf$shine, shine)

##test_nanotime_data_frame_rbind <- function() {
t1 <- nanotime(1:10)
d1 <- 1:10
t2 <- nanotime(11:20)
d2 <- 11:20
df1 <- data.frame(t = t1, d = d1)
df2 <- data.frame(t = t2, d = d2)
df <- rbind(df1, df2)
expect_identical(df, data.frame(t = c(t1, t2), d = c(d1, d2)))

##test_nanotime_data_frame_cbind <- function() {
t1 <- nanotime(1:10)
d1 <- 1:10
t2 <- nanotime(11:20)
d2 <- 11:20
df1 <- data.frame(t1 = t1, d1 = d1)
df2 <- data.frame(t2 = t2, d2 = d2)
df <- cbind(df1, df2)
expect_identical(df, data.frame(t1 = t1, d1 = d1, t2=t2, d2=d2))


## nanoduration
##test_nanoduration_data_frame_constructor <- function() {
## simpler data.frame, inserts 'formatted'
set.seed(42)
N <- 300
shine <- as.nanoduration(1000) + cumsum(10*rpois(n=N+1, lambda=4))
rain  <- as.nanoduration(2000) + cumsum(10*rpois(n=N+1, lambda=4) + round(runif(N+1)*25))
newdf <- data.frame(rain=rain, shine=shine)
expect_identical(newdf$rain,  rain)
expect_identical(newdf$shine, shine)

##test_nanoduration_data_frame_rbind <- function() {
t1 <- as.nanoduration(1:10)
d1 <- 1:10
t2 <- as.nanoduration(11:20)
d2 <- 11:20
df1 <- data.frame(t = t1, d = d1)
df2 <- data.frame(t = t2, d = d2)
df <- rbind(df1, df2)
expect_identical(df, data.frame(t = c(t1, t2), d = c(d1, d2)))


##test_nanoduration_data_frame_cbind <- function() {
t1 <- as.nanoduration(1:10)
d1 <- 1:10
t2 <- as.nanoduration(11:20)
d2 <- 11:20
df1 <- data.frame(t1 = t1, d1 = d1)
df2 <- data.frame(t2 = t2, d2 = d2)
df <- cbind(df1, df2)
expect_identical(df, data.frame(t1 = t1, d1 = d1, t2=t2, d2=d2))


## nanoperiod
##test_nanoperiod_data.frame_constructor  <- function() {
## simpler data.frame, inserts 'formatted'
set.seed(42)
N <- 300
shine <- as.nanoperiod(1000) + cumsum(10*rpois(n=N+1, lambda=4))
rain  <- as.nanoperiod(2000) + cumsum(10*rpois(n=N+1, lambda=4) + round(runif(N+1)*25))
newdf <- data.frame(rain=rain, shine=shine)
expect_identical(newdf$rain,  rain)
expect_identical(newdf$shine, shine)

##test_nanoperiod_data_frame_rbind <- function() {
t1 <- as.nanoperiod(1:10)
d1 <- 1:10
t2 <- as.nanoperiod(11:20)
d2 <- 11:20
df1 <- data.frame(t = t1, d = d1)
df2 <- data.frame(t = t2, d = d2)
df <- rbind(df1, df2)
expect_identical(df, data.frame(t = c(t1, t2), d = c(d1, d2)))

##test_nanoperiod_data_frame_cbind <- function() {
t1 <- as.nanoperiod(1:10)
d1 <- 1:10
t2 <- as.nanoperiod(11:20)
d2 <- 11:20
df1 <- data.frame(t1 = t1, d1 = d1)
df2 <- data.frame(t2 = t2, d2 = d2)
df <- cbind(df1, df2)
expect_identical(df, data.frame(t1 = t1, d1 = d1, t2=t2, d2=d2))


## nanoival
##test_nanoival_data.frame_constructor  <- function() {
## simpler data.frame, inserts 'formatted'
set.seed(42)
N <- 300
shine_start <- nanotime(1000) + cumsum(10*rpois(n=N+1, lambda=4))
shine_end   <- shine_start + as.nanoduration("01:00:00")
rain_start  <- nanotime(2000) + cumsum(10*rpois(n=N+1, lambda=4) + round(runif(N+1)*25))
rain_end    <- shine_end + as.nanoduration("02:00:00")
shine <- nanoival(shine_start, shine_end)
rain  <- nanoival(rain_start, rain_end) 
newdf <- data.frame(rain=rain, shine=shine)
expect_identical(newdf$rain,  rain)
expect_identical(newdf$shine, shine)

##test_nanoival_data_frame_rbind <- function() {
t1 <- nanoival(nanotime(1:10), nanotime(2:11))
d1 <- 1:10
t2 <- nanoival(nanotime(11:20), nanotime(12:21))
d2 <- 11:20
df1 <- data.frame(t = t1, d = d1)
df2 <- data.frame(t = t2, d = d2)
df <- rbind(df1, df2)
expect_identical(df, data.frame(t = c(t1, t2), d = c(d1, d2)))

##test_nanoival_data_frame_cbind <- function() {
t1 <- nanoival(nanotime(1:10), nanotime(2:11))
d1 <- 1:10
t2 <- nanoival(nanotime(11:20), nanotime(12:21))
d2 <- 11:20
df1 <- data.frame(t1 = t1, d1 = d1)
df2 <- data.frame(t2 = t2, d2 = d2)
df <- cbind(df1, df2)
expect_identical(df, data.frame(t1 = t1, d1 = d1, t2=t2, d2=d2))

