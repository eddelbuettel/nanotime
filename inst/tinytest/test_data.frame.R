test_data_frame_constructor <- function() {
  ## simpler data.frame, inserts 'formatted'
  set.seed(42)
  N <- 300
  shine <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4))
  rain  <- nanotime(Sys.time()) + cumsum(10*rpois(n=N+1, lambda=4) + round(runif(N+1)*25))
  newdf <- data.frame(rain=rain, shine=shine)
  checkEquals(newdf$rain,  rain)
  checkEquals(newdf$shine, shine)
}
test_data_frame_rbind <- function() {
  t1 <- nanotime(1:10)
  d1 <- 1:10
  t2 <- nanotime(11:20)
  d2 <- 11:20
  df1 <- data.frame(t = t1, d = d1)
  df2 <- data.frame(t = t2, d = d2)
  df <- rbind(df1, df2)
  checkEquals(df, data.frame(t = c(t1, t2), d = c(d1, d2)))
}
test_data_frame_cbind <- function() {
  t1 <- nanotime(1:10)
  d1 <- 1:10
  t2 <- nanotime(11:20)
  d2 <- 11:20
  df1 <- data.frame(t1 = t1, d1 = d1)
  df2 <- data.frame(t2 = t2, d2 = d2)
  df <- cbind(df1, df2)
  checkEquals(df, data.frame(t1 = t1, d1 = d1, t2=t2, d2=d2))
}
