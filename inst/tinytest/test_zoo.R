
library(nanotime)

isSolaris <- Sys.info()[["sysname"]] == "SunOS"

if (isSolaris) exit_file("Skipping tests on Solaris.")

#test_zoo <- function() {
suppressMessages(library(zoo))
set.seed(42)
x <- 100 + cumsum(rnorm(10))
y <- 100 + cumsum(rnorm(10))
mat <- cbind(x, y)
now <- nanotime("2017-02-06T03:36:10.626159000+00:00")
z <- zoo(mat, now + 11*(0:9))           # inc. by 11 to more visible, 1 works too
oldOptions <- getOption("nanotimeFormat")
options(nanotimeFormat = "%Y-%m-%dT%H:%M:%E9S%Ez")
indexCharacter <- format(index(z))
options(nanotimeFormat = oldOptions)
expect_equal(indexCharacter,
             c("2017-02-06T03:36:10.626159000+00:00",
               "2017-02-06T03:36:10.626159011+00:00",
               "2017-02-06T03:36:10.626159022+00:00",
               "2017-02-06T03:36:10.626159033+00:00",
               "2017-02-06T03:36:10.626159044+00:00",
               "2017-02-06T03:36:10.626159055+00:00",
               "2017-02-06T03:36:10.626159066+00:00",
               "2017-02-06T03:36:10.626159077+00:00",
               "2017-02-06T03:36:10.626159088+00:00",
               "2017-02-06T03:36:10.626159099+00:00"))
