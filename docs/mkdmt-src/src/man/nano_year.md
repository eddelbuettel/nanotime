
## Get a component of a date time

### Description

Get a component of a date time. `nano_wday` returns the numeric position
in a week, with Sunday == 0. `nano_mday` returns the numeric day (i.e. a
value from 1 to 31). `nano_month` returns the month (i.e. a value from 1
to 12). `nano_year` returns the year.

### Usage

    nano_wday(x, tz)

    nano_mday(x, tz)

    nano_month(x, tz)

    nano_year(x, tz)

### Arguments

| Argument | Description                                  |
|----------|----------------------------------------------|
| `x`      | a `nanotime` object                          |
| `tz`     | `character` a string representing a timezone |

### Details

Note that the `tz` parameter is mandatory because the day boundary is
different depending on the time zone and `nanotime` does not store the
timezone as it is just an offset in nanoseconds from the epoch.

### Examples

    ## Not run: 
    nano_wday(as.nanotime("2020-03-14 23:32:00-04:00"), "America/New_York")
    nano_wday(as.nanotime("2020-03-14 23:32:00 America/New_York"), "Europe/Paris")
    nano_mday(as.nanotime("2020-03-14 23:32:00-04:00"), "America/New_York")
    nano_mday(as.nanotime("2020-03-14 23:32:00 America/New_York"), "Europe/Paris")
    nano_month(as.nanotime("2020-12-31 23:32:00-04:00"), "America/New_York")
    nano_month(as.nanotime("2020-12-31 23:32:00 America/New_York"), "Europe/Paris")
    nano_year(as.nanotime("2020-12-31 23:32:00-04:00"), "America/New_York")
    nano_year(as.nanotime("2020-12-31 23:32:00 America/New_York"), "Europe/Paris")

    ## End(Not run)

