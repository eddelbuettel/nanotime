if (requireNamespace("ggplot2", quietly=TRUE) &&
    requireNamespace("scales", quietly=TRUE)  &&
    requireNamespace("data.table", quietly=TRUE)) {

    library(nanotime)
    library(ggplot2)
    library(scales)
    library(data.table)


    nanotime_breaks <- function(n=5) function(x) nanotime(pretty_breaks(n)(as.numeric(x)))

    nanotime_format <- function(format = getOption("nanotimeFormat", default="%Y-%m-%dT%H:%M:%EXS%Ez"),
                                tz     = getOption("nanotimeTz", default="UTC")) {
        function(x) format(x, format, tz=tz)
    }

    nanotime_trans <- function(tz = NULL) {
        
        to_time <- function(x) {
            nanotime(x)
        }
        
        from_time <- function(x) {
            if (!inherits(x, "nanotime")) {
                stop("Invalid input: nanotime_trans works with objects of class ",
                     "nanotime only", call. = FALSE)
            }
            structure(as.numeric(x), names = names(x))
        }

        trans_new("nanotime", "from_time", "to_time",
                  breaks=nanotime_breaks(),
                  format=nanotime_format(tz=tz))
    }

    is.waive <- function(x) inherits(x, "waiver")

    
    ScaleContinuousNanotime <-
        ggproto("ScaleContinuousNanotime", ScaleContinuous,
                secondary.axis = waiver(),
                timezone = NULL,
                transform = function(self, x) {
                    ggproto_parent(ScaleContinuous, self)$transform(x)
                },
                map = function(self, x, limits = self$get_limits()) {
                    self$oob(x, limits)
                },
                break_info = function(self, range = NULL) {
                    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
                    if (!(is.waive(self$secondary.axis) || self$secondary.axis$empty())) {
                        self$secondary.axis$init(self)
                        breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
                    }
                    breaks
                },
                sec_name = function(self) {
                    if (is.waive(self$secondary.axis)) {
                        waiver()
                    } else {
                        self$secondary.axis$name
                    }
                },
                make_sec_title = function(self, title) {
                    if (!is.waive(self$secondary.axis)) {
                        self$secondary.axis$make_title(title)
                    } else {
                        ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
                    }
                })

    scale_type.nanotime <- function(x) "nanotime"

    nanotime_scale <- function(aesthetics, trans, palette,
                               breaks = pretty_breaks(), minor_breaks = waiver(),
                               labels = waiver(), date_breaks = waiver(),
                               date_labels = waiver(),
                               date_minor_breaks = waiver(), timezone = NULL,
                               guide = "legend", ...) {
        continuous_scale(
            aesthetics,
            "nanotime",
            palette = palette,
            breaks = breaks,
            minor_breaks = minor_breaks,
            labels = labels,
            guide = guide,
            trans = trans,
            ...,
            super = ScaleContinuousNanotime
        )}


    ## copy from `ggplot2` a function that is not exported:
    set_sec_axis <- function(sec.axis, scale) {
        if (!is.waive(sec.axis)) {
            if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
            if (!is.sec_axis(sec.axis)) abort("Secondary axes must be specified using 'sec_axis()'")
            scale$secondary.axis <- sec.axis
        }
        return(scale)
    }


    ## `ggplot2` code will location these functions by building a name with "scale_", "x" or "y", "_", "nanotime":
    scale_x_nanotime <- function(name = waiver(),
                                 breaks = waiver(),
                                 date_breaks = waiver(),
                                 labels = waiver(),
                                 date_labels = waiver(),
                                 minor_breaks = waiver(),
                                 date_minor_breaks = waiver(),
                                 timezone = NULL,
                                 limits = NULL,
                                 expand = waiver(),
                                 guide = waiver(),
                                 position = "bottom",
                                 sec.axis = waiver()) {
        sc  <- nanotime_scale(aesthetics=c("x", "xmin", "xmax", "xend"),
                              trans="nanotime",
                              name = name,
                              palette = identity,
                              breaks = breaks,
                              date_breaks = date_breaks,
                              labels = labels,
                              date_labels = date_labels,
                              minor_breaks = minor_breaks,
                              date_minor_breaks = date_minor_breaks,
                              timezone = timezone,
                              guide = guide,
                              limits = limits,
                              expand = expand,
                              position = position
                              )

        set_sec_axis(sec.axis, sc)
    }

    
    scale_y_nanotime <- function(name = waiver(),
                                 breaks = waiver(),
                                 date_breaks = waiver(),
                                 labels = waiver(),
                                 date_labels = waiver(),
                                 minor_breaks = waiver(),
                                 date_minor_breaks = waiver(),
                                 limits = NULL,
                                 expand = waiver(),
                                 guide = waiver(),
                                 position = "left",
                                 sec.axis = waiver()) {
        nanotime_scale(
            aesthetics=c("y", "ymin", "ymax", "yend"),
            trans="nanotime",
            name = name,
            palette = identity,
            breaks = breaks,
            date_breaks = date_breaks,
            labels = labels,
            date_labels = date_labels,
            minor_breaks = minor_breaks,
            date_minor_breaks = date_minor_breaks,
            guide = guide,
            limits = limits,
            expand = expand,
            position = position
        )
    }



    ## now, test the above:
    ## -------------------

    ## create `data.table` with `idx` of type `nanotime`:
    idx <- nanotime(1) + 1e9*(1:100)
    dt <- data.table(idx=idx, a=1:100, b=11:110)

    ## nanotime on the x-axis:
    ggplot(dt, aes(x=idx, y=b)) +
        geom_line()

    ## nanotime on the y-axis:
    ggplot(dt, aes(x=b, y=idx)) +
        geom_line()
    
    ## change the format of the tick labels:
    ggplot(dt, aes(x=idx, y=b)) +
        geom_line() +
        scale_x_nanotime(labels=nanotime_format("%H:%M:%S"))    
  
    ## change format and write tick labels diagonally:
    ggplot(dt, aes(x=idx, y=b)) +
        geom_line() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_nanotime(labels = nanotime_format("%Y-%m-%d %H:%M:%S"))




}
