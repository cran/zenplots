## Tools for *_*_graphics functions


##' @title Function to set up plot region for graphics plots
##' @param xlim x-axis limits
##' @param ylim y-axis limits
##' @param plot... arguments passed to the underlying 'plot()'
##' @return invisible()
##' @author Marius Hofert
plot_region <- function(xlim, ylim, plot... = NULL)
{
    if(is.null(plot...)) {
        plot(NA, xlim = xlim, ylim = ylim, type = "n", ann = FALSE, axes = FALSE, log = "")
    } else {
        fun <- function(...) plot(NA, xlim = xlim, ylim = ylim, ...)
        do.call(fun, plot...)
    }
}
