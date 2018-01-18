## Tools for *_*_grid functions


##' @title Constructing the viewports for grid functions
##' @param ispace inner space (in [0,1])
##' @param xlim x-axis limits; if NULL, the data limits are used
##' @param ylim y-axis limits; if NULL, the data limits are used
##' @param x x data (only used if is.null(xlim)); if NULL, 0:1 is used
##' @param y y data (only used if is.null(ylim)); if NULL, 0:1 is used
##' @param ... Additional arguments passed to the underlying viewport()
##' @return The viewport
##' @author Marius Hofert
##' @note Ideas from dataViewport() and extendrange()
##'       Omitted check:
##'       if(length(ispace) != 4) ispace <- rep(ispace, length.out = 4)
##'       stopifnot(0 <= ispace, ispace <= 1)
vport <- function(ispace, xlim = NULL, ylim = NULL, x = NULL, y = NULL, ...)
{
  if(is.null(xlim) && is.null(ylim) && is.null(x) && is.null(y)) {
    ## Non-data viewport
    viewport(x = unit(ispace[2], "npc"),
             y = unit(ispace[1], "npc"),
             just = c("left", "bottom"),
             width  = unit(1-sum(ispace[c(2,4)]), "npc"),
             height = unit(1-sum(ispace[c(1,3)]), "npc"))
  } else {
    ## Data viewport
    ran.x <- if(is.null(xlim)) {
      if(is.null(x)) x <- 0:1
      range(x, na.rm = TRUE)
    } else xlim
    ran.y <- if(is.null(ylim)) {
      if(is.null(y)) y <- 0:1
      range(y, na.rm = TRUE)
    } else ylim
    viewport(xscale = ran.x + c(-ispace[2], ispace[4]) * diff(ran.x),
             yscale = ran.y + c(-ispace[1], ispace[3]) * diff(ran.y), ...)
  }
}

