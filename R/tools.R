## Tools for constructing your own plot1d and plot2d functions


##' @title Determining the indices of the x and y variables of the current plot
##' @param zargs The argument list as passed from zenplot().
##' @return A vector of length two giving the plot variables (integers)
##' @author Marius Hofert
##' @note This is exported so that one doesn't always have to figure
##'       out whether the variables (axes) in the current plot need
##'       to be switched manually.
plot_indices <- function(zargs) zargs$vars[zargs$num,]

##' @title Auxiliary function for burst()
##' @param x A vector, matrix or data.frame (or a (pure) list, but that we don't use here)
##' @param labs The variable labels:
##'        - if NULL, no labels are used
##'        - if of length 1, use this label and append 1:ncol(x)
##'          but only if x doesn't have any column names (otherwise use the latter)
##'        - if of length ncol(x), use that
##'          but only if x doesn't have any column names (otherwise use the latter)
##' @return 'x' as a list of named columns
##' @author Marius Hofert
##' @note - Performance critical (no checks here)
##'       - Data frames always have default names. They are possibly
##'         ugly but we have to use them here as we cannot
##'         determine whether they were assigned automatically or
##'         on purpose.
burst_ <- function(x, labs = "V")
{
    ## Construct labels
    if(is.vector(x)) x <- as.matrix(x)
    nc <- ncol(x)
    if(is.null(labs)) {
        labs. <- rep("", nc) # no names
    } else {
        labs. <- colnames(x)
        if(is.null(labs.)) { # ... then construct the labels
            labs. <- if(length(labs) == 1) {
                paste0(labs, seq_len(nc)) # construct labels
            } else labs # must be of the right length then
        }
    }

    ## Split 'x' and use correct labels
    x <- if(is.matrix(x)) {
        .Call(col_split, x)
    } else { # works for data.frame (= lists of columns; and (general) lists)
        unclass(x)
    }
    names(x) <- labs. # put labels in
    x # return
}

##' @title Auxiliary function for checking and converting the data argument of zenplot()
##' @param x A numeric vector, matrix, data.frame or a list of such
##' @param labs A list with components
##'        'group': the label names for group labels (or NULL for no group labels)
##'        'var': the variable labels (or NULL for no variable labels)
##'        'sep': the separator between group and variable labels
##'        If labs = NULL, neither group nor variable labels are used
##' @param sep The separator between the group and variable labels; ignored if
##'        the 'group' or 'var' label is NULL.
##' @return A list with components
##'         'xcols': a list containing the column vectors of x
##'         'groups': the group number for each column of x
##'         'vars': the variable number (within each group) for each column of x
##'         'glabs': the group label for each column of x
##'         'labs': the group and variable labels for each column of x
##' @author Marius Hofert
##' @note Performance critical
burst <- function(x, labs = list())
{
    ## Checks
    if(!is.standard(x))
        stop("'x' must be a vector, matrix, data.frame, or a list of such.")
    if(!is.null(labs)) {
        ## With this construction, the user gets the following defaults
        ## even when (s)he only specifies less than the three components
        nms <- names(labs)
        if(all(is.na(pmatch("group", table = nms, duplicates.ok = TRUE))))
            labs$group <- "G"
        if(all(is.na(pmatch("var", table = nms, duplicates.ok = TRUE))))
            labs$var <- "V"
        if(all(is.na(pmatch("sep", table = nms, duplicates.ok = TRUE))))
            labs$sep <- ", "
    }

    ## Distinguish the cases
    if(is.vector(x, mode = "list")) { # proper list (and not a data.frame)

        ngrps <- length(x) # number of groups
        if(ngrps == 0) stop("'x' has to have positive length.")

        ## Burst all groups
        x. <- x # dummy for calling burst_() on
        names(x.) <- NULL # remove names for burst as these group names show up in variable names otherwise
        col.lst <- lapply(x., burst_, labs = if(is.null(labs)) NULL else labs[["var"]])
        xcols <- unlist(col.lst, recursive = FALSE)
        gsizes <- vapply(col.lst, length, NA_real_)
        groups <- rep(1:ngrps, times = gsizes)
        vars <- unlist(lapply(gsizes, seq_len), use.names = FALSE)
        vlabs <- if(is.null(labs)) NULL else names(xcols) # unlist(lapply(col.lst, FUN = names), use.names = FALSE)

        ## Build group labels
        ## - If is.null(labs[["group"]]), omit group labels
        ## - If no group labels are given at all, construct them;
        ## - If some group labels are given, use (only) them and omit the others
        glabs <- if(is.null(labs)) {
            NULL
        } else if(is.null(labs[["group"]])) {
            r <- rep("", ngrps)
            r[groups] # expand
        } else {
            nms <- names(x) # use the names of 'x' as group labs, or, if NULL, build them
            r <- if(is.null(nms)) {
                labs.group <- labs[["group"]]
                if(length(labs.group) == 1) { # if of length 1, append number
                    paste0(labs.group, 1:ngrps)
                } else { # otherwise, use that
                    ## stopifnot(length(labs.group) == ngrps)
                    labs.group
                }
            } else nms
            r[groups] # expand
        }

        ## Build joint labels used for 'x' only (unless is.null(labs))
        if(!is.null(labs)) {
            is.null.labs.group <- is.null(labs[["group"]])
            is.null.labs.var <- is.null(labs[["var"]])
            labs <- if(is.null.labs.group && is.null.labs.var) {
                character(length(xcols)) # no labels
            } else { # if at least one is given (not both NULL)
                if(is.null.labs.group) { # only use var labels
                    vlabs
                } else if(is.null.labs.var) { # only use group labels
                    glabs
                } else { # use both labels
                    trimws(paste(glabs, vlabs, sep = labs[["sep"]]))
                }
            }
            names(xcols) <- labs # use these group and variable labels as labels of the columns
        }

    } else { ## if(is.vector(x) || is.matrix(x) || is.data.frame(x)) {

        xcols <- burst_(x, labs = if(is.null(labs)) NULL else labs[["var"]]) # columns with names
        l <- length(xcols)
        groups <- rep(1, l)
        vars <- seq_len(l)
        glabs <- NULL
        vlabs <- names(xcols)

    } ## else stop("Wrong 'x'.")

    ## Return result
    list(xcols = xcols, # list of columns (with 'full labels' = group and variable labels)
         groups = groups, # group numbers
         vars = vars, # variable numbers
         glabs = glabs, # group labels (NULL unless 'x' is a list)
         vlabs = vlabs) # variable labels
}

.burst_environ <- new.env(hash = FALSE, parent = emptyenv()) # define the environment to cache the burst x

##' @title A list of columns
##' @param x A list of columns
##' @return A list where each column is converted to data (range() works,
##'         can be plotted, etc.)
##' @author Marius Hofert
##' @note See plot.default -> xy.coords()
as_numeric <- function(x)
    lapply(x, function(x.) {
        if (is.language(x.)) {
            if (inherits(x., "formula") && length(x.) == 3) {
                x. <- eval(x.[[2L]], environment(x.))
            } else stop("Invalid first argument.")
        } else if (is.matrix(x.) || is.data.frame(x.)) {
            x. <- data.matrix(x.)[,1]
        } else {
            if (is.factor(x.)) x. <- as.numeric(x.)
        }
        if (inherits(x., "POSIXt")) x. <- as.POSIXct(x.)
        as.double(x.)
    })

##' @title Checking whether certain arguments appear in zargs
##' @param zargs The argument list as passed from zenplot()
##' @param ... The arguments to check zargs for
##' @return A logical indicating whether some arguments are missing in zargs
##' @author Marius Hofert
check_zargs <- function(zargs, ...)
{
    args <- list(...)
    miss <- args[which(!(args %in% names(zargs)))]
    missSome <- length(miss) > 0
    if(missSome)
        stop("Missing arguments ",paste(sQuote(miss), collapse = ", "),
             ". Consider providing your own functions.")
    missSome
}

##' @title Extracting information for our default/provided plot1d()
##' @param zargs The argument list as passed from zenplot(); this must at least
##'        contain 'x', 'orientations', 'vars', 'num', 'lim' and 'labs'.
##' @return A list with
##'         - the data x to plot in the 1d plot;
##'         - a list with all columns of x;
##'         - the group numbers (for each column of x);
##'         - the variable numbers (for each column of x);
##'         - the group labels (for each column of x);
##'         - the variable labels (for each column of x);
##'         - horizontal (logical);
##'         - the (x-)axis limits
##' @author Marius Hofert
##' @note Performance critical
extract_1d <- function(zargs)
{
    ## Checks
    check_zargs(zargs, "x", "orientations", "vars", "num", "lim", "labs")

    ## Extract quantities
    x <- zargs$x
    orientations <- zargs$orientations
    vars <- zargs$vars
    lim <- zargs$lim
    labs <- zargs$labs
    num <- zargs$num

    ## Burst x and cache result (fail-safe)
    if(!exists("burst.x", envir = .burst_environ) || num == 1) { # if 'burst.x' does not exist in .burst_environ or if the current call is the first for the current plot...
        xburst <- burst(x, labs = labs) # compute it (=> xcols (with glabs + vlabs), groups, vars, glabs, vlabs)
        assign("burst.x", xburst, envir = .burst_environ) # ... and cache it
    } else { # if it exists
        xburst <- get("burst.x", envir = .burst_environ) # ... get it
        if(num == nrow(vars)) rm("burst.x", envir = .burst_environ) # ... but remove it again after the last plot!
    }

    ## Pick out the plot variable index
    ix <- vars[num,1] # index of x

    ## Pick out the data
    xcols <- xburst$xcols
    xcols. <- as_numeric(xcols) # possibly transform all columns so that range() etc. works
    x. <- matrix(xcols.[[ix]]) # (possibly transformed) data x to be plotted
    ## => Conversion to matrix allows for column names
    colnames(x.) <- names(xcols[ix])

    ## Determine whether the plot is horizontal
    horizontal <- orientations[num] == "h"

    ## Determine xlim, ylim
    lim.method <- if(is.numeric(lim) && length(lim) == 2) "fixed" else lim
    switch(lim.method,
    "fixed" = {
        xlim <- lim
    },
    "individual" = {
        xlim <- if(all(is.na(x.))) 0:1 else range(x.[is.finite(x.)]) # adapted from plot.default()
    },
    "groupwise" = {
        if(is.list(x) && !is.data.frame(x)) { # multiple groups
            x.. <- unlist(xcols.[xburst$groups == xburst$groups[ix]]) # all x's belonging to the current group
            xlim <- if(all(is.na(x..))) 0:1 else range(x..[is.finite(x..)])
        } else { # no groups = only one group = global
            ax <- vars[,1] # all x indices
            axd <- unlist(xcols.[ax]) # all x data; need the unlisted version here for is.finite()
            xlim <- if(all(is.na(axd))) 0:1 else range(axd[is.finite(axd)])
        }
    },
    "global" = {
        ax <- vars[,1] # all x indices
        axd <- unlist(xcols.[ax]) # all x's; need the unlisted version here for is.finite()
        xlim <- if(all(is.na(axd))) 0:1 else range(axd[is.finite(axd)])
    },
    stop("Wrong 'lim.method'."))

    ## Return
    c(x = list(x.), # this way we keep the column labels
      xburst, # components returned by burst(): xcols (with glabs + vlabs), groups, vars, glabs, vlabs
      horizontal = list(horizontal),
      list(xlim = xlim))
}

##' @title Extracting information for our default/provided plot2d()
##' @param zargs The argument list as passed from zenplot(); this must at least
##'        contain 'x', 'vars', 'num', 'lim' and 'labs'.
##' @return A list with
##'         - the data x and y to plot in the 2d plot;
##'         - a list with all columns of x;
##'         - the group numbers (for each column of x);
##'         - the variable numbers (for each column of x);
##'         - the group labels (for each column of x);
##'         - the variable labels (for each column of x);
##'         - the x- and y-axis limits;
##'         - a logical indicating whether x and y are in the same group
##' @author Marius Hofert
##' @note Performance critical
extract_2d <- function(zargs)
{
    ## Checks
    check_zargs(zargs, "x", "vars", "num", "lim", "labs")

    ## Extract quantities (all but num; see below)
    x <- zargs$x
    vars <- zargs$vars
    lim <- zargs$lim
    labs <- zargs$labs
    num <- zargs$num

    ## Burst x and cache result (fail-safe)
    if(!exists("burst.x", envir = .burst_environ) || num == 1) { # if 'burst.x' does not exist in .burst_environ or if the current call is the first for the current plot...
        xburst <- burst(x, labs = labs) # compute it (=> xcols (with glabs + vlabs), groups, vars, glabs, vlabs)
        assign("burst.x", xburst, envir = .burst_environ) # ... and cache it
    } else { # if it exists
        xburst <- get("burst.x", envir = .burst_environ) # ... get it
        if(num == nrow(vars)) rm("burst.x", envir = .burst_environ) # ... but remove it again after the last plot!
    }

    ## Pick out the plot variable indices
    ix <- vars[num,1] # index of x
    iy <- vars[num,2] # index of y

    ## Pick out the data
    xcols <- xburst$xcols
    xcols. <- as_numeric(xcols) # possibly transform all columns so that range() etc. works
    x. <- matrix(xcols.[[ix]]) # (possibly transformed) data x to be plotted
    y. <- matrix(xcols.[[iy]]) # (possibly transformed) data y to be plotted
    ## => Conversion to matrix allows for column names
    colnames(x.) <- names(xcols[ix])
    colnames(y.) <- names(xcols[iy])

    ## Determine whether they are in the same group and compute xlim, ylim
    same.group <- xburst$groups[ix] == xburst$groups[iy]
    lim.method <- if(is.numeric(lim) && length(lim) == 2) "fixed" else lim
    switch(lim.method,
    "fixed" = {
        xlim <- lim
        ylim <- lim
    },
    "individual" = {
        xlim <- if(all(is.na(x.))) 0:1 else range(x.[is.finite(x.)]) # adapted from plot.default()
        ylim <- if(all(is.na(y.))) 0:1 else range(y.[is.finite(y.)])
    },
    "groupwise" = {
        if(is.list(x) && !is.data.frame(x)) { # multiple groups
            x.. <- unlist(xcols.[xburst$groups == xburst$groups[ix]]) # all x's belonging to the current group
            xlim <- if(all(is.na(x..))) 0:1 else range(x..[is.finite(x..)])
            y.. <- unlist(xcols.[xburst$groups == xburst$groups[iy]])
            ylim <- if(all(is.na(y..))) 0:1 else range(y..[is.finite(y..)])
        } else { # no groups = only one group = global
            ax <- vars[,1] # all x indices
            ay <- vars[,2] # all y indices
            axd <- unlist(xcols.[ax]) # all x data; need the unlisted version here for is.finite()
            xlim <- if(all(is.na(axd))) 0:1 else range(axd[is.finite(axd)])
            ayd <- unlist(xcols.[ay]) # all y data; need the unlisted version here for is.finite()
            ylim <- if(all(is.na(ayd))) 0:1 else range(ayd[is.finite(ayd)])
        }
    },
    "global" = {
        ax <- vars[,1] # all x indices
        ay <- vars[,2] # all y indices
        axd <- unlist(xcols.[ax]) # all x's; need the unlisted version here for is.finite()
        xlim <- if(all(is.na(axd))) 0:1 else range(axd[is.finite(axd)])
        ayd <- unlist(xcols.[ay]) # all y's; need the unlisted version here for is.finite()
        ylim <- if(all(is.na(ayd))) 0:1 else range(ayd[is.finite(ayd)])
    },
    stop("Wrong 'lim.method'."))

    ## Return
    c(x = list(x.), y = list(y.), # (numeric!) data (possibly converted); list() to keep the column labels
      xburst, # components returned by burst(): xcols (original entries; with glabs + vlabs), groups, vars, glabs, vlabs
      list(xlim = xlim, ylim = ylim, same.group = same.group))
}
