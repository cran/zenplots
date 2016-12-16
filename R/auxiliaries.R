## Auxiliary tools


### Graphical tools ############################################################

##' @title Creating a Gray Color with Alpha Blending
##' @param n a number determining the alpha levels if alpha is NULL
##'        (= number of distinguishable layers on each pixel); n/10 = number of
##'        points needed to saturate
##' @param h see ?hcl
##' @param c see ?hcl
##' @param l see ?hcl
##' @param alpha see ?hcl; if NULL, then alpha is determined from 'n'
##' @param fixup see ?hcl
##' @return hcl alpha blended gray color
##' @author Marius Hofert and Wayne Oldford
gray_alpha_blend <- function(n, h = 260, c = 0, l = 65, alpha = NULL, fixup = TRUE)
    hcl(h, c = c, l = l, alpha = if(is.null(alpha)) 1/(n/10 + 1) else alpha, fixup = fixup)

##' @title Defining an arrow
##' @param turn The direction in which the arrow shall point ("l", "r", "d", "u")
##' @param length The length of the arrow in [0,1] from tip to base
##' @param angle The angle
##' @return A 3-column matrix containing the (x,y) coordinates of the left
##'         edge end point, the arrow head and the right edge end point
##' @author Marius Hofert
zenarrow <- function(turn, angle = 80, length = 1, coord.scale = 1)
{
    stopifnot(0 <= angle, angle <= 180)
    th <- angle * pi / 180 # convert from angle to radians
    ## Determine head and two edges (center = (0,0)) of an arrow pointing 'right'
    ## Arrow head
    head <- c(length * 0.5, 0) # arrow head
    ## Left edge (in direction of the arrow)
    th2 <- th/2 # half the angle
    left <- c(length * (-0.5), coord.scale * length * tan(th2)) # end point of left edge of the arrow head
    ## => first component ('width') is 1 * length; the unit of the second
    ##    component is the same as the first (Cartesian coordinate system)
    ## Right edge (in direction of the arrow)
    right <- c(left[1], -left[2]) # end point of right edge of the arrow head
    ## Now turn the base arrow appropriately
    rot <- switch(turn,
                  "l" = {   pi },
                  "r" = {    0 },
                  "d" = { 3*pi/2 },
                  "u" = {   pi/2 },
                  stop("Wrong 'turn'"))
    rot.mat <- matrix(c(cos(rot), -sin(rot), sin(rot), cos(rot)),
                      nrow = 2, ncol = 2, byrow = TRUE)
    left <- rot.mat %*% left
    right <- rot.mat %*% right
    head <- rot.mat %*% head
    ## Return
    cbind(left = left, head = head, right = right) # (2, 3)-matrix
}


### Technical tools ############################################################

##' @title Converting an Occupancy Matrix (consisting of 0--4) to
##'        a Human Readable Matrix
##' @param x an occupancy matrix
##' @param to symbols being mapped to by the occupancy matrix
##' @return matrix of encoded entries of the occupancy matrix
##' @author Marius Hofert
occupancy_to_human <- function(x, to = c("", "<", ">", "v", "^"))
{
    stopifnot(0 <= x, x <= 4, length(to) == 5)
    if(is.matrix(x)) {
        dm <- dim(x)
        matrix(to[x+1], nrow=dm[1], ncol=dm[2])
    } else {
        to[x+1]
    }
}

##' @title Auxiliary Function for Constructing Default n2dcols
##' @param n2dplots The number of variates (= nfaces)
##' @param method One of "letter", "square", "A4", "golden", "legal"
##' @return An odd integer for n2dcols
##' @author Wayne Oldford
n2dcols_aux <- function(n2dplots, method = c("letter", "square", "A4", "golden", "legal"))
{
    method <- match.arg(method)
    scaling <- switch(method,
                      "golden" = (1 + sqrt(5))/2,
                      "square" = 1,
                      "letter" = 11/8.5,
                      "legal" = 14/8.5,
                      "A4" = sqrt(2),
                      stop("Wrong 'method'"))
    n2dcols <- max(3,
                  ## n2dcol is never less than 3
                  ## nrows should be scaling * ncols
                  ## n2dplots is actually about (ncols - 1) * nrows
                  ## Solve for ncols, want an integer value
                  ## ncols = 0.5 * (1 + sqrt( 1 + 4 *n2dplots / scaling))
                  round(0.5 * (1 + sqrt( 1 + 4 * (n2dplots - 1) / scaling))))
    if ((n2dcols %% 2) == 0) n2dcols <- n2dcols + 1 # the default should be odd
    n2dcols
}

##' @title Check the Turns (Number/Type)
##' @param turns The turns
##' @param n2dplots The number of 2d plots
##' @param first1d A logical indicating whether the first 1d plot should be plotted
##' @param last1d A logical indicating whether the last 1d plot should be plotted
##' @return TRUE (unless it fails)
##' @author Marius Hofert
turn_checker <- function(turns, n2dplots, first1d, last1d)
{
    ## Check the type of the turns
    if(!is.character(turns) || !all(turns %in% c("d", "u", "r", "l")))
        stop("'turns' not all in 'd', 'u', 'r' or 'l'")

    ## Check the length of the turns
    nturns <- length(turns)
    l <- as.numeric(!first1d) + as.numeric(!last1d) # 0 (first1d = last1d = TRUE), 1 (precisely one FALSE) or 2 (first1d = last1d = FALSE)
    if(nturns != 2 * n2dplots - l + 1) {
        if(l == 0) { # just for a nicer error message
            stop("Number of turns is ",nturns," but must be 2 * n2dplots + 1 = ",2*n2dplots+1)
        } else {
            stop("Number of turns is ",nturns," but must be 2 * <number of variables> - ",l," + 1 = ",2*n2dplots-l+1)
        }
    }
    TRUE
}

##' @title Check Argument for Being a Vector, Matrix, Data Frame or a List of such
##' @param x A vector, matrix, data.frame or list of such
##' @return A logical indicating whether x is of the above type
##' @author Marius Hofert
is.standard <- function(x) {
    if(!is.vector(x, mode = "list")) { # has to be a vector, matrix or data.frame
        is.vector(x) || is.matrix(x) || is.data.frame(x)
    } else { # recursion
        all(vapply(x, is.standard, NA))
    }
}

##' @title Determine the number of columns if is.standard(x)
##' @param x A numeric vector, matrix, data.frame or a list of such.
##' @return The number of data columns of 'x'
##' @author Marius Hofert
num_cols <- function(x)
{
    if(!is.standard(x))
        stop("'x' must be a vector, matrix, data.frame, or a list of such.")
    if(is.vector(x, mode = "list")) {
        sum(sapply(x, num_cols))
    } else { # 'x' must be a vector, matrix or data.frame
        if(is.vector(x) || is.data.frame(x)) x <- as.matrix(x)
        ncol(x)
    }
}

##' @title Scale Data to Component-Wise Be in [0,1]
##' @param x A matrix or data.frame or a list of vectors
##' @param method character string indicating the method to be used
##'        scale all vectors of x with the same linear
##'        transformation or scale each vector of x itself
##' @param ... additional arguments passed to range() or rank()
##' @return x scaled to [0,1]
##' @author Marius Hofert
##' @note Pass through NA columns
scale01 <- function(x, method = c("columnwise", "all", "pobs"), ...)
{
    ## Convert inputs to a list of (column) vectors
    is.mat <- is.matrix(x)
    is.df <- is.data.frame(x)
    if(is.mat || is.df)
        x <- as.list(as.data.frame(x))
    len <- length(x)
    stopifnot(is.list(x), len >= 1)
    ## Note: When called from zenplot(), 'x' is already a list (but could
    ##       potentially contain factors. We deal with them as pairs() does.
    for(i in seq_len(len)) {
        if(is.factor(x[[i]]) || is.logical(x[[i]]))
            x[[i]] <- as.numeric(x[[i]])
        if(!is.numeric(unclass(x[[i]])))
            stop("Non-numeric argument to scale01()")
    }

    ## Scale
    method <- match.arg(method)
    res <- switch(method,
    "columnwise" = {
        lapply(x, function(x.) {
            if(all(is.na(x.))) return(x.) # no scaling done
            ran <- range(x., na.rm=TRUE, ...)
            dff <- diff(ran)
            if(dff==0)
                stop("Cannot scale non-NA data 'x': Division by 0 (as diff(range()) == 0)") # all non-NA data points are the same
            (x.-ran[1])/dff
        })
    },
    "all" = {
        x.vals <- unlist(x)
        if(all(is.na(x.vals))) return(x) # no scaling done
        ran <- range(x.vals, na.rm=TRUE, ...)
        dff <- diff(ran)
        if(dff==0)
            stop("Cannot scale non-NA data 'x': Division by 0 (as diff(range()) == 0)") # all non-NA data points are the same
        lapply(x, function(x.) (x.-ran[1])/diff(ran))
    },
    "pobs" = {
        lapply(x, function(x.) rank(x., na.last="keep", ...)/(length(x.)+1))
    },
    stop("Wrong 'method'"))

    ## Return
    if(is.mat) {
        matrix(unlist(res), ncol=length(res))
    } else if(is.df) {
        as.data.frame(res)
    } else res
}
