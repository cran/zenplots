## Default 2d plot functions based on grid


##' @title Plot of labels indicating adjacent groups
##' @param zargs argument list as passed from zenplot()
##' @param glabs group labels being indexed by the plot variables;
##'        if NULL then they are determined with extract_2d()
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert
##' @note For performance reasons (avoiding having to call extract_2d() twice),
##'       'glabs' is an extra argument
group_2d_grid <- function(zargs,
                          glabs, loc = c(0.5, 0.5),
                          draw = FALSE, ...)
{
    check_zargs(zargs, "turns", "vars", "num", "ispace")
    turns <- zargs$turns
    vars <- zargs$vars
    num <- zargs$num
    ii <- range(vars[num,]) # variable index
    ii <- if(turns[num-1] == "u" || turns[num] == "u") rev(ii) else ii
    labs <- paste0(glabs[ii], collapse = "\n") # labels (in the correct order for displaying the group change)
    ## Plotting
    vp <- vport(zargs$ispace)
    res <- textGrob(label = labs, x = loc[1], y = loc[2],
                    default.units = "npc",
                    name = "group_2d", gp = gpar(...), vp = vp)
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Point plot in 2d
##' @param zargs argument list as passed from zenplot()
##' @param type line type
##' @param pch plot symbol
##' @param size size of the plot symbol
##' @param box logical indicating whether a box should be drawn
##' @param box.width width of the box
##' @param box.height height of the box
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note - We use names depending on the 'type' here since otherwise, if one calls it
##'         once for 'p' and once for 'l', only one of them is plotted
##'       - The default point size was chosen to match the default of graphics
points_2d_grid <- function(zargs,
                           type = c("p", "l", "o"), pch = NULL, size = 0.02,
                           box = FALSE, box.width = 1, box.height = 1,
                           group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- r$x
    y <- r$y
    same.group <- r$same.group
    glabs <- r$glabsx
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim)
        if(box)
            gBox <- rectGrob(x = 0.5, y = 0.5,
                             width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                             just = "centre", default.units = "npc",
                             name = "box_2d", gp = gpar(...), vp = vp)
        type <- match.arg(type)
        switch(type,
        "p" = {
            if(is.null(pch)) pch <- 21
            gPoints <- pointsGrob(x = x, y = y, pch = pch,
                                  size = unit(size, units = "npc"),
                                  default.units = "native",
                                  name = "points_2d", gp = gpar(...), vp = vp)
            if(box) { # create a single grob
                gTree(children = gList(gBox, gPoints)) # note: first box
            } else {
                gTree(children = gList(gPoints))
            }
        },
        "l" = {
            gLines <- linesGrob(x = x, y = y,
                                default.units = "native",
                                name = "lines_2d", gp = gpar(...), vp = vp)
            if(box) { # create a single grob
                gTree(children = gList(gBox, gLines)) # note: first box
            } else {
                gTree(children = gList(gLines))
            }
        },
        "o" = {
            if(is.null(pch)) pch <- 20
            gLines <- linesGrob(x = x, y = y,
                                default.units = "native",
                                name = "lines_2d", gp = gpar(...), vp = vp)
            gPoints <- pointsGrob(x = x, y = y, pch = pch,
                                  size = unit(size, units = "npc"),
                                  default.units = "native",
                                  name = "points_2d", gp = gpar(...), vp = vp)
            if(box) { # create a single grob
                gTree(children = gList(gBox, gLines, gPoints)) # note: first box
            } else {
                gTree(children = gList(gLines, gPoints))
            }
        },
        stop("Wrong 'type'"))
    } else {
        args <- c(list(zargs = zargs, glabs = glabs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Density plot in 2d
##' @param zargs argument list as passed from zenplot()
##' @param ngrids number of grid points in each direction. Can be scalar or
##'        a length-2 integer vector.
##' @param ccol vector (which is then recycled to the appropriate length)
##'        giving the color of the contours
##' @param clwd vector (which is then recycled to the appropriate length)
##'        giving the line widths of the contours
##' @param clty vector (which is then recycled to the appropriate length)
##'        giving the line types of the contours
##' @param box logical indicating whether a box should be drawn
##' @param box.width width of the box
##' @param box.height height of the box
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note - We use names depending on the 'type' here since otherwise, if one calls it
##'         once for 'p' and once for 'l', only one of them is plotted
##'       - The default point size was chosen to match the default of graphics
##' @author Marius Hofert and Wayne Oldford
density_2d_grid <- function(zargs,
                            ngrids = 25, ccol = NULL, clwd = 1, clty = 1,
                            box = FALSE, box.width = 1, box.height = 1,
                            group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- r$x
    y <- r$y
    same.group <- r$same.group
    glabs <- r$glabsx
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        data <- na.omit(data.frame(x, y))
        colnames(data) <- c("x", "y")
        dens <- kde2d(data$x, data$y, n = ngrids, lims = c(xlim, ylim))
        contours <- contourLines(dens$x, dens$y, dens$z)
        levels <- sapply(contours, function(contour) contour$level) # list of contour levels
        nLevels <- length(levels) # number of levels
        uniqueLevels <- unique(levels) # unique levels (there could be more than one level curve with the same level)
        nuLevels <- length(uniqueLevels)
        if(is.null(ccol)) { # default grey scale colors
            basecol <- c("grey80", "grey0")
            palette <- colorRampPalette(basecol, space = "Lab")
            ccol <- palette(nuLevels) # different color for each 1d plot
        }
        ccol <- rep_len(ccol, nuLevels)
        clwd <- rep_len(clwd, nuLevels)
        clty <- rep_len(clty, nuLevels)
        ## Match the levels in the unique levels
        ccol. <- numeric(nLevels)
        clwd. <- numeric(nLevels)
        clty. <- numeric(nLevels)
        for (i in 1:nuLevels) {
            idx <- (1:nLevels)[levels == uniqueLevels[i]]
            ccol.[idx] <- ccol[i]
            clwd.[idx] <- clwd[i]
            clty.[idx] <- clty[i]
        }
        ## Plotting
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim, x = x, y = y)
        if(box)
            gBox <- rectGrob(x = 0.5, y = 0.5,
                             width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                             just = "centre", default.units = "npc",
                             name = "box_2d", gp = gpar(...), vp = vp)
        contourGrobs <- lapply(1:length(contours), # go over all contours
                               function(i) {
            contour <- contours[[i]]
            linesGrob(x = contour$x, y = contour$y,
                      gp = gpar(col = ccol.[i],
                                lwd = clwd.[i], lty = clty.[i], ...),
                      default.units = "native",
                      name = paste0("contour_",i), # note: have to be different!
                      vp = vp)
        })
        if(box) { # create a single grob
            gTree(children = do.call(gList, args = c(contourGrobs, list(gBox))))
        } else {
            gTree(children = do.call(gList, args = contourGrobs))
        }
    } else {
        args <- c(list(zargs = zargs, glabs = glabs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Axes arrows in 2d
##' @param zargs argument list as passed from zenplot()
##' @param angle angle of the arrow head (see ?arrow)
##' @param length length of the arrow in [0,1] from tip to base
##' @param type type of the arrow head (see ?arrow)
##' @param eps distance by which the axes are moved away from the plot region
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note Inspired by https://stat.ethz.ch/pipermail/r-help/2004-October/059525.html
axes_2d_grid <- function(zargs,
                         angle = 30, length = unit(0.05, "npc"), type = "open", eps = 0.02,
                         group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- r$x
    y <- r$y
    same.group <- r$same.group
    glabs <- r$glabsx
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim, x = x, y = y)
        x.grob <- linesGrob(x = unit(c(-eps, 1+eps), "npc"),
                            y = unit(c(-eps,  -eps), "npc"),
                            arrow = arrow(angle = angle, length = length,
                                          ends = "last", type = type),
                            name = "x_axis_2d",
                            gp = gpar(...), vp = vp) # x axis
        y.grob <- linesGrob(x = unit(c(-eps,  -eps), "npc"),
                            y = unit(c(-eps, 1+eps), "npc"),
                            arrow = arrow(angle = angle, length = length,
                                          ends = "last", type = type),
                            name = "y_axis_2d",
                            gp = gpar(...), vp = vp) # y axis
        gTree(children = gList(x.grob, y.grob)) # create a single grob
    } else {
        args <- c(list(zargs = zargs, glabs = glabs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Arrow plot in 2d
##' @param zargs argument list as passed from zenplot()
##' @param loc (x,y)-location of the center of the arrow
##' @param angle angle from the shaft to the edge of the arrow head
##' @param length length of the arrow in [0,1] from tip to base
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
arrow_2d_grid <- function(zargs,
                          loc = c(0.5, 0.5), angle = 60, length = 0.2,
                          group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    glabs <- r$glabs
    check_zargs(zargs, "num", "turns", "ispace")
    turn.out <- zargs$turns[zargs$num]
    res <- if(same.group) {
        vp <- vport(zargs$ispace)
        arrow <- zenarrow(turn.out, angle = angle, length = length,
                          coord.scale = 1)
        arr <- loc + arrow
        ## Plotting
        linesGrob(x = arr[1,], y = arr[2,], default.units = "npc",
                  name = "arrow_2d", gp = gpar(...), vp = vp)
    } else {
        args <- c(list(zargs = zargs, glabs = glabs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Rectangle plot in 2d
##' @param zargs argument list as passed from zenplot()
##' @param loc (x,y)-location of the rectangle
##' @param width rectangle width as a fraction of 1
##' @param height rectangle height as a fraction of 1
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
rect_2d_grid <- function(zargs,
                         loc = c(0.5, 0.5), width = 1, height = 1,
                         group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    glabs <- r$glabs
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        vp <- vport(zargs$ispace)
        rectGrob(x = loc[1], y = loc[2], width = width, height = height,
                 default.units = "npc", name = "rect_2d",
                 gp = gpar(...), vp = vp)
    } else {
        args <- c(list(zargs = zargs, glabs = glabs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Label plot in 2d
##' @param zargs argument list as passed from zenplot()
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param label label to be used
##' @param cex character expansion factor
##' @param just (x,y)-justification of the label
##' @param rot rotation of the label
##' @param box logical indicating whether a box should be drawn
##' @param box.width width of the box
##' @param box.height height of the box
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
label_2d_grid <- function(zargs,
                          loc = c(0.98, 0.05), label = NULL, cex = 0.66,
                          just = c("right", "bottom"), rot = 0,
                          box = FALSE, box.width = 1, box.height = 1,
                          group... = list(cex = cex), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    vlabs <- r$vlabs
    glabs <- r$glabs
    check_zargs(zargs, "vars", "num", "ispace")
    vars <- zargs$vars
    num <- zargs$num
    res <- if(same.group) {
        xlab <- vlabs[vars[num, 1]]
        ylab <- vlabs[vars[num, 2]]
        if(is.null(label)) label <- paste0("(",xlab,", ",ylab,")")
        vp <- vport(zargs$ispace)
        gText <- textGrob(label = label,
                          x = loc[1], y = loc[2], just = just, rot = rot,
                          default.units = "npc",
                          name = "label_2d", gp = gpar(cex = cex, ...), vp = vp)
        if(box) {
            gBox <- rectGrob(x = 0.5, y = 0.5,
                             width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                             default.units = "npc",
                             name = "box_2d", gp = gpar(...), vp = vp)
            gTree(children = gList(gBox, gText)) # note: first box
        } else {
            gTree(children = gList(gText))
        }
    } else {
        args <- c(list(zargs = zargs, glabs = glabs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Layout plot in 2d
##' @param zargs argument list as passed from zenplot()
##' @param ... additional arguments passed to label_2d_grid()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note Here we also pass '...' to group_2d_grid() (to easily adjust
##'       font size etc.)
layout_2d_grid <- function(zargs, ...)
    label_2d_grid(zargs, loc = c(0.5, 0.5),
                  just = "centre", box = TRUE, group... = list(...),
                  ...)

