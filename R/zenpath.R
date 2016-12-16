## Tools for computing a path through all variables which can then be plotted
## with a zen plot


##' @title Find the Pairs with Smallest (or Largest) Entry in the (Lower)
##'        Triangular Area of a Symmetric Matrix
##' @param x A symmetric matrix (of distances)
##' @param n Number of extreme values to be returned
##' @param method A character string indicating the method to be used
##' @param use.names A logical indicating whether colnames(x) are to be
##'        used (if not NULL)
##' @return A (n, 3)-matrix with the n largest/smallest/both
##'         values in the symmetric matrix x (3rd column) and the
##'         corresponding indices (1st and 2nd column)
##' @author Marius Hofert and Wayne Oldford
extreme_pairs <- function(x, n = 6, method = c("largest", "smallest", "both"),
                          use.names = FALSE)
{
    ## Checks
    if(!is.matrix(x)) x <- as.matrix(x)
    d <- ncol(x)
    method <- match.arg(method)
    stopifnot(n >= 1, d >= 2, nrow(x) == d, is.logical(use.names))

    ## Build (row, col)-matrix
    ind <- as.matrix(expand.grid(1:d, 1:d)[,2:1])
    ind <- ind[ind[,1]<ind[,2],] # pick out indices as they appear in the upper triangular matrix
    colnms <- colnames(x)
    if(use.names && !is.null(colnms))
        ind <- matrix(colnms[as.matrix(ind)], ncol = 2)

    ## Merge with entries to a (row, col, value)-data frame
    ## Note that, since x is symmetric, values of the *lower* triangular
    ## matrix as a vector matches indices in 'ind'
    val <- data.frame(ind, x[lower.tri(x)], stringsAsFactors = FALSE)

    ## Sort the data.frame according to the values and adjust the row/column names
    res <- val[order(val[,3], decreasing = TRUE),]
    colnames(res) <- c("row", "col", "value")
    rownames(res) <- NULL

    ## Now grab out the 'extreme' pairs and values
    pairs <- 1:nrow(ind) # d*(d-1)/2 pairs
    switch(method,
    "largest" = {
        stopifnot(n <= nrow(ind))
        res[head(pairs, n = n),] # from large to small
    },
    "smallest" = {
        stopifnot(n <= nrow(ind))
        res[rev(tail(pairs, n = n)),] # from small to large
    },
    "both" = {
        stopifnot(n <= floor(nrow(ind)/2))
        res[c(head(pairs, n = n), tail(pairs, n = n)),] # from large to small
    },
    stop("Wrong 'method'"))
}

##' @title Compute a Graph Showing the Pairs with Largest (or Smallest)
##'        n Values in a Symmetric Matrix
##' @param x A symmetric matrix
##' @param n Number of extreme values to be returned
##' @param method A character string indicating the method to be used
##' @param use.names A logical indicating whether colnames(x) are to be
##'        used (if not NULL)
##' @return graphNEL object (can be plotted with Rgraphviz)
##' @author Marius Hofert and Wayne Oldford
extreme_pairs_graph <- function(x, n = 6, method = c("largest", "smallest", "both"),
                                use.names = FALSE)
{
    ## Call extreme_pairs_graph()
    xtr.pairs <- as.matrix(extreme_pairs(x, n = n, method = method, use.names = FALSE))
    colnames(xtr.pairs) <- NULL

    ## Build vertex names
    ft <- xtr.pairs[,1:2] # from/to
    v.ind <- sort(unique(as.vector(ft))) # indices of vertices
    colnms <- colnames(x)
    v.nms <- if(use.names && !is.null(colnms)) { # determine vertex names
        ft[,1:2] <- colnms[ft[,1:2]] # put the variable names in ft
        colnms[v.ind]
    } else as.character(v.ind)

    ## Build graph
    ftM2graphNEL(ft, W = xtr.pairs[,3], V = v.nms, edgemode = "undirected") # possibly uneven, disconnected
}

##' @title Extract Pairs from a Path of Indices and Return the Shortened Path
##' @param x The path as a vector or list of indices of the variables to be plotted
##' @param n A vector of length 2 giving the number of pairs to extract from
##'        the path x. The first number corresponds to the beginning of the path,
##'        the second to the end. At least one should be > 0.
##'        If NULL, all pairs are returned, nothing extracted; if n is of
##'        length 1, this corresponds to rep(extract, 2).
##' @return Object of the same type as 'x' but shortened (unless n = NULL)
##' @author Marius Hofert
extract_pairs <- function(x, n)
{
    if(is.null(n))
        return(x) # nothing extracted
    if(length(n) == 1) n <- rep(n, 2)
    if((length(n) != 2) || any(n < 0))
        stop("'n' must be NULL or one or two integers >= 0.")
    if(sum(n) <= 0)
        stop("At least one component of 'n' should be >= 1.")
    stopifnot(is.list(x) || (is.numeric(x) && is.vector(x)))
    if(is.list(x)) { # x is a list of indices

        ## Auxiliary function
        truncation_point <- function(x, num, first = TRUE) {
            l <- 0
            i.ind <- 1:length(x)
            if(!first) i.ind <- rev(i.ind)
            for(i in i.ind) { # iterates over x top-down (if first = TRUE) or bottom-up (if first = FALSE)
                j.ind <- seq_len(length(x[[i]])-1) # iterates over the pairs in x[[i]]
                if(!first) j.ind <- rev(j.ind)
                for(j in j.ind) { # iterates over the pairs in the vector x[[i]]
                    l <- l + 1
                    if(l >= num)
                        return(c(i, j)) # i = sublist; j = first index where last pair of interest begins
                }
            }
        }

        ## Determine the first part
        if(n[1] == 0) x.first <- NULL else {
            x.first <- x
            ij <- truncation_point(x, num = n[1])
            i <- ij[1]
            j <- ij[2]
            x.first[[i]] <- head(x.first[[i]], n = j+1) # grab out the first part of the line
            x.first <- head(x.first, n = i) # truncate the rest of the list
        }

        ## Determine the last part
        if(n[2] == 0) x.last <- NULL else {
            x.last <- x
            ij <- truncation_point(x, num = n[2], first = FALSE)
            i <- ij[1]
            j <- ij[2]
            x.last[[i]] <- tail(x.last[[i]], n = length(x.last[[i]])-j+1) # grab out the last part of the line
            x.last <- tail(x.last, n = length(x)-i+1) # truncate the first part of the list
        }

        ## Return
        res <- c(x.first, x.last)
        if(is.list(res) && length(res) == 1) unlist(res) else res

    } else { # x is a vector of indices

        c(head(x, n = n[1] + 1), tail(x, n = n[2] + 1))

    }
}

##' @title Connecting a List of Pairs if They Overlap
##' @param x A two-column matrix or a list of pairs (vectors of lengths 2)
##' @param duplicate.rm A logical indicating whether equal pairs (up to
##'        permutation) are omitted.
##' @return A list containing vectors of length >= 2 (if > 2, those are connected pairs)
##' @author Marius Hofert
connect_pairs <- function(x, duplicate.rm = FALSE)
{
    if(is.list(x)) {
        stopifnot(all(sapply(x, function(x.) length(x.) == 2)))
        x <- matrix(unlist(x), ncol = 2, byrow = TRUE)
    }
    stopifnot(is.matrix(x), nrow(x) >= 1, is.logical(duplicate.rm))
    if(duplicate.rm) {
        swapped <- rep(FALSE, nrow(x))
        for(i in seq_len(nrow(x))) {
            if(x[i,1] > x[i,2]) {
                x[i,] <- rev(x[i,])
                swapped[i] <- TRUE
            }
        }
        dupl <- duplicated(x)
        x <- x[!dupl,] # only grab out the unique pairs
        swapped <- swapped[!dupl]
        for(i in seq_len(nrow(x))) {
            if(swapped[i]) x[i,] <- rev(x[i,]) # swap back
        }
    }
    if(!is.matrix(x)) x <- rbind(x)
    nr <- nrow(x)
    res <- vector("list", length = nr) # result list of variables/indices
    l <- 1 # index where to add next element in res
    vec <- integer(2*nr) # most of it is 0, but c() to an empty vector is about 50x slower
    vec[1:2] <- x[1,] # start with first two connected variables
    v <- 2 # index of the last element in vec
    for(i in 2:nr) { # go over all variable pairs
        ## Deal with first 2 variables of a new group (we can still rev() these variables)
        if(v == 2) {
            matches <- x[i-1,] %in% x[i,] # which of the two entries in the previous row is in the current row
            sm <- sum(matches) # number of variables in the current row also present in the previous row
            if(sm == 2 && duplicate.rm)
                ## Note: - This case should actually not happen due to the removal of the duplicates above
                ##       - Without the duplicate.rm part above, this would only remove *adjacent* duplicates
                ## warning("Found two equal pairs (up to permutation) in rows ",i-1," and ",i,".")
                next
            if(sm >= 1)
                if(matches[1]) vec[1:2] <- vec[2:1] # if the first one matches, flip the elements
        }
        ## Now check whether the last variable in vec is found in the current row of pairs
        ## If so, add the *other* variable from the current row of pairs; otherwise start a new group
        is.valid.match <- vec[v] == x[i,]
        stopifnot(sum(is.valid.match) <= 1) # fail-safe programming
        if(any(is.valid.match)) {
            j <- which(!is.valid.match) # index of the *other* variable (the new one to add)
            vec[v+1] <- x[i,j]
            v <- v+1 # update index of last element in vec
        } else { # start a new group
            res[l] <- list(vec[1:v]) # add old vector
            l <- l+1 # update index where to add next element in res
            vec[1:2] <- x[i,] # define the new vec (entry is reversed above if necessary)
            v <- 2 # update index of last element in vec
        }
        if(i == nr) res[l] <- list(vec[1:v]) # add last built vector
    }
    res[1:l]
}

##' @title Computing Indices to Sort Data for a Zenplot
##' @param x An input object giving the weights (depending on the method)
##'        according to which the pairs of variables are sorted. For
##'        "front.loaded": A single integer >= 1
##'        "back.loaded":  A single integer >= 1
##'        "balanced":     A single integer >= 1
##'        "eulerian.cross": Two integers >= 1.
##'        "eulerian.weighted": A vector (or matrix or distance matrix)
##'        "strictly.weighted": A vector (or matrix or distance matrix)
##' @param pairs An (n, 2)-matrix (some n) containing the pairs of variables
##'        to be sorted according to the weights
##' @param method A sorting method; one of
##'        "front.loaded": Sort all pairs such that the first variables appear
##'                        the most frequently early in the sequence;
##'                        an Eulerian path
##'        "back.loaded": Sort all pairs such that the later variables appear
##'                       the most frequently later in the sequence;
##'                       an Eulerian path
##'        "balanced": Sort all pairs such that all variables appear in
##'                    balanced blocks throughout the sequence
##'                    (a Hamiltonian Decomposition; Eulerian, too).
##'        "eulerian.cross": Generate a sequence of pairs such that
##'                          each is formed with one variable from each group.
##'        "eulerian.weighted": Sort all pairs according to a greedy (heuristic)
##'                             Euler path with 'x' as weights visiting each
##'                             edge precisely once.
##'        "strictly.weighted": Strictly respect the order of the weights.
##' @param decreasing A logical indicating whether the sorting is done according
##'        to increasing or decreasing weights
##' @return A vector or list of indices according to which data can be
##'         indexed via group() for plotting with zenplot()
##' @author Marius Hofert
zenpath <- function(x, pairs = NULL,
                    method = c("front.loaded", "back.loaded", "balanced",
                               "eulerian.cross", "eulerian.weighted",
                               "strictly.weighted"),
                    decreasing = TRUE)
{
    method <- match.arg(method)
    switch(method,
    "front.loaded" = {

        stopifnot(is.numeric(x))
        if(x %% 1 != 0 || length(x) != 1 || x < 1)
            stop("'x' has to be an integer >= 1 for method = \"front.loaded\".")
        if(x > 1) rev((x:1)[eseq(x)]) else 1

    },
    "back.loaded" = {

        stopifnot(is.numeric(x))
        if(x %% 1 != 0 || length(x) != 1 || x < 1)
            stop("'x' has to be an integer >= 1 for method = \"back.loaded\".")
        if(x > 1) eseq(x) else 1

    },
    "balanced" = {

        stopifnot(is.numeric(x))
        if(x %% 1 != 0 || length(x) != 1 || x < 1)
            stop("'x' has to be an integer >= 1 for method = \"balanced\".")
        if(x > 1) hpaths(x, matrix = FALSE) else 1

    },
    "eulerian.cross" = {

        stopifnot(is.numeric(x))
        if(x %% 1 != 0 || length(x) != 2 || any(x < 1))
            stop("'x' has to be an integer vector of length 2 with entries >= 1.")
        g1 <- seq_len(x[1])
        g2 <- x[1] + seq_len(x[2])
        as.numeric(eulerian(bipartite_graph(g1, g2)))

    },
    "eulerian.weighted" =, "strictly.weighted" = {

        ## Deal with missing 'x' if pairs are given
        if(missing(x)) {
            if(missing(pairs))
                stop("'pairs' need to be specified for method = ",method,".")
            if(!is.matrix(pairs)) pairs <- as.matrix(pairs)
            ## Now pairs are given but 'x' is missing => construct 'x'
            x <- 1:nrow(pairs) # 'decreasing' is dealt with differently for the different methods
        }

        ## If 'x' is a matrix or distance matrix, convert it to a vector
        if(is.matrix(x) || inherits(x, "dist"))
            x <- as.vector(as.dist(x)) # => lower triangular matrix as vector

        ## Check
        if(!is.vector(x))
            stop("'x' needs to be a vector (or matrix, or distance matrix).")

        ## Check pairs
        if(is.null(pairs)) {
            ## Check if length(x) is of the form 'n*(n-1)/2'
            nVars <- (1+sqrt(1+8*length(x)))/2
            if(nVars %% 1 != 0)
                stop("'x' has to be of length n*(n-1)/2 for some n >= 2.")
            ## Build matrix of (all) pairs
            pairs <- expand.grid(1:nVars, 1:nVars)
            pairs <- pairs[pairs[,1] > pairs[,2],] # => Pairs = (2, 1), (3, 1), ... (=> lower triangular matrix)
            pairs <- as.matrix(pairs)
            rownames(pairs) <- NULL
            colnames(pairs) <- NULL
        }
        if(!is.matrix(pairs)) pairs <- as.matrix(pairs)
        nr <- nrow(pairs)
        stopifnot(length(x) == nr, nr >= 1, ncol(pairs) == 2)
        ## Check whether none of the edges is given more than once (fail-safe programming)
        ## Note: pairs. is not used anymore below!
        pairs. <- pairs
        for(i in 1:nr) { # sort the pairs (only for checking)
            if(pairs.[i,1] < pairs.[i,2]) {
                tmp <- pairs.[i,1]
                pairs.[i,1] <- pairs.[i,2]
                pairs.[i,2] <- tmp
            }
        }
        if(nrow(unique(pairs.)) != nrow(pairs.)) # use sorted 'pairs' to unique-ify and check
            stop("'pairs' needs to have unique rows (possibly after applying rev()).")

        ## Now distinguish between the methods
        if(method == "eulerian.weighted") { # method "eulerian.weighted"

            if(decreasing) x <- -x
            eul <- eulerian(ftM2graphNEL(ft = pairs, W = x, edgemode = "undirected"))
            eul.lst <- lapply(eul, as.numeric) # returns character otherwise
            if(is.vector(eul)) as.numeric(eul) else eul.lst

        } else { # method "strictly.weighted"

            pairs. <- pairs[order(x, decreasing = decreasing),] # sort pairs according to decreasing/increasing weights
            lst.indices <- split(pairs., f = row(pairs.)) # result list of variables/indices
            names(lst.indices) <- NULL # remove names
            lst.indices

        }

    },
    stop("Wrong 'method'"))
}

##' @title Splitting a Matrix into a List of Matrices
##' @param x A matrix
##' @param indices A list of vectors of indices according to
##'        which 'x' is grouped
##' @return A list of (grouped) matrices
##' @author Marius Hofert
group <- function(x, indices)
{
    if(length(dim(x)) != 2)
       stop("'x' needs to have two dimensions")
    stopifnot(is.list(indices))
    lapply(indices, function(ii) x[, unlist(ii), drop = FALSE])
}
