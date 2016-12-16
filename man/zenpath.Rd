\name{zenpath}
\alias{zenpath}
\alias{extract_pairs}
\alias{connect_pairs}
\alias{group}
\title{(Group) Indices to Subset a Matrix for a Zenplot}
\description{
  Constructing (possibly grouped) indices which can be used to subset
  a data matrix for plotting via a zenplot.
}
\usage{
zenpath(x, pairs = NULL,
        method = c("front.loaded", "back.loaded", "balanced",
                   "eulerian.cross", "eulerian.weighted", "strictly.weighted"),
        decreasing = TRUE)
extract_pairs(x, n)
connect_pairs(x, duplicate.rm = FALSE)
group(x, indices)
}
\arguments{
  \item{x}{for
    \describe{
      \item{\code{zenpath}:}{for \code{method}
	\describe{
	  \item{\code{"front.loaded"}:}{single \code{\link{integer}}.}
	  \item{\code{"back.loaded"}:}{as for \code{method = "front.loaded"}.}
	  \item{\code{"balanced"}:}{as for \code{method = "front.loaded"}.}
	  \item{\code{"eulerian.cross"}:}{two \code{\link{integer}}s
	    representing the group sizes.}
	  \item{\code{"eulerian.weighted"}:}{\code{\link{numeric}}
	    \code{\link{vector}} (or
	    \code{\link{matrix}} or distance matrix).}
	  \item{\code{"strictly.weighted"}:}{as for
	    \code{method = "eulerian.weighted"}.}
	}
      }
      \item{\code{extract_pairs}:}{the path, a \code{\link{vector}} or
	\code{\link{list}} of indices of the variables to be plotted.}
      \item{\code{connect_pairs}:}{two-column \code{\link{matrix}} or
	a \code{\link{list}} containing vectors of length two.}
      \item{\code{group}:}{\code{\link{matrix}} (or an object
	convertible to such via \code{\link{as.matrix}()}).}
  }}
  \item{pairs}{two-column \code{\link{matrix}} containing (row-wise)
    the pairs of variables to be sorted according to the
    weights. \code{pairs} is only used for methods
    \code{eulerian.weighted}, \code{strictly.weighted}.}
  \item{method}{\code{\link{character}} string indicating the sorting
    method to be used. Available are:
    \describe{
      \item{\code{"front.loaded"}:}{sort all pairs such that the first
	variables appear the most frequently early in the sequence.}
      \item{\code{"back.loaded"}:}{sort all pairs such that the later variables appear
        the most frequently later in the sequence.}
      \item{\code{"balanced"}:}{sort all pairs such that all variables
	appear in balanced blocks throughout the sequence
	(a Hamiltonian Decomposition).}
      \item{\code{"eulerian.cross"}:}{generate a sequence of pairs such
	that each is formed with one variable from each group.}
      \item{\code{"eulerian.weighted"}:}{sort all pairs according to a
	greedy (heuristic) Euler path visiting each edge precisely once.}
      \item{\code{"strictly.weighted"}:}{this method strictly respects the order
	given by the weights.}
  }}
  \item{decreasing}{\code{\link{logical}} indicating whether the
    sorting is done according to increasing or decreasing weights.}
  \item{n}{\code{\link{vector}} of length two giving the number of pairs to
    extract from the path \code{x} (if \code{NULL}, all pairs are
    returned (nothing extracted); if of length one, it is replicated)
    The first number corresponds to the beginning of the path,
    the second to the end; at least one of the two numbers should be >= 1.}
  \item{duplicate.rm}{\code{\link{logical}} indicating whether equal
    pairs (up to permutation) are omitted.}
  \item{indices}{list of vectors of indices according to which \code{x}
    is grouped.}
}
\value{
  \code{zenpath()} returns a sequence of variables (indices or names,
  possibly a list of such), which can then be used to index the data
  (via \code{group()}) for plotting via \code{\link{zenplot}()}.

  \code{extract_pairs()} returns an object of the same type as the input
  \code{x} but (possibly) shortened. It extracts the first/last so-many
  pairs of \code{x}.

  \code{connect_pairs()} returns a \code{\link{list}} of (possibly connected)
  pairs, so a list of vectors of length at least 2.

  \code{group()} returns a \code{\link{list}} of (grouped)
  matrices. This is then typically passed on to \code{\link{zenplot}()}.
}
\author{Marius Hofert and Wayne Oldford}
%% \references{
%%   Hofert, M., Oldford, W. (2015). Zigzag Expanded Navigation Plots.
%%   \emph{} \bold{}(), --.
%% }
\seealso{
  \code{\link{zenplot}()} which provides the zenplot.
}
\examples{
## A baby example to see how group() works
A <- matrix(1:12, ncol = 3)
lst <- list(1, list(2:3))
group(A, indices = lst) # split the matrix according to the grouping given by lst

## Some calls of zenpath()
zenpath(10) # integer argument
## Note that the result is of length 50 > 10 choose 2 as the underlying graph has to
## be even (and thus edges are added here)
(zp <- zenpath(c(3, 5), method = "eulerian.cross")) # integer(2) argument

## Extract the first and last three pairs of indices
extract_pairs(zp, n = 3)

## A more sophisticated example
nVars <- 5 # number of variables involved
set.seed(271)
x <- runif(nVars*(nVars-1)/2) # weights
## Construct the pairs
pairs <- expand.grid(1:nVars, 1:nVars)[,2:1]
pairs <- pairs[pairs[,1] < pairs[,2],]
pairs <- matrix(unlist(pairs), ncol = ncol(pairs))
stopifnot(length(x) == nrow(pairs)) # sanity check
## Manually compute the result of method = "strictly.weighted" and group the pairs
## 1) Sort pairs according to the weights x
(pairs. <- pairs[order(x, decreasing = TRUE),])
## 2) Now go through the rows and determine the sequence of adjacent pairs
##    which can be plotted with a zenplot
res <- list(c(5,3,1),
            c(3,2,5),
            c(4,1,5),
	    c(1,2),
	    c(5,4,3),
	    c(2,4))
## Call zenpath() and check whether we get the same
(zp  <- connect_pairs(zenpath(x, pairs = pairs, method = "strictly.weighted")))
stopifnot(identical(zp, lapply(res, as.integer)))

## Extract the first and last three pairs of indices
(ezp <- extract_pairs(zp, n = 3))

## Another example based on a matrix of (trivial) weights
## This also shows that an input matrix 'x' does not have to
## be symmetric. In that case, the lower triangular matrix is used.
d <- 10
x <- matrix(1, nrow = d, ncol = d)
k <- 1
for(j in 1:(d-1)) {
    for(i in (j+1):d) {
        x[i,j] <- k
        k <- k+1
    }
}
x

## Compute the 'strictly.weighted' zenpath (all pairs sorted in decreasing order)
k <- 10 # bottom and top number of pairs (k most extreme pairs)
zpath <- zenpath(x, method = "strictly.weighted") # compute path over all pairs (decreasing weights)
stopifnot(sapply(1:length(zpath), function(i) x[zpath[[i]][1], zpath[[i]][2]]) ==
          45:1) # check
zpath <- connect_pairs(zpath) # connect the pairs
zp <- extract_pairs(zpath, n = c(3, 0)) # grab out the top three pairs
}
\keyword{utilities}