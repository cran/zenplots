\name{burst}
\alias{burst}
\alias{burst_}
\title{Splitting an Input Object into a List of Columns}
\description{
  Split a (numeric/logical/character) vector, matrix, data.frame or a list of
  such into a list of columns, with corresponding group and variable
  information and labels.
}
\usage{
burst(x, labs = list())
burst_(x, labs = "V")
}
\arguments{
  \item{x}{\code{\link{numeric}} \code{\link{vector}}, \code{\link{matrix}},
    \code{\link{data.frame}} or, for \code{burst()}, a
    \code{\link{list}} of such.}
  \item{labs}{for
    \describe{
      \item{\code{burst()}:}{either \code{\link{NULL}} (in which case
	neither group nor variable labels are computed) or a list
	containing the components \code{group} (either
	\code{\link{NULL}}, the group label base-name or labels for the group labels),
	\code{var} (either \code{\link{NULL}}, the variable label
	base-name or labels for the variable labels) and \code{sep} (the
	separator between the two). If any of these three components is
	not given, it is set to the defaults as can be found in
	\code{\link{zenplot}()}.}
      \item{\code{burst_()}:}{either \code{\link{NULL}} (in which case
	no variable labels are computed), a \code{\link{vector}} of
	length 1 (then appended by the variable number) or a
	\code{\link{vector}} of length equal to the number of columns of
	\code{x} giving the variable labels.}
    }
    Note that if at least one (group or variable) label is given in
    \code{x}, then those (original) labels will be used.
    }
}
\value{
  \code{burst_()} returns a \code{\link{list}} containing all columns
  of \code{x} (possibly with constructed variable names).

  \code{burst()} returns a \code{\link{list}} of length five,
  containing all columns of \code{x} (possibly with constructed group
  and variable names), the group and variable numbers (indices), and the group and
  variable labels.
}
\author{Marius Hofert}
\examples{
## Checking out burst_() and burst() (which itself calls burst_())

## burst_()
A <- matrix(1:12, ncol = 3)
burst_(A) # no labels given => generate them with default 'V'
burst_(A, labs = LETTERS[1:3]) # use letters
colnames(A) <- LETTERS[1:3] # alternatively, use column names
burst_(A)

## Unnamed list of (some named, some unnamed) valid components
x <- list(A, 1:4, as.data.frame(A))
burst(x, labs = list(group = "G", var = "V", sep = ", "))
burst(x) # the same defaults as above
burst(x, labs = list(sep = " ")) # only changing the separator
## Note: - No group labels are given in 'x' and thus they are constructed
##       - The variable names are only constructed if not given
burst(x, labs = list(group = ""))
burst(x, labs = list(group = NULL)) # no group labels
burst(x, labs = list(var = NULL)) # no variable labels
burst(x, labs = list(group = NULL, var = NULL)) # neither one
burst(x, labs = NULL) # similarly, without any labels at all

## Named list
x <- list(mat = A, vec = 1:4, df = as.data.frame(A))
burst(x)
## Note: - The given group labels are used
##       - The variable names are only constructed if not given

## Partially named list
x <- list(mat = A, vec = 1:4, as.data.frame(A))
burst(x)
burst(x, labs = list(group = NULL)) # no group labels
burst(x, labs = list(var = NULL)) # no variable labels
burst(x, labs = list(group = NULL, var = NULL)) # neither one
}
\keyword{datagen}