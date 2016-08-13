\name{ntile}
\alias{ntile}
\title{
Membership of ntile groups
}
\description{
Creates groups where the groups each have as close to the same number of members as possible.
}
\usage{
ntile(x, ngroups, na.rm = FALSE, result = "list",
    reverse = FALSE, checkBleed = TRUE)
}
\arguments{
  \item{x}{
a numeric vector.
}
  \item{ngroups}{
a positive integer giving the number of groups to create.
}
  \item{na.rm}{
logical value: if \code{TRUE}, then missing values are removed before
forming the groups.
}
  \item{result}{
a character string specifying the form of the resulting value.
This must be (an abbreviation of) one of: \code{"list"}, \code{"numeric"},
\code{"factor"}
}
  \item{reverse}{
logical value: if \code{TRUE}, then the biggest values are in group 1.
}
  \item{checkBleed}{
logical value: if \code{TRUE}, then a warning is issued if the same
value appears in more than one group.
}
}
\value{
the form of the value depends on the \code{result} argument.

The \code{"list"} result has the values of the input \code{x} grouped
into \code{ngroups} components.

The \code{"numeric"} result is a vector of integers from 1
to \code{ngroups} indicating which group the corresponding element
of \code{x} is in.

The \code{"factor"} result is an ordered factor version of the
\code{"numeric"} result.
}
\author{
Pat Burns
}
\note{
A more primitive version of this function appeared in a blog post
called "Miles of iles" on the Portfolio Probe website.
A bit of comparison with the alternative functions in See Also may
be found there.
}
\seealso{
\code{\link{cut}}, \code{\link[Hmisc]{cut2}},
\code{\link[gtools]{quantcut}}.
}
\examples{
ntile(setNames(state.area, state.name), 10)

ntile(Loblolly$height, 5, result="factor", reverse=TRUE)

ntile(c(-10:10, rep(0, 7)), 4)
}
\keyword{ distribution }
