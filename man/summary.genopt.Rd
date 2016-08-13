\name{summary.genopt}
\alias{summary.genopt}
\title{
Summary of genopt object
}
\description{
The call, best solution and summary of objectives in the final population.
}
\usage{
\method{summary}{genopt}(object, ...)
}
\arguments{
  \item{object}{
the result of a call to \code{genopt}.
}
  \item{...}{
currently unused.
}
}
\value{
a list with components:
\item{call }{
image of the call to \code{genopt}.
}
\item{summary.objectives }{
summary of the objectives of the solutions in the final population.
}
\item{best.solution }{
the vector of parameters with the best solution in the population.
}
}

\seealso{
\code{\link{genopt}}.
}
\examples{
  # two parameters, population size 5
  go1 <- genopt(function(x, other) sum(x, other),
                population=matrix(rexp(10), nrow=2, ncol=5),
                lower=0, add.arg=list(other=3), trace=FALSE)
  summary(go1)
}
\keyword{optimize}
