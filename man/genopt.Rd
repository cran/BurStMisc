\name{genopt}
\alias{genopt}
\title{ Genetic Optimization
}
\description{
Approximately minimizes the value of a function using a simple
heuristic optimizer that uses a combination of genetic and
simulated annealing optimization.
}
\usage{
genopt(fun, population, lower = -Inf, upper = Inf, 
	scale = dcontrol["eps"], add.args = NULL, 
	control = genopt.control(...), ...)
}
\arguments{
  \item{fun}{ 
function that returns a numerical value.
}
  \item{population}{ 
a matrix or a list.

If a matrix, then the rows correspond to the parameters and the
columns are different parameter vectors.

If a list, then it should have a component named "population" and
optionally a component named "objective" -- in particular, it can
be the result of a call to \code{genopt}.  
If the "objective" component exists, then it is assumed to be the
function values of the population and the function will not be
evaluated on the members of the population.
}
  \item{lower}{ 
vector giving the lower bound for parameter values.
This is replicated to be as long as the number of parameters.
}
  \item{upper}{ 
vector giving the upper bound for parameter values.
This is replicated to be as long as the number of parameters.
}
  \item{scale}{ 
vector of scales to use when doing local search with a solution.
This is replicated to be as long as the number of parameters.
}
  \item{add.args}{ 
list of additional arguments to \code{fun}.
}
  \item{control}{ 
an object like the output of \code{genopt.control} containing
control parameters for the optimization.
}
  \item{\dots}{ 
arguments for \code{genopt.control} if the \code{control} argument
is not given explicitly.
}
}
\value{
a list with the following components:
  \item{population }{
a matrix of the same size as the input population matrix, but generally
with different values in it.
}
  \item{objective }{
a vector with length equal to the number of columns of \code{population}
containing the function value for the corresponding parameter vector in
\code{population}.
}
  \item{funevals }{
the number of function evaluations performed.
If \code{population} is given as a list that contains a "funevals"
component, then that number is added to the number of evaluations
done in the current call.
}
  \item{random.seed }{
the random seed at the start of the call -- given so that you can
reproduce the computations.
}
  \item{call }{
an image of the call that created this object.
}
}
\references{ 
The original version of this function appeared in "S Poetry".
}
\seealso{ \code{\link{genopt.control}}.
}
\examples{
# two parameters, population size 5
go1 <- genopt(function(x, other) sum(x, other),
   matrix(rexp(10), nrow=2, ncol=5), lower=0, add.arg=list(other=3),
   trace=FALSE)

go2 <- genopt(function(x, other) sum(x, other), go1,
   lower=0, add.arg=list(other=3), trace=FALSE)

}
\keyword{optimize}
