\name{genopt.control}
\alias{genopt.control}
\title{ Control parameters for genopt
}
\description{
Returns a list suitable as the \code{control} argument
of the \code{genopt} function.
}
\usage{
genopt.control(births = 100, random.n = 0, jitters.n = 3, 
   trace = TRUE, eps = 0.1, prob = 0.4, scale.min = 1e-12, 
   maxeval = Inf)
}
\arguments{
  \item{births}{ 
the number of times two parents are combined to create a
new solution.
}
  \item{random.n}{ 
the number of random solutions created (and evaluated)
before the genetic phase starts.
}
  \item{jitters.n}{ 
the number of times a newly successful solution is changed
(in the simulated annealing phase)
in an attempt to find a nearby better one.
}
  \item{trace}{ 
logical value: should progress be printed?
}
  \item{eps}{ 
the default value for the scale of the jittering in
simulated annealing.
}
  \item{prob}{ 
in the selection of parameters between two parents, the probability
of each coming from the first parent.
}
  \item{scale.min}{ 
the minimum value allowed in the \code{scale} argument of
\code{genopt}.
}
  \item{maxeval}{ 
the maximum number of function evaluations allowed.
This takes previous function calls into account if \code{funevals}
is a component of the \code{population} argument of
\code{genopt}.
}
}
\value{
a list with components:
  \item{icontrol }{
vector of the control parameters that are logically integer 
(or logical).
}
  \item{dcontrol }{
vector of the control parameters that are logically real-valued.
}
}
\seealso{ \code{\link{genopt}}.
}
\examples{
goc1 <- genopt.control(random.n=200, births=1000, trace=FALSE)

go1 <- genopt(function(x, other) sum(x, other),
   matrix(rexp(10), nrow=2, ncol=5), lower=0, add.arg=list(other=3),
   control=goc1)

goc2 <- genopt.control(random.n=0, births=1000, trace=FALSE)

go2 <- genopt(function(x, other) sum(x, other), go1,
   lower=0, add.arg=list(other=3), control=goc2)

}
\keyword{optimize}
