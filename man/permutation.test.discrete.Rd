\name{permutation.test.discrete}
\alias{permutation.test.discrete}
\alias{permutation.test.fun}
\alias{print.permtstBurSt}
\title{ Random Permutation Test}
\description{
Performs a random permutation test on the relationship between 
two discrete variables, or by using a function.
}
\usage{
permutation.test.discrete(x, y = NULL, scores, alternative = "greater", 
	trials = 1000)
permutation.test.fun(x, y = NULL, fun = function(x, y) sum(x * y), 
	alternative = "greater", trials = 1000)
}
\arguments{
  \item{x}{ 
	either a two-column matrix or data frame, or a vector.
	When this has two columns, \code{y} should not be given.
}
  \item{y}{ 
	either a zero-length object, or a vector the same length as \code{x}.
}
  \item{scores}{ 
	a numeric matrix providing the scores for each combination of
	the unique values in \code{x} and \code{y}.
	The dimnames need to consist of these unique values.
}
  \item{fun}{ 
	a function that takes two arguments and returns a single numeric value.
}
  \item{alternative}{ 
	a character string that partially matches either \code{"greater"}
	or \code{"less"}.
}
  \item{trials}{ 
	the number of random permutations to be performed.
}
}
\value{
  an object of class \code{permtstBurSt} which is a list with the
	following components:
  \item{original.score }{
	the score (or function value) produced by the original data.
}
  \item{perm.scores }{
a vector of the scores (or function values) from the random permutations.
}
  \item{stats }{
a numeric vector that has the number of observations in the data, the
number of random permutations done, the number of permutations that
produced a score at least as extreme as the original, and the p-value
for the test.
}
  \item{alternative }{
either \code{"greater"} or \code{"less"}.
}
  \item{random.seed }{
the random seed at the start of the call.
}
  \item{call }{
an image of the call that was used.
}
}
\section{Side effects }{
The object \code{.Random.seed} is either created or updated.
}
\section{Details }{
There are \code{print} and \code{plot} methods for this class of object.
The print method merely describes the object and shows the p-value of the
test (rounded, by default, to 4 digits).

A simple version of the p-value is the
number of random permutations that are at least as extreme as the original
divided by the total number of random permutations.
The value computed, which is more correct, has 1 added to both numerator 
and denominator.
In general the difference is of no consequence.  
However, there are cases where it does matter, for example when independent
p-values are combined.

These functions are related to "Permuting Super Bowl Theory" which can
be found in the working papers section of \url{http://www.burns-stat.com}.
The paper explains permutation tests via a discussion of the Super Bowl
indicator of the stock market.
}
\seealso{ 
\code{\link{plot.permtstBurSt}}.
}
\examples{
winner <- c('N', 'N', 'A', 'N', 'A', 'N')
market <- c('+', '-', '-', '+', '+', '+')
smat <- diag(2)
dimnames(smat) <- list(c('N', 'A'), c('+', '-'))

pt1 <- permutation.test.discrete(winner, market, smat)
print(pt1)
plot(pt1)

pt2 <- permutation.test.fun(ToothGrowth[, -2], fun=cor)
print(pt2)
plot(pt2)


smat2 <- matrix(c(-3, -.5, 3, -1, 1, 0, 0, 1, -1, 3, -.5, -3),
	3, 4, dimnames=list(c('Up', 'Neut', 'Down'), 
	c('Q1', 'Q2', 'Q3', 'Q4')))
my.results <- data.frame(results=sample(c('Up', 'Neut', 'Down'),
       100, replace=TRUE), quartile=sample(
       c('Q1', 'Q2', 'Q3', 'Q4'), 100, replace=TRUE))
permutation.test.discrete(my.results[, c("results", "quartile")], 
	score=smat2)

}
\keyword{ htest }
