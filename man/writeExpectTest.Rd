\name{writeExpectTest}
\alias{writeExpectTest}
\title{
Text for a testthat test
}
\description{
Automatically writes the text of a \code{testthat::expect_equal} test of an expression.
}
\usage{
writeExpectTest(expr, filename = "", ...)
}
\arguments{
  \item{expr}{
an expression to be tested.
}
  \item{filename}{
where should the results go to?
The empty string means that it goes to the console.
}
  \item{\dots}{
additional arguments to \code{cat} may be given to control the output.
The most likely choice is \code{append}.
}
}

\value{
\code{NULL}
}
\section{Side effects }{
text is written to the console or to a file.
}

\note{
Some functions are hard to test, like \code{\link{scriptSearch}}.
It seems like this one would be too, but in fact it is self-testing.
If the test it writes doesn't pass in the same environment, then it
must have done something wrong.
}

\seealso{
\code{\link[testthat]{expect_equal}}.
}
\examples{
writeExpectTest(head(1:10))
writeExpectTest(head(cars))
}
\keyword{programming }
