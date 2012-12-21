\name{corner}
\alias{corner}
\title{ Portion of an Array }
\description{
Returns an array of the same number of dimensions as the input,
but generally with smaller dimensions.
}
\usage{
corner(x, corner = "tlffff", n = rep(6, length(corner)))
}
\arguments{
  \item{x}{an array or data frame. }
  \item{corner}{a single character string which codes the particular
corner of the array that is desired.
The first character is either "t" (top) or "b" (bottom).
The second character is either "l" (left) or "r" (right).
Remaining characters should be either "f" (front) or "b" (back).
 }
  \item{n}{a vector of positive integers which should have length
at least that of the \code{dim} of the array.
This states (the maximum of) each of the dimensions of the result.
 }
}
\value{
an array similar to the input \code{x}, but with smaller dimensions in general.
}

\seealso{ \code{\link{head}}, \code{\link{tail}}. }

\examples{
corner(freeny.x)
corner(freeny.x, "br", c(2,2))

# R example
corner(iris3)

# S-PLUS example
corner(iris)

}
\keyword{ manip }% at least one, from doc/KEYWORDS
