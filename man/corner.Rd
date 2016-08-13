\name{corner}
\alias{corner}
\title{ Portion of an Array or Data Frame }
\description{
  Returns an array (or data frame) of the same number of dimensions
  as the input, but generally with smaller dimensions.
}
\usage{
  corner(x, corner = "tlffff", n = 6)
}
\arguments{
  \item{x}{an array (which includes matrices) or data frame. }
  \item{corner}{a single character string which codes the particular
    corner of the array that is desired.
    The first character is either "t" (top) or "b" (bottom).
    The second character is either "l" (left) or "r" (right).
    Remaining characters should be either "f" (front) or "b" (back).
  }
  \item{n}{a vector of positive integers which generally should have
    length at least that of the \code{dim} of the array -- it is
    replicated if not.
    This states (the maximum of) each of the dimensions of the result.
  }
}
\value{
  an array similar to the input \code{x}, but with smaller dimensions in general.
}

\seealso{ \code{\link{head}}, \code{\link{tail}}. }

\examples{
  corner(freeny.x)
  corner(freeny.x, "br", c(2,3))

  # with three-dimensional arrays:
  corner(iris3)
  corner(array(1:1000, rep(10, 3)), "brf", 2:4)
}
\keyword{ manip }
