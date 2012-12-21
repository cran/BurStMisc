\name{plot.permtstBurSt}
\alias{plot.permtstBurSt}
\title{ Plot Distribution from a Permutation Test }
\description{
Shows the distribution of the scores of the random permutations relative
to the score from the actual data.
}
\usage{
\method{plot}{permtstBurSt}(x, col = c("black", "red"), width = 10, 
	uniqlim = 10, main = "", xlab = "Scores", ...)
}
\arguments{
  \item{x}{ an object of class \code{permtstBurSt}, which is likely
the result of:

 \code{permutation.test.discrete} or \code{permutation.test.fun}.
}
  \item{col}{ the colors to use in the plot -- mainly for the bar-type
plot when there are only a few scores in the permutation distribution.
The first color is the one used for the insignificant part of the distribution,
the second color is for the significant part of the distribution.
The second color is used to identify the original score when a histogram 
is used.
}
  \item{width}{ the width of the bars.
}
  \item{uniqlim}{ the value that determines which type of plot to use.
If the number of unique values in the scores from the permutations
is bigger than \code{uniqlim}, then a histogram is drawn.
Otherwise, bars are drawn.
}
  \item{main}{ character string giving the main title for the plot.
}
  \item{xlab}{ character string giving the label for the x-axis.
}
  \item{\dots}{ additional graphics parameters may be given.
}
}
\section{Side effects }{
a plot is produced.
}
\section{Details }{
Two styles of plot are possible.
A histogram is more appropriate when there are more than a few
values in the permutation distribution.
When there are only a few distinct values, 
then a bar-type plot is more informative.
The \code{uniqlim} argument controls the definition of "a few".

These functions are related to "Permuting Super Bowl Theory" which can
be found in the working papers section of \url{http://www.burns-stat.com}.
The paper explains permutation tests via a discussion of the Super Bowl
indicator of the stock market.
}
\seealso{ \code{\link{permutation.test.discrete}}.
}
\examples{
\dontrun{
pt1 <- permutation.test.discrete(winner, market, smat)
plot(pt1, col=c("blue", "gold"))
}
}
\keyword{ htest }
