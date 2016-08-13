\name{scriptSearch}
\alias{scriptSearch}
\title{
Search R scripts for text
}
\description{
Returns a list of text matches in the scripts in a directory.
}
\usage{
scriptSearch(pattern, path = ".", subdirs = TRUE, suffix = "\\\\.[rR]$",
      commentsIncluded = FALSE, ..., verbose = FALSE)
}
\arguments{
  \item{pattern}{
character string containing a regular expression
(this is the argument of the same name for \code{\link{grep}}).
}
  \item{path}{
a character string giving the (relative) path of the directory holding
the R scripts.
}
  \item{subdirs}{
logical value: if \code{TRUE}, then subdirectories of \code{path} are
also searched.
}
  \item{suffix}{
a character string restricting the files to be searched.
}
  \item{commentsIncluded}{
logical value: if \code{FALSE}, then lines beginning with the hash
symbol are ignored.
}
  \item{\dots}{
additional arguments to \code{grep} may be given -- the most likely
possibilities are \code{ignore.case}, \code{perl} and \code{fixed}.
}
  \item{verbose}{
logical value: if \code{TRUE}, then the names of the scripts being
searched are printed.  The full name of this argument needs to be given --
no abbreviation is allowed.
}
}

\value{
a list: the names are equal to the file names containing matches,
and each component is a character vector of the whitespace-trimmed
lines in the file that match the pattern.
}
\details{
The default \code{suffix} argument restricts the search to R scripts.
An R script is considered to be a file that ends in dot-R, where the
"R" can be lowercase or uppercase.
}
\section{Limitations }{
Using \code{commentsIncluded=FALSE} does not preclude a match being
declared when it is in a comment not starting the line.
}
\author{
Pat Burns
}
\seealso{
\code{\link{grep}}
}
\examples{
\dontrun{
scriptSearch("list.files")
scriptSearch("garch", "~/../burns-stat3/webpages/blog")

# search for an assignment
# ' *' means zero or more spaces
scriptSearch("specialObject *<- ", "~/myScriptDirectory", subdirs=FALSE)

# search COBOL files
scriptSearch("blah", suffix="\\\\.cbl$")

}
}
\keyword{ character }
