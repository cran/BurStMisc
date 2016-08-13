\name{Cfrag.list}
\alias{Cfrag.list}
\title{ Write a Fragment of a C Program }
\description{
Writes either the C declaration of items in a list, or initializes them with
the contents of the list.
}
\usage{
Cfrag.list(x, file = NULL,  item.num = c(3, 10, 5),
	indent = c("\t", "\t\t"), declaration.only = FALSE,
	long = FALSE, append = FALSE)
}
\arguments{
  \item{x}{
	required.
	A list with names.
	}
  \item{file}{
	either \code{NULL} or a character string.
	If \code{NULL} (or an empty string), then a vector of characters
	is returned.
	}
  \item{item.num}{
	length three vector giving the number of items per line for doubles,
	integers and characters.
	}
  \item{indent}{
	length two vector giving the amount to indent declarations and the
	items in the initialization.
	}
  \item{declaration.only}{
	logical flag.
	If \code{TRUE}, then the variables are declared but not given
	initial values.
	}
  \item{long}{
	logical flag.
	If \code{TRUE}, then integers are declared to be "long".
	If \code{FALSE}, then integers are declared to be "int".
	}
  \item{append}{
	logical flag.
	If \code{TRUE}, then the file is appended to; otherwise it is
	overwritten if it exists.
	}
}
\value{
if \code{file} is a non-empty character string, then the name of the file
that is written.
Otherwise, a character vector of the declarations -- each element representing
a different line.
}

\section{Side Effects }{
if \code{file} is a non-empty character string, then the file is created,
overwritten or appended.
}

\section{Details }{
The type to declare is dependent on the storage mode of the component of
\code{x}.
You may need to coerce components to get them to be declared the correct type.
}

\note{
Rcpp now probably makes most uses of this function obsolete.
}

\seealso{
\code{.C}, \code{storage.mode}, \code{as.double},
\code{as.integer}, \code{as.character}, \code{cat}.
}

\examples{
test.list <- list(adoub=as.double(rnorm(20)), anint=as.integer(92:109),
	achar=c("aaa", "bbbb", "ccccc"))

Cfrag.list(test.list, file="")

\dontrun{
Cfrag.list(test.list, file="test.c")

Cfrag.list(test.list[1], file="test.c", dec=TRUE)
Cfrag.list(test.list[-1], file="test.c", dec=FALSE, append=TRUE)
}
}
