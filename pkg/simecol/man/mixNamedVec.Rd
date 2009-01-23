\name{mixNamedVec}
\alias{mixNamedVec}

\title{
  Mix Two Named Vectors, Resolving Name Conflicts
}
\description{
  The function mixes two named vectors. The resulting vectors contains
  all elements with unique name and only one of the two versions
  of the elements which have the same name in both vectors.
}
\usage{
mixNamedVec(x, y, resolve.conflicts = c("x", "y"), warn = TRUE)
}

\arguments{
  \item{x}{first named vector,}
  \item{y}{second named vector,}
  \item{resolve.conflicts}{name of the vector from which all elements
    are taken,}
  \item{warn}{an indicator if a warning should be given if elements are
    not unique. This argument should usually set to FALSE, but the default
    is TRUE to be on the safe side.}
}

\value{
  a vector with all elements from one vector and only these elements
  of the second, that have a unique name not contained in the other vector.
}
\author{
Thomas Petzoldt
}
\seealso{
\code{\link{which}}
}
\examples{
x <- c(a=1, b=2, c=3)
y <- c(a=1, b=3, d=3)

mixNamedVec(x, y)
mixNamedVec(x, y, resolve.conflicts="x")

mixNamedVec(x, y, resolve.conflicts="x", warn=FALSE)
mixNamedVec(x, y, resolve.conflicts="y", warn=FALSE)

## without names, returns vector named in "resolve conflicts"
x <- as.vector(x)
y <- as.vector(y)
mixNamedVec(x, y)
mixNamedVec(x, y, resolve.conflicts="y")

## names partly
x <- c(4, a=1, b=2, c=3, 4, 9)
y <- c(a=1, 6, b=3, d=3, 8)

mixNamedVec(x, y)
mixNamedVec(x, y, resolve.conflicts="y")
}

\keyword{ misc }

