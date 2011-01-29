\name{vollenweider}
\alias{vollenweider}

\title{Orthophosphate Version of the Vollenweider Model}
\description{
  The Vollenweider model is a standard method for the trophic state
  classification of lakes and reservoirs.
}
\usage{
vollenweider(L, z, t, ...)
}

\arguments{
  \item{L}{ phosporus loading (in g / m2 / year), }
  \item{z}{ mean depth of the lake (in m), }
  \item{t}{ mean theoretical retention time (years), }
  
  \item{\dots}{ additional arguments passed to the
    \code{\link[graphics]{points}} function.}
}


\references{
  Benndorf, J. (1979): A contribution to the phosphorus loading
  concept. Int. Revue ges. Hydrobiol. 64: 177-188.
}

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
vollenweider(10, 10, 1)
}
\keyword{ misc }

