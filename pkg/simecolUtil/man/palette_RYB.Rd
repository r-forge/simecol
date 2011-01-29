\name{palette_RYB}
\alias{palette_RYB}

\title{
Create Smooth Red-Yellow-Blue Palette
}
\description{
 Creates a vector of (n) contiguous colors
  (red-yellow-blue).
} 

\usage{
palette_RYB(n = 100)
}

\arguments{
  \item{n}{
    number of colors.
  }
}

\value{
 A character vector with n color codes.
}

\references{
  This function is a wrapper around a modified palette according to:
  
  Erich Neuwirth <erich.neuwirth@univie.ac.at> (2007).
  RColorBrewer: ColorBrewer palettes. R package
  version 1.0-2.
 
  The palette is based on \url{http://www.colorbrewer.org} provided by 
  Cynthia Brewer, Mark Harrower and The Pennsylvania State University.
}

\seealso{
\code{\link{rainbow}}, \code{\link{heat.colors}},
  \code{\link{topo.colors}}
}
\examples{

col <- palette_RYB(10)
x <- 1:100
z <- x \%o\% x
image(z, col=col)

}
\keyword{ misc }

