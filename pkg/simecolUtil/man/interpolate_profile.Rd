\name{interpolate_profile}
\alias{interpolate_profile}

\title{
  Interpolate 1D Profile Data Over Time.
}
\description{
 The function may also be useful for other 2D interpolation purposes.
}
\usage{
interpolate_profile(x, y, z, xo = seq(min(x), max(x), length = 100), 
  yo = seq(min(y), max(y), length = 100), output = c("xyz", "grid"))
}

\arguments{
  \item{x}{
    x-coordinates (e.g. time),
}
  \item{y}{
    y-coordinates (e.d. depth),
}
  \item{z}{
    z-values, i.e. values that are to be interpolated.
}
  \item{xo}{
 vector of x-coordinates of output grid. The default is 100 points
 evenly spaced over the range of x. 

}
  \item{yo}{
 vector of x-coordinates of output grid. The default is 100 points
 evenly spaced over the range of x. 

}
  \item{output}{
determines whether the result is a grid (like in \code{\link{interp}} or
  a data frame with x, y, z-values.
}
}

\value{
  Either a grid (like in \code{\link{interp}} or
  a data frame with x, y, z-values.
}

\seealso{
\code{\link{interp}}, \code{\link{expand.grid}}
}
\examples{
## basic usage with image
data(t_xyz)
grid <- with(t_xyz, interpolate_profile(day, depth, temp, output = "grid"))
image(grid, ylim = c(10, 0))

## 'contourplot' from lattice uses x, y, z data
xyz <- with(t_xyz, interpolate_profile(day, depth, temp, output = "xyz"))

contourplot(z ~ x * y, data = xyz,
  cex.axis=1.6,
  aspect = "fill",
  xlab = "",
  ylab = "Depth (m)",
  main = "",
  ylim = c(10,0),
  cuts = 50,
  contour = FALSE,
  region = TRUE,
  col.regions = rev(palette_RYB(100)),
)

## Draw 10 deg C isotherme
contourplot(z ~ x * y, data = xyz,
  cex.axis=1.6,
  aspect = "fill",
  xlab = "",
  ylab = "Depth (m)",
  main = "",
  ylim = c(10,0),
  cuts = 50,
  contour = FALSE,
  region = TRUE,
  col.regions = rev(palette_RYB(100)),
  panel=function(x, y, z, subscripts, ...) {
      panel.levelplot(x, y, z, subscripts, ...)
      panel.levelplot(x, y, z, subscripts, contour = TRUE, at = 10)
  }
)

}

\keyword{ misc }

