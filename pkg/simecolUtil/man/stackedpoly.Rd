\name{stackedpoly}
\alias{stackedpoly}

\title{Stacked polygon plot}
\description{
  Plot a data data frame with one independent and several dependend variables as stacked polygons.
}
\usage{
stackedpoly(x, columns, density = NA, angle = 45, border = NA, col = NA, xlab = "x values", ylab = "y values", ...)
}

\arguments{
 \item{x}{object of class \code{data.frame}, the first column is used as x-axis.} 
  \item{columns}{vector of column names in appropriate order which are plotted}
    \item{density}{vector with densities of shading lines, in lines per inch.  The
    default value of \code{NA} means that no shading lines are drawn and so allow
    color filling.}
  \item{angle}{vector with slopes of shading lines, given as an angle
     in degrees (counter-clockwise).}
  \item{border}{the color to draw the border.  The default, \code{NA},
    means to omit borders.}
   \item{col}{the color for filling the polygon. The default,
     \code{NA}, is to leave polygons unfilled, unless \code{density} is
     specified.}
  \item{xlab}{a title for the x axis: see \code{\link{title}}.}
  \item{ylab}{a title for the y axis: see \code{\link{title}}.}
  \item{\dots}{other graphical parameters passed to \code{\link{plot}} resp. \code{\link{polygon}}.}
}

\seealso{\code{\link{plot.default}}, \code{\link{polygon}}}
\examples{


## test data
dat <- data.frame(time = seq(7, 365, 14),
                  y1 = runif(26, min=1, max=10),
                  y2 = runif(26, min=0, max=15),
                  y3 = runif(26, min=0, max=2)
)
                  

## example palette
palette <-c("grey", "brown", "green", "yellow", "yellowgreen",
            "darkgreen", "blue", "black", "red", "grey", "red", "orange",
            "pink", "cyan"
)


## usage
stackedpoly(dat, c("y1", "y2", "y3"), col=palette)

## polygons with border
stackedpoly(dat, c("y1", "y2", "y3"), col=palette, border="black")

## optional plotting parameters
stackedpoly(dat, c("y1", "y2", "y3"), col=palette, ylim=c(0,50), main="Phytoplankton")

## changed order
stackedpoly(dat, c("y3", "y2", "y1"), col=palette, main="Phytoplankton")

## hatched area
stackedpoly(dat, c("y3", "y2", "y1"), density=c(20, 20, 40),
  angle=c(0, 90, 45), ylim=c(0,50), border=palette, main="Phytoplankton")
  
}

\keyword{ hplot }
