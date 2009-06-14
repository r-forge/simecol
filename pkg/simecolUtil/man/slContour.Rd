\name{slContour}
\alias{slContour}
\title{Level (Contour) Plots (modified version)}
\usage{
  slContour(x = seq(0, 1, len = nrow(z)),
            y = seq(0, 1, len = ncol(z)),
            z,
            xlim = range(x, finite=TRUE),
            ylim = range(y, finite=TRUE),
            zlim = range(z, finite=TRUE),
            levels = pretty(zlim, nlevels), nlevels = 20,
            color.palette = cm.colors,
            col = color.palette(length(levels) - 1),
            plot.title, plot.axes, key.title, key.axes,
            asp = NA, xaxs = "i", yaxs = "i", las = 1, axes = TRUE,
            smooth = TRUE,
            drawlines = TRUE,
            drawlabels = TRUE, 
            \dots)
}
\arguments{
  \item{x,y}{locations of grid lines at which the values in \code{z} are
    measured.  These must be in ascending order.  By default, equally 
    spaced values from 0 to 1 are used.  If \code{x} is a \code{list},
    its components \code{x$x} and \code{x$y} are used for \code{x}
    and \code{y}, respectively. If the list has component \code{z} this
    is used for \code{z}.} 
  \item{z}{a matrix containing the values to be plotted (\code{NA}s are
    allowed).  Note that \code{x} can be used instead of \code{z} for  
    convenience.}
  \item{xlim}{x limits for the plot.}
  \item{ylim}{y limits for the plot.}
  \item{zlim}{z limits for the plot.}
  \item{levels}{a set of levels which are used to partition the range
    of \code{z}.  Must be \bold{strictly} increasing (and finite).  Areas
    with \code{z} values between consecutive levels are painted with the
    same color.}
  \item{nlevels}{if \code{levels} is not specified, the range of \code{z},
    values is divided into approximately this many levels.}
  \item{color.palette}{a color palette function to be used to assign
    colors in the plot.}
  \item{col}{an explicit set of colors to be used in the plot.
    This argument overrides any palette function specification.}
  \item{plot.title}{statements which add titles the main plot.}
  \item{plot.axes}{statements which draw axes on the main plot.
    This overrides the default axes.}
  \item{key.title}{statements which add titles for the plot key.}
  \item{key.axes}{statements which draw axes on the plot key.
    This overrides the default axis.}
  \item{asp}{the \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.}
  \item{xaxs}{the x axis style.  The default is to use internal
    labeling.}
  \item{yaxs}{the y axis style.  The default is to use internal
    labeling.}
  \item{las}{the style of labeling to be used.  The default is to
   use horizontal labeling. }
  \item{smooth}{logical. If false the filled areas are not smoothed, so
   showing only, what was really measured.}
  \item{drawlines}{logical. Switches the smooth contour lines on and
   off.}
  \item{drawlabels}{logical. Contours are labelled if true}
  \item{axes, \dots}{additional graphical parameters.}
}
\description{
    The function is a modified version of \code{\link{filled.contour}},
    with an additional option to show the given values as color image
    without interpolation.
}
\references{
  Cleveland, W. S. (1993)
  \emph{Visualizing Data}.
  Summit, New Jersey: Hobart.
}
\author{Ross Ihaka. (slightly modified version by Susanne Rolinski and Thomas Petzoldt)}

\seealso{
  \code{\link{filled.contour}},
  \code{\link{contour}},
  \code{\link{image}},
  \code{\link{palette}}.}
\examples{
  data(tProfile)
  day          <- tProfile$day
  depth        <- 0:10
  tProfile$day <- NULL         # delete column
  newdat       <- NULL
  newday       <- seq(0,365,1) # Interpolation with 1 day interval

  for (i in 1:ncol(tProfile)) {
      newtemp <- approx(day, tProfile[,i], newday)$y
      newdat  <- cbind(newdat, newtemp)
  }

  slContour(x=newday,
            y=depth,
            as.matrix(newdat),
            ylim=c(max(depth),0),           # revert y-scale (depth)
            levels=seq(0,24,2),             # 0 ... +2 ... 26 deg C
            col=rev(rainbow(n=13,end=0.7)), # color scale
            xlab="Julian Day",
            ylab="Depth (m)",
            key.title=title("T (degrees C)"),
            smooth=FALSE,
            drawlines=TRUE
           )
}
\keyword{hplot}

