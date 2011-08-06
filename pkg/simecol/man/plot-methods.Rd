\name{plot-methods}
\docType{methods}
%\alias{plot}
\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,simObj,missing-method}
\alias{plot,odeModel,odeModel-method}
\alias{plot,odeModel,missing-method}
\alias{plot,gridModel,missing-method}
\alias{plot,rwalkModel,missing-method}
\title{Methods for Function plot in Package `simecol'}
\description{
 Methods for function \code{plot} in package \pkg{simecol}.
}

\usage{
  \S4method{plot}{simObj,missing}(x, y, \dots)
  \S4method{plot}{odeModel,missing}(x, y, \dots)
  \S4method{plot}{odeModel,odeModel}(x, y, \dots)
  \S4method{plot}{gridModel,missing}(x, y, index=1:length(x@out), delay=0, \dots)
  \S4method{plot}{rwalkModel,missing}(x, y, index=1:length(x@out), delay=0, \dots)

}

\arguments{
  \item{x}{an object of class \code{simObj},}
  \item{y}{either a second \code{odeModel} object or ignored,}
  \item{index}{index of time steps to be plotted,}
  \item{delay}{delay (in ms) between consecutive images (for
    \code{gridModel}s) or xy-plots (for \code{rwalkModel}s),
  }
  \item{...}{optional plotting parameters.}
}

\section{Methods}{
  \describe{

    \item{x = "ANY", y = "ANY"}{Generic function: see
      \code{\link[graphics]{plot}}.  }

    \item{x = "simObj", \dots}{template function, does nothing and
      only issues a warning.}

    \item{x = "odeModel", \dots}{plots time series of the state
      variables where one or more \code{odeModel} objects can be supplied.
      Optional plotting parameters are possible, too. 
      See \code{\link{plot.deSolve}} for details.}

    \item{x = "gridModel", \dots}{displays a series of images for the
      simulated grid.}

    \item{x = "rwalkModel", \dots}{displays a series of x-y plots of the
      simulated individuals.}

  }
}
\keyword{methods}
\keyword{hplot}
