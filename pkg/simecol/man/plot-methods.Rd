\name{plot-methods}
\docType{methods}
%\alias{plot}
\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,simObj,missing-method}
\alias{plot,odeModel,missing-method}
\alias{plot,gridModel,missing-method}
\alias{plot,rwalkModel,missing-method}
\title{Methods for Function plot in Package `simecol'}
\description{
 Methods for function \code{plot} in package \pkg{simecol}.
}

\usage{
  \S4method{plot}{simObj,missing}(x, index=1:length(x@out), delay=0, \dots)
}

\arguments{
  \item{x}{an object of class \code{simObj},}
%  \item{y}{omitted.}
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

    \item{x = "simObj", \dots}{returns a warning that a specific method is
      required, depending on the actual data structure.}

    \item{x = "odeModel", \dots}{plots time series of the state
      variables. Optional plotting parameters are possible.}

    \item{x = "gridModel", \dots}{displays a series of images for the
      simulated grid.}

    \item{x = "rwalkModel", \dots}{displays a series of x-y plots of the
      simulated individuals.}

  }
}
\keyword{methods}
\keyword{hplot}
