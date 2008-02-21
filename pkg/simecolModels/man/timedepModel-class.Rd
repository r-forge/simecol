\name{timedepModel-class}
\docType{class}
\alias{timedepModel-class}
\alias{timedepModel}

\title{Class "timedepModel"}
\description{This class extends class \code{simObj}. It is intended for analytically
solved ODE models or empirical regression-type models over time.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("timedepModel", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{parms}:}{Object of class \code{"numericOrlist"}}
    \item{\code{init}:}{Object of class \code{"data.frame"}}
    \item{\code{main}:}{Object of class \code{"function"}}
    \item{\code{equations}:}{Object of class \code{"listOrNULL"}}
    \item{\code{times}:}{Object of class \code{"numeric"}}
    \item{\code{inputs}:}{Object of class \code{"ANY"}}
    \item{\code{solver}:}{Object of class \code{"character"}}
    \item{\code{out}:}{Object of class \code{"ANY"}}
    \item{\code{initfunc}:}{Object of class \code{"functionOrNULL"}}
  }
}
\details{
  The natural solver method applicable for this type of models is
    \code{\link[simecol]{iteration}}.
}
%\section{Extends}{
%Class \code{"\linkS4class{simObj}"}, directly.
%}
\references{
  Petzoldt, T. and K. Rinke (2007).  simecol: An
  Object-Oriented Framework for Ecological
  Modeling in R.  \emph{Journal of Statistical Software}, \bold{22}(9), 1--31.
  URL \url{http://www.jstatsoft.org/v22/i09/}.

}

\seealso{
	\code{\link{daphnia_tefi}}
}
\examples{
showClass("timedepModel")
}
\keyword{classes}
