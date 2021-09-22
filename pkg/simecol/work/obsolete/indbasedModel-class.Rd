\name{indbasedModel-class}
\docType{class}
\alias{indbasedModel-class}

\alias{observer}
\alias{observer-methods}
\alias{observer,indbasedModel-method}

\alias{observer<-}
\alias{observer<--methods}
\alias{observer<-,indbasedModel-method}

\alias{iteration}
\alias{iteration-methods}
\alias{iteration,indbasedModel-method}


\title{Class "indbasedModel"}
\description{This class extends class \code{simObj}. It is intended for individual-based
models where the state (\code{init}-slot) is represented by a data frame and the parameters
are represented as list.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("indbasedModel", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{parms}:}{Object of class \code{"list"}}
    \item{\code{init}:}{Object of class \code{"list"} or \code{"data.frame"}}
    \item{\code{main}:}{Object of class \code{"function"}}
    \item{\code{equations}:}{Object of class \code{"listOrNULL"}}
    \item{\code{times}:}{Object of class \code{"numeric"}}
    \item{\code{inputs}:}{Object of class \code{"ANY"}}
    \item{\code{solver}:}{Object of class \code{"character"}}
    \item{\code{observer}:}{Object of class \code{"functionOrNULL"}}
    \item{\code{out}:}{Object of class \code{"ANY"}}
    \item{\code{initfunc}:}{Object of class \code{"functionOrNULL"}}
  }
}
\details{
The optional \code{observer}-slot is intended as a flexible means to provide
user-definded functions which are called in each \code{\link[simecol]{iteration}} step. 
Their main task is to define a way to compute or collected the required outputs during
the simulation. It is also possible to use this mechanism for animation or
debugging, see \code{\link{daphnia_ibm}} for examples.
}
%\section{Extends}{
%Class \code{"\linkS4class{simObj}"}, directly.
%}
\references{
  Petzoldt, T. and K. Rinke (2007).  simecol: An
  Object-Oriented Framework for Ecological
  Modeling in R.  \emph{Journal of Statistical Software}, \bold{22}(9), 1--31.
  URL \url{https://www.jstatsoft.org/v22/i09/}.

}

\seealso{
	\code{\link[simecol]{iteration}}, \code{\link{daphnia_ibm}}
}
\examples{
showClass("indbasedModel")
}
\keyword{classes}
