\name{fixParms}
\docType{methods}
\alias{fixParms}
\alias{fixParms-methods}
\alias{fixParms,simObj-method}

\alias{fixInit}
\alias{fixInit-methods}
\alias{fixInit,simObj-method}

\alias{fixTimes}
\alias{fixTimes-methods}
\alias{fixTimes,simObj-method}


\title{
  Fix `parms', `init' or '`times' Slot of `simecol' Objects
}
\description{
  The functions invoke an editor dialog for parameters, initial
  values or time steps of \code{simObj} objects and then assign the
  new (edited) version of \code{x} in the user's workspace. A \bold{Tcl/Tk}
  version or spreadsheet editor is displayed if possible depending
  on the structure of the respective slot.
}
\usage{
  fixParms(x)
  fixTimes(x)
  fixInit(x)
}
\arguments{
  \item{x}{A valid instance of the \code{simObj} class.}
}
\seealso{
  \code{\link{sEdit}}, \code{\link{simObj}},
  \code{\link{parms}}, \code{\link{times}}, \code{\link{init}},
  \code{\link[utils]{fix}}
}
\examples{
\dontrun{
data(lv)        # load basic Lotka-Volterra model
fixParms(lv) 
plot(sim(lv))

data(conway)    # Conway's game of life
init(conway) <- matrix(0, 10, 10)
fixInit(conway) # enter some "1"
sim(conway, animate=TRUE, delay=100)
}
}

\keyword{utilities}
