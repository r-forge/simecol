\name{editParms}
\docType{methods}
\alias{editParms}
\alias{editParms-methods}
\alias{editParms,simObj-method}

\alias{editInit}
\alias{editInit-methods}
\alias{editInit,simObj-method}

\alias{editTimes}
\alias{editTimes-methods}
\alias{editTimes,simObj-method}


\title{
  Edit `parms', `init' or `times' Slot of `simecol' Objects
}
\description{
  The functions invoke an editor dialog for parameters, initial
  values or time steps of \code{simObj} objects and then assign the
  new (edited) version of \code{x} in the user's workspace. A \bold{Tcl/Tk}
  version or spreadsheet editor is displayed if possible, depending
  on the structure of the respective slot.
}
\usage{
  editParms(x)
  editTimes(x)
  editInit(x)
}
\arguments{
  \item{x}{A valid instance of the \code{simObj} class.}
}
\seealso{
  \code{\link{sEdit}}, \code{\link{simObj}},
  \code{\link{parms}}, \code{\link{times}}, \code{\link{init}},
  \code{\link[utils]{edit}}
}
\examples{
\dontrun{
require("tcltk")
data(lv)        # load basic Lotka-Volterra model
lv <- editParms(lv)
plot(sim(lv))

data(conway)    # Conway's game of life
init(conway) <- matrix(0, 10, 10)
conway <- editInit(conway) # enter some "1"
sim(conway, animate = TRUE, delay = 100)
}
}

\keyword{utilities}
