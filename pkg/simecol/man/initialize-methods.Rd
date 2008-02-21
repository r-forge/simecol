\name{initialize-methods}
\docType{methods}
\alias{initialize-methods}
%\alias{initialize}
\alias{initialize,simObj-method}

\title{Methods for Function `initialize' in Package `simecol'}

\description{
  This function is used to initialize objects derived from the \code{simObj}
  superclass.
}
\usage{
  \S4method{initialize}{simObj}(.Object, \dots)
}

\arguments{
  \item{.Object}{The \code{simObj} instance which is to be re-initialized.}
  \item{\dots}{Provided for compatibility with the default method of
    \code{initialize}, or slots of the object which is to be
    created (in case of \code{\link[methods]{new}})}
}

\section{Methods}{
  \describe{
    \item{.Object = "ANY"}{Generic function: see \code{\link[methods]{new}}.}
    \item{.Object = "simObj"}{
      The \code{initialize} function is normally called implicitly by \code{new}
      to create new objects. It may also be called explicitly to return a cloned
      and re-initialized object.
      The \pkg{simecol} version of \code{initialize} provides an additonal mechanism
      to call a user specified function provided in the \code{initfun} slot
      of a \code{simObj} instance that can perform computations during the
      object creation process. The \code{initfunc} must have \code{obj} as its
      only argument and must return the modified version of this \code{obj},
      see examples below.
    }
  }
}

\seealso{
\code{\link{simObj}}, \code{\link[methods]{new}}

}
\examples{
    # Note: new calls initialize and initialize calls initfunc(obj)
    lv_efr <- new("odeModel",
      main = function (time, init, parms, ...) {
        x <- init
        p <- parms
        S <- approxTime1(inputs, time, rule=2)["s.in"]
        dx1 <-   S * p["k1"] * x[1] - p["k2"] * x[1] * x[2]
        dx2 <-     - p["k3"] * x[2] + p["k2"] * x[1] * x[2]
        list(c(dx1, dx2))
      },
      parms  = c(k1=0.2, k2=0.2, k3=0.2),
      times  = c(from=0, to=100, by=0.5),
      init   = c(prey=0.5, predator=1),
      solver = "rk4",
      initfunc = function(obj) {
        tt <- fromtoby(times(obj))
        inputs(obj) <- as.matrix(data.frame(
                time = tt,
                s.in = pmax(rnorm(tt, mean=1, sd=0.5), 0)
              ))
        obj
      }
    )

    plot(sim(lv_efr))           # first call
    plot(sim(lv_efr))           # same figure with identical object

    lv_efr<- initialize(lv_efr) # re-initializes, new random inputs
    plot(sim(lv_efr))           # different figure
}
\keyword{methods}


