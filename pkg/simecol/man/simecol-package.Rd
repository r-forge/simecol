\name{simecol-package}
\alias{simecol-package}
\alias{simecol}
\alias{simObj}

\alias{simObj-class}
\alias{odeModel-class}
\alias{gridModel-class}
\alias{rwalkModel-class}
\alias{indbasedModel-class}

\docType{package}

\title{General Information about the `simecol' Package}

\description{
  The \pkg{simecol} package is intended to give users
  (scientists and students) an interactive environment to implement,
  distribute, simulate and document ecological and other dynamic models without
  the need to write long simulation programs. For this purpose, an object
  oriented approach using the S4 class system
  is proposed, which should provide a consistent but still
  flexible and extensible way to implement simulation models of different
  types, namely:
  \itemize{
    \item ordinary differential equation (ode) models (class \code{odeModel}),
    \item grid-oriented individual-based models (class \code{gridModel}), and
    \item particle diffusion-type models (class \code{rwalkModel}),
    \item individual-based models (class \code{indbasedModel}),
    \item other model types by deriving a user specified subclass from
    \code{simObj}.
  }
  Each simulation model is implemented as S4 object (superclass \code{simObj})
  with the following slots:
  \itemize{
    \item \code{main = function(time, init, parms, ...)}: a function holding the
      main equations of the model,
    \item \code{equations}: an optional non-nested list holding arbitrary
      sub-equations (sub-models) of the
      model. Sub-equations can be interdependent and can be called
      directly from within \code{main} or \code{initialize}.
    \item \code{parms}: a list (or vector for some classes) with 
      constant model parameters,
    \item  \code{times}: vector of time steps or vector with three
      named values \code{from}, \code{to}, \code{by} specifying the
      simulation time steps. The from-to-by form can be edited with
     \code{fixParms}.
    \item \code{init}: initial state (start values) of the
      simulation. This is typically a named vector (state variables in
      \code{odeModel}s) or matrix (e.g. initial grid of
      \code{gridModel}s).
    \item \code{inputs}: time dependend or spatially resolved external
      inputs can be specified as data frame or matrix (more efficient). It
      is up to the user to evaluate the corresponding values within the
      \code{main} function. Performance optimized versions of
      \code{approx} (see \code{\link{approxTime}}) are available.
     \item \code{solver}: character string specifying the numerical algorithm
       used, e.g. \code{"lsoda"}, \code{"rk4"} or \code{"euler"} from package 
       \code{deSolve}). In addition, solvers from package
        \code{"ddesolve"} can also be used.
        In contrast to \code{"euler"} that can be used for difference equations 
        (i.e. \code{main} returns derivatives), \code{"iterator"} is intended for
        models where main returns the new state (i.e for individual-based models).
        It is also possible to reference own algorithms (solvers) that are defined
        in the useer workspace or to assign solver functions directly.
     \item \code{observer}: optional slot which determines the data stored
       during the simulation. A user-provided \code{observer} function can also
       be used to write logging information to the screen or to the hard-disk,
       to perform run-time visualisation, or statistical analysis during the
       simulation.\cr
       Note that the \code{observer}-mechanism works only with
       \code{\link{iteration}} solvers. It is currently not supported by
       \code{odeModel}s and \code{gridModel}s.
     \item \code{out}: this slot holds the simulation results after a
       simulation run as data frame (if the return value of \code{main} is a vector)
       or as list (otherwise). The type of data stored in \code{out} can be
       manipulated by providing a user-definded \code{observer} function.
     \item \code{initfunc}: this slot can hold an optional function which is called
       automatically when a new object is created by \code{new} or when it
       is re-initialized by \code{initialize}.
  }  

  \code{simObj} model objects should be defined and created using the
  common S4 mechanisms (\code{\link{new}}).

  In this way, a \code{simObj} object may contain all data, which are
  needed to run simulations simply by entering the model object
  via \code{source()} or \code{data()} and then to run and plot the
  model for example via \code{plot(sim(obj))}.

  Accessor functions (with names identical to the slot names) are
  provided to get or set model parameters,
  time steps, initial values, inputs, the solver, the main and
  sub-equations or initfunc or to extract the model outputs.
  It is also possible to modify the components of the simecol objects
  directly, e.g. the model equations of a model \code{\link{lv}} with
  \code{lv@main}, but there is no guarantee that this will work in a compatible
  way in future versions.

  Models of different type are provided as data and some more in source code
  (see directory examples). 

  The examples are intended as a starting point to
  write your own \code{simObj} objects and to distribute them to whomever you wish.

  The package is supplemented with several utility functions
  (e.g. \code{\link{seedfill}} or \code{\link{neighbours}}), which can
  be used independently from \code{simObj} objects.
}



\references{
  Petzoldt, T and K. Rinke (2007), \pkg{simecol}: An Object-Oriented Framework for
    Ecological Modeling in \R. Journal of Statistical Software,
    22(9). URL \url{http://www.jstatsoft.org/v22/i09/}.
}


\author{
  Thomas Petzoldt \email{thomas.petzoldt@tu-dresden.de}
}

\examples{
## (1) Quick Start Examples ====================================================
data(lv)        # load basic Lotka-Volterra model

\dontrun{fixParms(lv)}
parms(lv)
main(lv)
lv <- sim(lv)
plot(lv)
results <- out(lv)

\dontrun{
data(conway)    # Conway's game of life
init(conway) <- matrix(0, 10, 10)
times(conway) <-  1:100
fixInit(conway) # enter some "1"
sim(conway, animate=TRUE, delay=100)
}

## (2) Additional Resources ====================================================
\dontrun{
## open the directory with source code of demo
browseURL(paste(system.file(package="simecol"), "/demo", sep=""))

## run demo
demo(jss)

## open the directory with R sourcecode examples
browseURL(paste(system.file(package="simecol"), "/examples", sep=""))

## show package vignette with introductory article
vignette("simecol-introduction")
edit(vignette("simecol-introduction"))

## open the installation directory of simecol
##   (NEWS, THANKS, FAQ, ToDo, WISHLIST, ...)
browseURL(paste(system.file(package="simecol"), "/", sep=""))


## Open Project Homepage
browseURL("http://www.simecol.de")
}


}

\keyword{misc}
