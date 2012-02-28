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

\title{The `simecol' Package}

\description{
  
  The \pkg{simecol} package is intended to give users (scientists and
  students) an interactive environment to implement, distribute,
  simulate and document ecological and other dynamic models without the
  need to write long simulation programs. An object oriented framework
  using the S4 class system provides a consistent but still flexible
  approach to implement simulation models of different types:
  
  \itemize{
    \item differential equation (ODE, PDE) models (class \code{odeModel}),
    \item grid-oriented individual-based models (class \code{gridModel}), and
    \item particle diffusion-type models (class \code{rwalkModel}),
    \item individual-based models (class \code{indbasedModel}),
    \item other model types by deriving a user specified subclass from
    \code{simObj}.
  }
  Each simulation model is implemented as S4 object (superclass \code{simObj})
  with the following slots:
  \itemize{

    \item \code{main = function(time, init, parms, ...)}: a function
      holding the main equations of the model,
    
    \item \code{equations}: an optional non-nested list holding
      arbitrary sub-equations (sub-models) of the model. Sub-equations
      can be interdependent and can be called directly from within
      \code{main} or \code{initfunc}.
  
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
      inputs can be specified as data frame or matrix (more
      efficient). Performance optimized versions of \code{approx} (see
      \code{\link{approxTime}}) are available.
      
    \item \code{solver}: a function or a character string specifying the
      numerical algorithm used, e.g. \code{"lsoda"}, \code{"rk4"} or
      \code{"euler"} from package \code{deSolve}). In contrast to
      \code{"euler"} that can be used for difference equations
      (i.e. \code{main} returns derivatives), \code{"iterator"} is
      intended for models where main returns the new state (i.e for
      individual-based models).  It is also possible to reference own
      algorithms (solvers) that are defined in the user workspace or to
      assign solver functions directly.
      
    \item \code{observer}: optional slot which determines the data
      stored during the simulation. A user-provided \code{observer}
      function can also be used to write logging information to the
      screen or to the hard-disk, to perform run-time visualisation, or
      statistical analysis during the simulation.
      
      The \code{observer}-mechanism works only with
      \code{\link{iteration}} solvers. It is not available for
      \code{odeModel}s.
  
    \item \code{out}: this slot holds the simulation results after a
      simulation run as data frame (if the return value of \code{main}
      is a vector) or as list (otherwise). The type of data stored in
      \code{out} can be manipulated by providing a user-definded
      \code{observer} function.
  
    \item \code{initfunc}: this slot can hold an optional function which
      is called automatically when a new object is created by \code{new}
      or when it is re-initialized by \code{initialize} or \code{sim}.
    
  }

  \code{simObj} model objects should be defined and created using the
  common S4 mechanisms (\code{\link{new}}).

  Normally, a \code{simObj} object can contain all data needed to run
  simulations simply by entering the model object via \code{source()} or
  \code{data()} and then to run and plot the model with
  \code{plot(sim(obj))}.

  Accessor functions (with names identical to the slot names) are
  provided to get or set model parameters, time steps, initial values,
  inputs, the solver, the main and sub-equations, an observer or an
  initfunc and to extract the model outputs.  It is also possible to
  modify the components of the simecol objects directly, e.g. the model
  equations of a model \code{\link{lv}} with \code{lv@main}, but this is
  normally not recommended as there is no guarantee that this will work in a
  compatible way in future versions.

  Models of different type are provided as data and some more in source
  code (see directory examples).

  The examples can be used as a starting point to write own
  \code{simObj} objects and to distribute them to whomever you wish.

  The package is supplemented with several utility functions
  (e.g. \code{\link{seedfill}} or \code{\link{neighbours}}), which can
  be used independently from \code{simObj} objects.
}

\references{
  Petzoldt, T. and K. Rinke (2007) \pkg{simecol}: An Object-Oriented
  Framework for Ecological Modeling in \R. \emph{Journal of Statistical
  Software}, \bold{22}(9). URL \url{http://www.jstatsoft.org/v22/i09/}.
}

\author{
  Thomas Petzoldt \email{thomas.petzoldt@tu-dresden.de}
}

\seealso{
  \code{\link{CA}},
  \code{\link{chemostat}},
  \code{\link{conway}},
  \code{\link{diffusion}},
  \code{\link{lv}},
  \code{\link{lv3}},
  \code{\link{upca}}.
}


\examples{
## (1) Quick Start Examples ====================================================

data(lv)        # load basic Lotka-Volterra model

\dontrun{
fixParms(lv)
}
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

## (2) Define and run your own  simecol model ==========================

lv <- new("odeModel", 
  main = function (time, init, parms) {
    with(as.list(c(init, parms)), {
      dn1 <-   k1 * N1 - k2 * N1 * N2
      dn2 <- - k3 * N2 + k2 * N1 * N2
      list(c(dn1, dn2))
    })
  },
  parms  = c(k1 = 0.2, k2 = 0.2, k3 = 0.2),
  times  = c(from = 0, to = 100, by = 0.5),
  init   = c(N1 = 0.5, N2 = 1),
  solver = "lsoda"
)

lv <- sim(lv)
plot(lv)

## (3) The same in matrix notation; this allows generalization      ====
##     to multi-species interaction models with > 2 species.        ====

LVPP <- new("odeModel",
  main = function(t, n, parms) {
    with(parms, {
      dn <- r * n  + n * (A \%*\% n)
      list(c(dn))
    })
  },
  parms = list(
    # growth/death rates
    r = c(k1 = 0.2, k3 = -0.2),
    # interaction matrix
    A = matrix(c(0.0, -0.2,
                 0.2,  0.0),
                 nrow = 2, ncol = 2, byrow=TRUE)
  ),
  times  = c(from = 0, to = 100, by = 0.5),
  init   = c(N1 = 0.5, N2 = 1),
  solver = "lsoda"
)

plot(sim(LVPP))


## (4) Additional resources ============================================

\dontrun{
## open the directory with source code of demo
browseURL(paste(system.file(package="simecol"), "/demo", sep=""))

## run demo
demo(jss)

## open the directory with R sourcecode examples
browseURL(paste(system.file(package="simecol"), "/doc/examples", sep=""))

## show package vignette with introductory article
vignette("simecol-introduction")
edit(vignette("simecol-introduction"))

## Open Project Homepage
browseURL("http://www.simecol.de")
}

## How to cite package simecol in publications
citation("simecol")
}

\keyword{misc}
