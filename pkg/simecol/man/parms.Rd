\name{parms}

\alias{main}
\alias{main-methods}
\alias{main,simObj-method}

\alias{main<-}
\alias{main<--methods}
\alias{main<-,simObj-method}

\alias{equations}
\alias{equations-methods}
\alias{equations,simObj-method}

\alias{equations<-}
\alias{equations<--methods}
\alias{equations<-,simObj-method}


\alias{parms}
\alias{parms-methods}
\alias{parms,simObj-method}

\alias{parms<-}
\alias{parms<--methods}
\alias{parms<-,simObj-method}

\alias{init}
\alias{init-methods}
\alias{init,simObj-method}

\alias{init<-}
\alias{init<--methods}
\alias{init<-,simObj,ANY-method}
\alias{init<-,gridModel,matrix-method}
\alias{init<-,gridModel,ANY-method}

\alias{times}
\alias{times-methods}
\alias{times,simObj-method}

\alias{times<-}
\alias{times<--methods}
\alias{times<-,simObj-method}

\alias{inputs}
\alias{inputs-methods}
\alias{inputs,simObj-method}

\alias{inputs<-}
\alias{inputs<--methods}
\alias{inputs<-,simObj-method}

\alias{solver}
\alias{solver-methods}
\alias{solver,simObj-method}

\alias{solver<-}
\alias{solver<--methods}
\alias{solver<-,simObj-method}

\alias{initfunc}
\alias{initfunc-methods}
\alias{initfunc,simObj-method}

\alias{initfunc<-}
\alias{initfunc<--methods}
\alias{initfunc<-,simObj-method}

\alias{out}
\alias{out-methods}
\alias{out,simObj-method}
\alias{out,gridModel-method}
\alias{out,odeModel-method}

\alias{out<-}
\alias{out<--methods}
\alias{out<-,simObj-method}

\title{Accessor Functions for `simObj' Objects}
\description{
  Get or set simulation model parameters, main or sub-equations, initial
  values, time steps or solvers and extract simulation results.
}
\usage{
   parms(obj, ...)
   parms(obj) <- value

   main(obj, ...)
   main(obj) <- value

   equations(obj, ...)
   equations(obj) <- value

   init(obj, ...)
   init(obj) <- value

   inputs(obj, ...)
   inputs(obj) <- value

   times(obj, ...)
   times(obj) <- value

   solver(obj, ...)
   solver(obj) <- value
   
   initfunc(obj, ...)
   initfunc(obj) <- value

   out(obj, ...)
   out(obj) <- NULL
}
\arguments{
  \item{obj}{A valid \code{simObj} instance.}
  \item{value}{Named list, vector, function or other data structure (depending
    on the slot and model class) with the same structure as the value returned by 
    \code{parms}. Either all or a subset of values (e.g. single
    elements of vectors or lists) can be changed at once.}
  \item{...}{Reserved for method consistency.}
}
% argument last is not yet documented

\details{These are the accessing functions for \code{parms},
  \code{times} etc.
  Please take care of the semantics of your model when changing slots.
  All, element names, data structure and values have to correspond to you
  model object definition. For example in \code{init} the
  applied names must exactly correspond to the names and number (!) of state
  variables. The restrictions of \code{parms} or \code{equations} are less strict
  (additional values for ``future use'' are allowed).
  
  The function \code{times} allows either to assign or to modify a special
  vector with three elements named \code{from}, \code{to} and \code{by} or to
  overwrite \code{times} with an un-named sequence (e.g.
  \code{seq(1, 100, 0.1)}.
  
  To ensure object consistency function \code{out} cannot assign arbitrary values.
  It can only extract or delete the contents (by assigning \code{NULL}) of the 
  \code{out}-slot.
}

\value{A list, named vector, matrix or function (for
  \code{main} slot) or list of functions (\code{equation} slot) or other
  appropriate data structure depending on the class of the model object.
}

\seealso{
  \code{\link{simecol-package}}.
}
\examples{
    data(lv)
    parms(lv)
    parms(lv)       <- c(k1=0.2, k2=0.5, k3=0.3)
    parms(lv)["k2"] <- 0.5

    data(conway)
    parms(conway)
    parms(conway)$srv <- c(2, 2)
    parms(conway)

    # add a new named parameter value
    parms(lv)["dummy"] <- 1
    # remove dummy parameter
    parms(lv) <- parms(lv)[names(parms(lv)) != "dummy"]
    
    # simulation and extraction of outputs
    lv <- sim(lv)
    o <- out(lv)
    
    # remove outputs from object
    out(lv) <- NULL
    # store object persistently to the disk
    save(lv, file="lv.Rdata")           # in binary form
    dput(as.list(lv), file="lv-list.R") # in text form
}
\keyword{programming}
