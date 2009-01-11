\name{lv}
\docType{data}
\alias{lv}
\title{Lotka-Volterra Predator-Prey Model}
\description{
  \pkg{simecol} example: basic Lotka-Volterra predator prey-model.
}
\usage{data(lv)}
\format{
  An S4 object according to the \code{\link{odeModel}} specification.
  The object contains the following slots:

  \describe{
    \item{\code{main}}{Lotka-Volterra equations for predator and prey.}
    \item{\code{parms}}{Vector with the named parameters of the model:
      \describe{
        \item{\code{k1}}{growth rate of the prey population,}
        \item{\code{k2}}{encounter rate of predator and prey,}
        \item{\code{k3}}{death rate of the predator population.}
      }
    }
    \item{\code{times}}{Simulation time and integration interval.}
    \item{\code{init}}{Vector with start values for predator and prey.}
  }
}

\details{To see all details, please have a look into the implementation.
}


\references{
Lotka, A. J. 1925. \emph{Elements of physical biology.} Williams and Wilkins, Baltimore.

Volterra, V. (1926). Variazionie fluttuazioni del numero d'individui in specie animali conviventi.
\emph{Mem. Acad.Lincei}, \bold{2}, 31-113.
}

\seealso{
  \code{\link{simecol-package}},
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
##============================================
## Basic Usage:
##   explore the example
##============================================
data(lv)
print(lv)
plot(sim(lv))

parms(lv) <- c(k1=0.5, k2=0.5, k3=0.5)
plot(sim(lv))

##============================================
## Implementation:
##   The code of the Lotka-Volterra-model
##============================================
lv <- new("odeModel",
  main = function (time, init, parms) {
    x <- init
    p <- parms
    dx1 <-   p["k1"] * x[1] - p["k2"] * x[1] * x[2]
    dx2 <- - p["k3"] * x[2] + p["k2"] * x[1] * x[2]
    list(c(dx1, dx2))
  },
  parms  = c(k1=0.2, k2=0.2, k3=0.2),
  times  = c(from=0, to=100, by=0.5),
  init   = c(prey=0.5, predator=1),
  solver = "rk4"
)

}

\keyword{datasets}
