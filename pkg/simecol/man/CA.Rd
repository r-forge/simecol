\name{CA}
\docType{data}
\alias{CA}
\title{Stochastic Cellular Automaton}
\description{
  
  \pkg{simecol} example: This model simulates a stochastic cellular
  automaton.
  
}
\usage{data(conway)}

\format{
  An S4 object according to the \code{\link{gridModel}} specification.
  The object contains the following slots:

  \describe{
    \item{\code{main}}{functions with the state transition rules of
      Coway's Game of Life.
    }
    \item{\code{parms}}{a list with two vector elements:
      \describe{
        \item{\code{pbirth}}{probability of birth,}
        \item{\code{pdeath}}{death probability, dependend on neighbors.}
      }
    }
    \item{\code{times}}{number of time steps to be simulated.}
    \item{\code{init}}{a matrix, giving the initial state of the
      cellular grid (default: rectangle in the middle of the grid).
    }
  } 
}

\details{
  To see all details, please have a look into the implementation below.
}
\seealso{
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
##============================================
## Basic Usage:
##   work with the example
##============================================
data(CA)
times(CA)["to"] <- 10
plot(sim(CA))

set.seed(345)
times(CA)["to"] <- 50
CA <- sim(CA)

library(lattice)
tcol <- (terrain.colors(13))[-13]
x <- out(CA, last=TRUE)
x <- ifelse(x == 0, NA, x)
levelplot(x,
  cuts = 11,
  col.regions = tcol,
  colorkey = list(at = seq(0, 55, 5))
)

##============================================
## Implementation:
##   The code of the CA model
##============================================
CA <- new("gridModel",
  main = function(time, init, parms) {
    z     <- init
    nb    <- eightneighbors(z)
    pgen  <- 1 - (1 - parms$pbirth)^nb
    zgen  <- ifelse(z == 0 &
               runif(z) < pgen, 1, 0)
    zsurv <- ifelse(z >= 1 &
               runif(z) < (1 - parms$pdeath),
               z + 1, 0)
    zgen + zsurv
  },
  parms = list(pbirth = 0.02, pdeath = 0.01),
  times = c(from = 1, to = 50, by = 1),
  init = matrix(0, nrow = 40, ncol = 40),
  solver = "iteration"
)
init(CA)[18:22,18:22] <- 1
##============================================
}

\keyword{datasets}
