\name{conway}
\docType{data}
\alias{conway}
\title{The Classical Coway's Game of Life}
\description{
  \pkg{simecol} example: This model simulates a deterministic cellular
  automaton.
}
\usage{data(conway)}

\format{

  An S4 object according to the \code{\link{gridModel}} specification.
  The object contains the following slots:

  \describe{
    \item{\code{main}}{functions with the state transition rules.}
    \item{\code{parms}}{A list with two vector elements:
      \describe{
        \item{\code{srv}}{number of neighbours, necessary to survive,}
	\item{\code{gen}}{number of neighbours, necessary to generate a
	  new cell.
	}
      }
    }
    \item{\code{times}}{number of time steps to be simulated,}
    \item{\code{init}}{matrix with the initial state of the cellular
      grid (default: random).  }
    
  } 
}

\details{
  To see all details, please have a look into the implementation below.
}

\references{
  Gardner, Martin (1970) The Fantastic Combinations of John Conway's New
  Solitaire Game 'Life.' \emph{Scientific American}, October 1970.
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
##   explore the example
##============================================
data(conway)
plot(sim(conway))

## more interesting start conditions
m <- matrix(0, 40, 40)
m[5:35, 19:21] <- 1
init(conway) <- m
plot(sim(conway), col=c("white", "green"), axes = FALSE)

## change survival rules
parms(conway) <- list(srv = c(3,4), gen = c(3, 4))
plot(sim(conway), col = c("white", "green"), axes = FALSE)
\dontrun{
init(conway) <- matrix(0, 10, 10)
fixInit(conway) # enter some "1"
sim(conway, animate = TRUE, delay = 100)

##============================================
## Implementation:
##   The code of Conways Game of Life
##============================================
conway <- new("gridModel",
  main = function(time, init, parms) {
    x      <- init
    nb     <- eightneighbours(x)
    surviv <- (x >  0 & (nb \%in\% parms$srv))
    gener  <- (x == 0 & (nb \%in\% parms$gen))
    x      <- (surviv + gener) > 0
    return(x)
  },
  parms  = list(srv = c(2, 3), gen = 3),
  times  = 1:17,
  init   = matrix(round(runif(1000)), ncol = 40),
  solver = "iteration"
)
}
}

\keyword{datasets}
