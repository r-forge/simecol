\name{CA}
\docType{data}
\alias{CA}
\title{Stochastic Cellular Automaton}
\description{
  \pkg{simecol} example: This model simulates a stochastic cellular automaton.
}
\usage{data(conway)}

\format{An S4 object according to the \code{\link{gridModel}} specification. 
The object contains the following slots:

\describe{
  \item{\code{main}}{Functions with the state transition rules of Coway's Game of Life.}
  \item{\code{parms}}{A list with two vector elements:
       \describe{
          \item{\code{pbirth}}{probability of birth,}
          \item{\code{pdeath}}{death probability, dependend on neighbors}
       }}
  \item{\code{times}}{Number of time steps to be simulated.}
  \item{\code{init}}{A matrix, giving the initial state of the cellular grid
     (default: rectangle in the middle of the grid).}
} 
}

\details{To see all details, please have a look into the implementation.
}


\seealso{
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
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
    print(levelplot(x,
                 cuts = 11,
                 col.regions = tcol,
                 colorkey=list(at=seq(0,55,5))
    ))
}

\keyword{datasets}
