\name{conway}
\docType{data}
\alias{conway}
\title{The Classical Coway's Game of Life}
\description{
  \pkg{simecol} example: This model simulates a deterministic cellular automaton.
}
\usage{data(conway)}

\format{An S4 object according to the \code{\link{gridModel}} specification. 
The object contains the following slots:

\describe{
  \item{\code{main}}{Functions with the state transition rules of Coway's Game of Life.}
  \item{\code{parms}}{A list with two vector elements:
       \describe{
          \item{\code{srv}}{number of neighbours, necessary to survive,}
          \item{\code{gen}}{number of neighbours, necessary to generate a new cell.}
       }}
  \item{\code{times}}{Number of time steps to be simulated.}
  \item{\code{init}}{A matrix, giving the initial state of the cellular grid (default: random).}
} 
}

\details{To see all details, please have a look into the implementation.
}

\references{
Gardner, Martin (1970) The Fantastic Combinations of John Conway's 
New Solitaire Game 'Life.' \emph{Scientific American}, October 1970.
}

\seealso{
  \code{\link{sim}},
  \code{\link{parms}},
  \code{\link{init}},
  \code{\link{times}}.
}


\examples{
    data(conway)
    plot(sim(conway))

    # more interesting start conditions
    m <- matrix(0, 40, 40)
    m[5:35,19:21] <-1
    init(conway) <- m
    plot(sim(conway), col=c("white", "green"), axes=FALSE)

    # change survival rules
    parms(conway) <- list(srv=c(3,4), gen=c(3,4))
    plot(sim(conway), col=c("white", "green"), axes=FALSE)
    \dontrun{
    init(conway) <- matrix(0, 10, 10)
    fixInit(conway) # enter some "1"
    sim(conway, animate=TRUE, delay=100)
    }
}

\keyword{datasets}
