\name{neighbours}
\alias{neighbours}
\alias{neighbors}
\title{Count Number of Neighbours in a Rectangular Cellular Grid.}

\description{This function is the base function for the simulation
  of deterministic and stochastic cellular automata on a rectangular
  grid.}
\usage{
  neighbours(x, state=NULL, wdist=NULL, tol=1e-4)
  neighbors(x, state=NULL, wdist=NULL, tol=1e-4)
}
\arguments{
  \item{x}{Matrix. The cellular grid, in which each cell can have a specific
    state value, e.g. zero (dead cell) or one (living cell) or the age
    of an individual.}
  \item{state}{A value, whose existence is checked within the
    neighbourhood of each cell.
  }
  \item{wdist}{The neighbourhood weight matrix.}
  \item{tol}{Tolerance value for the comparision of \code{state} with the
    state of each cell. If \code{tol} is a large value, then more than
    one state can be checked simultaneously.}
}
\seealso{
  \code{\link{seedfill}}, \code{\link{eightneighbours}}, \code{\link{conway}}
}
\details{
  See example for details.
}
\value{
  A matrix with the same structure as \code{x} with the weighted
  sum of the neigbours with values between \code{state - tol} and
  \code{state + tol}.
}

\examples{
    mycolors <- function(n) {
      col <- c("wheat","darkgreen")
      if (n>2) col <- c(col, heat.colors(n-2))
      col
    }

    # weight matrix for neighbourhood determination
    wdist <- matrix(c(0.5,0.5,0.5,0.5,0.5,
                      0.5,1.0,1.0,1.0,0.5,
                      0.5,1.0,1.0,1.0,0.5,
                      0.5,1.0,1.0,1.0,0.5,
                      0.5,0.5,0.5,0.5,0.5),nrow=5)

    pj <- 0.99  # survival probability of juveniles
    pa <- 0.99  # survival probability of adults
    ps <- 0.1   # survival probability of senescent
    ci <- 1.0   # "seeding constant"
    adult <- 5  # age of adolescence
    old   <- 10 # age of senescence

    ## Define a start population
    n<-80
    m<-80
    x<-rep(0, m*n)

    ## stochastic seed
    ## x[round(runif(20,1,m*n))] <- adult
    dim(x)<- c(n,m)

    ## rectangangular seed in the middle
    x[38:42,38:42] <- 5

    ## plot the start population
    image(x, col=mycolors(2))

    ## Simulation loop (hint: increase loop count)
    for (i in 1:10){

      ## rule 1: reproduction
      ## 1.1 which cells are adult? (only adults can generate)
      ad <- ifelse(x>=adult & x<old, x, 0)
      dim(ad) <- c(n,m)

      ## 1.2 how much (weighted) adult neighbours has each cell?
      nb <- neighbours(ad, wdist=wdist)

      ## 1.3 a proportion of the seeds develops juveniles
      ## simplified version, you can also use probabilities
      genprob <- nb * runif(nb) * ci
      xgen  <- ifelse(x==0 & genprob >= 1, 1, 0)

      ## rule 2: growth and survival of juveniles
      xsurvj <- ifelse(x>=1     & x < adult & runif(x) <= pj, x+1, 0)
      ## rule 2: growth and survival of adults
      xsurva <- ifelse(x>=adult & x < old   & runif(x) <= pa, x+1, 0)
      ## rule 2: growth and survival of senescent
      xsurvs <- ifelse(x>= old              & runif(x) <= ps, x+1, 0)

      ## make resulting grid of complete population
      x     <- xgen + xsurvj + xsurva + xsurvs
      dim(x)  <-c(n,m)

      ## plot resulting grid
      image(x, col=mycolors(max(x)+1), add=TRUE)
      if (max(x)==0) stop("extinction", call.=FALSE)
    }

    ## modifications:  pa<-pj<-0.9

    ## additional statistics of population structure
    ## with table, hist, mean, sd, ...
}
\keyword{array}
