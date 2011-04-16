## Basic Functions for Rectangular Cellular Automata
## Details: see documentation of package simecol

eightneighbours <- function(x){
  if (!is.matrix(x))    stop("x must be a matrix")
  n <- dim(x)[1]
  m <- dim(x)[2]
  y <- rep(0, length(x))
  z <- .C("eightneighbours", as.integer(n), as.integer(m),
          as.double(x), y=as.double(y), PACKAGE="simecol")$y
  z
}

neighbours <- function(x, state = NULL, wdist = NULL, tol = 1e-4, boundaries = 0){
  if (!is.matrix(x))    stop("x must be a matrix")
  if (!is.null(state) & !is.numeric(state)) stop("state must be numeric or NULL")
  if (!is.null(wdist) & !is.numeric(wdist)) stop("wdist must be numeric or NULL")
  if (!is.numeric(tol)) stop("tol must be numeric")

  #if (length(boundaries) %% 4)
  #  warning("length of 'boundaries' argument must be either one or four")
  boundaries <- rep(boundaries, length.out = 4)
  ## pack this in an integer bit mask
  bound <- sum(boundaries * c(1L, 2L, 4L, 8L))

  n <- dim(x)[1]
  m <- dim(x)[2]

  y <- rep(0, length(x))
  ## if wdist not given do the same as eightneighbours
  if (is.null(wdist)) wdist <- matrix(c(1,1,1,1,0,1,1,1,1), nrow=3)

  ndist <- dim(wdist)[1]
  mdist <- dim(wdist)[2]
  if (mdist != ndist) stop ("wdist must be a sqare matrix")
  ## default: all nonzero states in matrix counted
  ## we simply set all nonzero states to 1 and check against 1
  if (is.null(state)) {
    state <- 1
    x[x != 0] <- 1
  }

  z <- .C("xneighbours", as.integer(n), as.integer(m),
          as.double(x), y = as.double(y),
          as.integer(ndist), as.double(wdist),
          as.double(state[1]), as.double(tol[1]),
          as.integer(bound),
          PACKAGE = "simecol")$y
  z
}

## aliases
eightneighbors <- eightneighbours
neighbors      <- neighbours
