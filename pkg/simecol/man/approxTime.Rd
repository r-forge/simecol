\name{approxTime}
\alias{approxTime}
\alias{approxTime1}
\title{Linear Interpolation with Complete Matrices or Data Frames}
\description{

  Return a data frame, matrix or vector which linearly interpolates data
  from a given matrix or data frame.
  
}
\usage{
approxTime(x, xout, ...)
approxTime1(x, xout, rule = 1)
}
\arguments{

  \item{x}{a matrix or data frame with numerical values giving
    coordinates of points to be interpolated. The first column is
    interpreted as independend variable (e.g. time), the remaining
    columns as dependend variables.  }
  
  \item{xout}{a vector (or single value for \code{approxTime1}) of
    independend values specifying where interpolation has to be done.  }
  
  \item{rule}{an integer describing how interpolation is to take place
    outside the interval [min(x), max(x)]. If \code{rule} is 1 then
    \code{NA}s are returned for such points and if it is 2, the value at
    the closest data extreme is used.  }
  
  \item{...}{optional parameters passed to \code{approx}.}
  
}
\value{
  \code{approxTime} returns a matrix resp. data frame of the same
  structure as \code{x} containing data which interpolate the given data
  with respect to \code{xout}.  \code{approxTime1} is a performance
  optimized special version with less options than the original
  \code{approx} function. It returns an interpolated vector.
}
\details{
  The functions can be used for linear interpolation with a complete
  matrix or data frame. This can be used for example in the main
  function of an \code{odeModel} to get input values at a specified time
  \code{xout}. The version \code{approxTime1} is less flexible (only one
  single value for xout and only linear interpolation) but has increased
  performance.  Both functions are faster if \code{x} is a matrix
  instead of a data frame.
}

\seealso{
  \code{\link[stats]{approxfun}}
}

\examples{
inputs <- data.frame(time = 1:10, y1 = rnorm(10), y2 = rnorm(10, mean = 50))
input  <- approxTime(inputs, c(2.5, 3), rule = 2)
}
\keyword{arith}
