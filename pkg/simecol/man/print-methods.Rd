\name{print-methods}
\docType{methods}
%\alias{print}
\alias{print-methods}
\alias{print,ANY-method}
\alias{print,simObj-method}
\alias{show,simObj-method}
\title{Methods for Function `print' in Package `simecol'}
\description{
 Methods for function \code{print} in Package \pkg{simecol}.
}
\usage{
  \S4method{print}{simObj}(x, all = FALSE, ...)
}
\arguments{
  \item{x}{An object of class \code{simObj} or one of its subclasses.}
  \item{all}{Specifies whether all slots are printed. Default is that
    only not-empty slots are printed and the contents of \code{out} are suppressed.}
  \item{...}{Optional parameters passed to print.}
}
\section{Methods}{
  \describe{
    \item{x = "ANY"}{Generic function: see \code{\link[base]{print}}.}
    \item{x = "simObj"}{prints the contents (slots) of the \code{simObj} object.}
  }
}
\keyword{methods}
\keyword{print}
