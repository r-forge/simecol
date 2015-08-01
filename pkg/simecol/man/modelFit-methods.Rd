\docType{methods}
\name{modelFit-method}
\alias{$}
\alias{[}
\alias{[[}
\alias{$,modelFit-method}
\alias{[,modelFit-method}
\alias{[[,modelFit-method}
\alias{coef,modelFit-method}
\alias{deviance,modelFit-method}
\alias{summary,modelFit-method}
\title{Show Results of Model Fits}
\usage{

\S4method{coef}{modelFit}(object, ...)

\S4method{deviance}{modelFit}(object, ...)

\S4method{summary}{modelFit}(object, ...)

\S4method{$}{modelFit}(x, name)

\S4method{[}{modelFit}(x, i, j, ..., drop=TRUE)

\S4method{[[}{modelFit}(x, i, j, ...)
}

\arguments{
\item{object, x}{'modelFit' object from which to extract element(s).}

\item{i, j}{indices specifying elements to extract. Indices are numeric or character vectors or empty (missing) or NULL.}

\item{name}{a literal character string or a name (possibly backtick quoted). For extraction, this is normally partially matched to the names of the object.}

\item{drop}{For matrices and arrays. If TRUE the result is coerced to the lowest possible dimension.}


\item{\dots}{other arguments pased to the methods}
}
\description{
Functions to access the results of parameter fits.
}

\seealso{
\code{\link{fitOdeModel}}, \code{\link{Extract}}
}

