\name{as.simObj}
\alias{as.simObj}
%\alias{as.list}
\alias{coerce,simObj,list-method}
\alias{coerce,list,simObj-method}
\alias{as.list,simObj-method}
\alias{as.simObj,list-method}


\title{Coerce simObj Objects to Lists and Vice-Versa}
\description{
  
  These functions can be used to coerce (i.e. convert) \pkg{simecol}
  model objects (\code{simObj} objects) to ordinary lists.
  
}
\usage{
\S4method{as.simObj}{list}(x, ...)
\S4method{as.list}{simObj}(x, ...)
## alternative usage:
# as(x, "list")
# as(x, "simObj")
}

\arguments{
  
  \item{x}{object to be coerced}

  \item{\dots}{for compatibility}
}

\details{
  
  Function \code{as.list} converts the respective \code{simObj} model to
  an ordinary list with an additional element 'class' storing the class
  name of the original object.

  Function \code{as.simObj} converts in the opposite direction where the
  type of the object to be created is determined by a class name stored
  in the list element 'class'. If it is missing or contains a
  non-existing class name, an error message is printed. Additional list
  elements which are not slot names of the corresponding S4 object are
  omitted.
  
}
\seealso{
  \code{\link{new}},
  \code{\link{odeModel}},
  \code{\link[methods]{new}},
  \code{\link[methods]{as}},
  \code{\link[base]{as.list}},
  \code{\link{simecol-package}}
}
\examples{
data(lv3)
llv3 <- as(lv3, "list")
olv3 <- as(llv3, "simObj")

llv3 <- as.list(lv3)
olv3 <- as.simObj(llv3)

dput(as.list(lv3), control="useSource")
\dontrun{
## save human readable object representation
dput(as.list(lv3), file="lv3.R", control=c("all"))
## read it back and test it
l_lv3 <- dget("lv3.R")
o_lv3 <- as.simObj(l_lv3)
plot(sim(o_lv3))
}
}
\keyword{ misc }
