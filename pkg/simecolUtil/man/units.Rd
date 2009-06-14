\name{units}
\alias{units}
\docType{data}
\title{Common limnological measurement units for axis labels}
\description{
  This list contains several typical axes titles,
  used in limnological applications.
}
\usage{data(units)}
\details{
  The list is meant as extensible starting point.
}

\examples{
data(units)
str(units)
x <- as.Date("2006-01-15") + seq(0, 360, 14)
y <- runif(x, 0, 10)
plot(x, y, xlab = "time", ylab = units$phyto)

}
\keyword{datasets}
