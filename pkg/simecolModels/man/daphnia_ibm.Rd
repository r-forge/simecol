\name{daphnia_ibm}
\alias{daphnia_ibm}
%\docType{data}
\title{Simple Individual-Based Model of Daphnia}
\description{
  The TeFI model (Temperature-Food-Interaction) is a semi-empirical model
  that simulates population dynamics of an (with default parametrisation)
  exponentially growing Daphnia population.
}
\usage{daphnia_ibm()}

\value{S4 object according to the \code{\link[simecol]{simObj}} specification.
The object contains the following slots:
    \item{main}{The main equations of the model}
  \item{parms}{a list with the parameters of the model}
  \item{times}{Simulation time and time steps.}
  \item{inputs}{Environmental conditions}
  \item{solver}{Character string with the integration method.}
  \item{init, initfunc}{\code{init} is initialized automatically according to the
     parameter values.}
}

\details{
  To see all details, please have a look into the implementation and the
  original publications.
}

\references{
Rinke, K. and Petzoldt, T. (2003). Modelling the Effects of Temperature and
Food on Individual Growth and Reproduction of Daphnia and their
Consequences on the Population Level Limnologica, \bold{33}, 293-304.

Petzoldt, T. and K. Rinke (2007).  simecol: An Object-Oriented Framework for 
Ecological Modeling in R.  Journal of Statistical Software, 22(9), 1--31.  
URL \url{http://www.jstatsoft.org/v22/i09/}.
}


\seealso{
  \code{\link[simecol]{sim}},
  \code{\link[simecol]{parms}},
  \code{\link[simecol]{init}},
  \code{\link[simecol]{times}}.
}

\examples{
ibm <- daphnia_ibm()

times(ibm)["to"] <- 50
ibm <- sim(ibm)

observer(ibm) <- function(init) {
  print(nrow(init))
  hist(init$age, xlim=c(0,30))
  init
}

ibm <- sim(ibm)

observer(ibm) <- function(res) {
    # eggs, size, age, eggage
    number   <- nrow(res)
    meansize <- mean(res$size)
    meanage  <- mean(res$age)
    meaneggs <- mean(res$eggs)
    c(number=number, meansize=meansize,
      meanage=meanage, meaneggs=meaneggs)
}

## define a user specified plot function
setMethod("plot", c("indbasedModel", "missing"),
  function(x, y, ...) {
    o <- out(x)
    par(mfrow=c(2, 2))
    plot(o$times, o$meanage,  type="l",
      xlab="Time", ylab="Mean age (d)")
    plot(o$times, o$meaneggs, type="l",
      xlab="Time", ylab="Eggs per individual")
    plot(o$times, o$number,   type="l",
      xlab="Time", ylab="Abundance")
    plot(o$times, o$number,   type="l",
      xlab="Time", ylab="Abundance", log="y")
})


ibm <- sim(ibm)
plot(ibm)
}
\keyword{misc}
