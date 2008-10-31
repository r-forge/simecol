\name{daphnia_tefi}
\alias{daphnia_tefi}

\title{Daphnia Growth with Temperature and Food Interaction}
\description{
  This is the semi-empirical growth model of Daphnia galeata according to Rinke and
  Petzoldt (2003).
}
\usage{
daphnia_tefi()
}
\value{S4 object according to the \code{\link{timedepModel}} specification.
The object contains the following slots:
    \item{main}{The empirical equations for length and egg number.
      \describe{
        \item{L}{body length of individuals (mm),}
        \item{E}{potential number of eggs.}
      }
   }
  \item{parms}{a \bold{List} with parameters of the model:
    \describe{
      \item{a1          = 1.167}{parameter of the empirical growth model (mm)}
      \item{a2          = 0.573}{parameter of the empirical growth model (mg C / L)}
      \item{a3          = 1.420}{parameter of the empirical growth model (mm)}
      \item{a4          = 2.397}{parameter of the empirical growth model (d)}
      \item{b1          = 1.089e-2}{parameter of the empirical growth model (1/d)}
      \item{b2          = 0.122}{parameter of the empirical growth model (1/Celsius)}
      \item{X\_max\_slope = 23.83}{parameter of the clutch size model (number of eggs)}
      \item{K\_s\_slope   = 0.65}{parameter of the clutch size model (mg C /L)}
      \item{beta\_min    = -29.28}{parameter of the clutch size model (number of eggs)}
      \item{u\_c         = 1}{parameter of the clutch size model (1/mg C)}
      \item{L\_0\_Hall    = 0.35}{minimum individual body length of Hall's data (mm)}
      \item{L\_0         = 0.65}{minimum individual body length of our data (mm),
                                may also be adapted to other populations}
    }
  }
  \item{times}{Simulation time and time steps.}
  \item{inputs}{Environmental conditions, temperature (\code{T}, Celsius),
     and food (\code{F}, mg C/L).}
  \item{solver}{Character string with the integration method (default: 'iteration'.}
  \item{init, initfunc}{\code{init} is initialized automatically according to the
     parameter values.
     % You should call \code{initialize} if you change \code{L_0}
     % to get consistent results (see example).
  }
}

\details{
  To see all details, please have a look into the implementation and the
  original publications.
}
\references{
  Rinke, K. and Petzoldt, T. (2003). Modelling the effects of temperature and
   food on individual growth and reproduction of \textit{Daphnia} and their
   consequences on the population level. \emph{Limnologica} \bold{33}(4),
   293--304.
   
  Hall, D.J. (1964): An experimental approach to the dynamics of a natural
  population of \textit{Daphnia galeata mendotae}. \emph{Ecology} \bold{45}, 94--112.
}
\seealso{
  \code{\link[simecol]{sim}},
  \code{\link[simecol]{parms}},
  \code{\link[simecol:parms]{init}},
  \code{\link[simecol:parms]{times}}.
}
\examples{
## create model object
m <- daphnia_tefi()
## inspect parameters, start values, time steps
parms(m)
init(m)
times(m)
## simulate the model
m <- sim(m)
o <- out(m)

plot(m)

parms(m)$L0 <- 0.5
#m <- initialize(m)   # not more necessary, done automatically by sim()
plot(sim(m))
}

\keyword{ misc }