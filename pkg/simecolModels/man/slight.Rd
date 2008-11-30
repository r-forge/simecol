\encoding{latin1}
\name{slight}
\alias{slight}

\title{SALMO-Light: an absolutely simplistic lake model}
\description{
  This model is an extremely cut-down version of the ecological lake model
  SALMO of Benndorf (1979).
  The small demo was inspired by the simplicity of the AQUAMOD-I model
  by Straskraba and Gnauck (1983) and is intended solely for teaching purposes.
}
\usage{
slight()
}

\value{S4 object according to the \code{\link[simecol]{odeModel}} specification.
The object contains the following slots:
  \item{main}{The differential equations for two phytoplankton groups,
    zooplankton and phosporus:
    \describe{
      \item{phyto}{biovolume of phytoplankton (Chlorophyll, \eqn{\rm \mu g\, L^{-1}}{mu g/L}),}
      \item{zoo}{biovolume of zooplankton (Carbon, \eqn{\rm \mu g\, L^{-1}}{mu g/L}),}
      \item{phos}{phosphorus concentration (P, \eqn{\rm \mu g\, L^{-1}}{mu g/L}),}
    }
  }
  \item{parms}{Vector with the named parameters of the model, see \code{parms}},
  \item{times}{Simulation time and integration interval.}
  \item{init}{Vector with start values for the state variables.}
  \item{solver}{Character string with the integration method.}
  \item{parms}{
    \describe{
      \item{zin =}{0, zooplankton-import (carbon, \eqn{\rm \mu g\, L^{-1}}{mu g/L})}
      \item{kxg =}{0.25, half sat. ingest. zooplankton (SALMO, converted to carbon)}
      \item{gmax =}{0.8, max. ingestion of zooplankton mean value between min (0.26) 
        and max (1.3) (SALMO), temperature dependence ignored}
      \item{ae =}{0.6, assimilation efficiency}
      \item{ep =}{0.2, extinction of plankton free water (1/m) (lake specific)}
      \item{epx =}{0.425,  specific extinktion of phytoplankton carbon (\eqn{\rm m^2 mg^{-1}}{m^2/mg}) 
        Lampert & Sommer, converted from chlorophyll to carbon (* 500/20)}
      \item{ki =}{30, half saturation const light (\eqn{\rm J\, cm^{-2} d^{-1})}{J/(cm^2*d)}) (SALMO, rounded)}
      \item{kp =}{1.7, half saturation constant for phosphorus (\eqn{\rm mg\, m^{-3}}{mg/m^3}) (SALMO, Diatoms)}
      \item{kx =}{0.125,  strongly simplified from (SALMO/C) (value of kxmin)}
      \item{photxmax =}{1.8, maximum photosynthesis rate (1/d) (SALMO)}
      \item{photxmin =}{0.17, minimum photosynthesis rate (1/d) (SALMO)}
      \item{cp =}{0.04, C:P ratio \eqn{\rm mg\, \mu g^{-1}}{mg/micro g}, (Redfield, SALMO Diatoms)}
      \item{toptx =}{20, optimal temperature for photosynthesis (degrees C) (SALMO Diatoms)}
      \item{vs =}{0.1, sedimentation velocity (m/d) (SALMO Diaomst)}
      \item{rxtopt =}{0.06, resp rate 1/d at opt temp (SALMO, Diatoms)}
      \item{rxtmin =}{0.02, T dependence of resp (1/degrees C) (SALMO, Diatoms)}
      \item{rxmf =}{0.3, light dpendend part of phytoplankton respiration (SALMO)}
      \item{kmo =}{0.0175, half saturation constant of zooplankton mortality (carbon, mg/L) (SALMO, converted to carbon)}
      \item{momin =}{0.015, Zooplankton mortality near 0 degrees C (1/d) (SALMO)}
      \item{mot =}{0.006, temperature dependence of zooplankton mortality (degrees C/d) (SALMO)}
      \item{rz =}{0.2, respiration of zooplankton (20\% of grazing), assumption}
      \item{dtb =}{0.3720601,  inverse of egg development time at 20 degrees C (d) (BOTTRELL)}
      \item{dtmin =}{5, minimum egg development time (d) (SALMO)}
      \item{tref =}{293.15, reference temperature 20 degrees C (K)}
      \item{taegg =}{11500, Arrhenius temperature of egg development time (Kelvin temperature), 
           approximated after the curve of Bottrell (BOTTRELL, KOOIJMAN)}
      \item{iz =}{TRUE, i-depth version (TRUE) or i-mean version}
      \item{xcomp =}{TRUE, phytoplankton self limiting (TRUE) or not}
    }
  }
} 
\references{
Benndorf, J. (1979). Kausalanalyse, theoretische Synthese und Simulation des
Eutrophierungsprozesses in stehenden und gestauten
Gew\enc{ä}{ae}ssern. Habilitationsschrift. Technische Universit\enc{ä}{ae}t
Dresden, Fakultät Bau-, Wasser- und Forstwesen, 165 S.

Benndorf, J. and Recknagel, F. (1982).
Problems of application of the ecological model SALMO
to lakes and reservoirs having various trophic states.
Ecol. Model. 17: 129-145.

Straskraba, M. and Gnauck, A. (1983). Modellierung limnischer \enc{Ö}{Oe}kosysteme.
Fischer, Jena.
}


\seealso{
  \code{\link[simecol]{sim}},
  \code{\link[simecol]{parms}},
  \code{\link[simecol:parms]{init}},
  \code{\link[simecol:parms]{times}}.
}
\examples{
slight <- slight()
\dontrun{
plot(sim(slight))

## show input matrix
View(inputs(slight))

## reduce phosphorus
inputs(slight)[,"pin"] <- 10
plot(sim(slight))

}
}
\keyword{ misc }
