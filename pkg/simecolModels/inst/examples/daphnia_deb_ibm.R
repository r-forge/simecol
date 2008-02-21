daphnia_deb_ibm <- function(submodel=daphnia_deb_phyto()) {
  new("indbasedModel",
    times = c(from=0, to=20, by=1),
    equations = c(daphnia_deb_equations, daphnia_deb_lifeequations),
    solver = "iteration",
    parms = c(as.list(parms(submodel)),  # is already list
            list(samplesize=c(50, 100))
    ),
    init =  list(X = c(x1=0, x2=0, x3=0.25),
                 Z = data.frame(
                   age        = c(1, 2, 3),
                   weight     = c(0.57, 0.90, 1.30),
                   egg        = 0,
                   eggage     = 0,
                   length     = c(0.71, 0.82, 0.93),
                   eggstorage = 0
                  ),
                  refVol = 1  # L
    ),
    main = function(time, init, parms) {
      X      <- init$X
      Z      <- init$Z
      refVol <- init$refVol
      cat("time=", time, "ref. Vol=", refVol, "abu=", nrow(Z) / refVol, "\n")

      ## initialize DEB-Phyto submodel
      parms(submodel)["refVol"] <- refVol
      init(submodel)            <- list2vec(list(X = X, Z = Z))
      ## simulate DEB-Phyto submodel
      statelist <- live(submodel) # calls sim(submodel)

      ## now the individual-based part
      Z             <- statelist$Z
      Z             <- survive(Z, parms)                    # survival
      Z             <- hatch(Z, parms)                      # hatching
      scaled        <- rescale(Z, refVol, parms$samplesize) # re-scaling
      Z             <- scaled$Z
      refVol        <- scaled$refVol

      statelist$Z <- Z
      c(statelist, refVol = as.numeric(refVol))
    },
    observer = function(statelist){
      X          <- statelist$X
      Z          <- statelist$Z
      refVol     <- statelist$refVol
      c(X["x1"], X["x2"], X["x3"],
        abundance = nrow(Z)/refVol,
        age       = mean(Z$age),
        weight    = mean(Z$weight),
        eggs      = mean(Z$egg),
        length    = mean(Z$length),
        refVol    = refVol
      )
    }
  )
}
