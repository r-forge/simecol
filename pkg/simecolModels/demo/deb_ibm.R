## load individual-level model (DEB and phyto, differential equations)
submodel <- daphnia_deb_phyto()

## test one outer step alone
m <- sim(submodel)
out(m)

### load individual-based model (IBM, discrete, population dynamics)
deb_ibm <- daphnia_deb_ibm()

initfunc(deb_ibm) <- function(obj) {
 p <- as.list(parms(obj))
      parms(obj)["WAM"] <- length2weight(p$SAM, p$l2w)  # weight at maturity
      parms(obj)["WON"] <- length2weight(p$SON, p$l2w)  # weight of neonates
      age <- sample(1:30, 100, repl=TRUE)
      len    <- p$SON + 2 * age/30 ## simple linear approximation
      weight <- length2weight(len, p$l2w)

      initlist <- list(X = c(x1=0, x2=0, x3=0.25),
                       Z = data.frame(
                         age        = age,
                         weight     = weight,
                         egg        = sample(1:5, 100, repl=TRUE),
                         eggage     = runif(100, min=0, max=5),
                         length     = len,
                         eggstorage = 0
                        ),
                        refVol = 10 # 10L
      )
      init(obj) <- initlist #list2vec(initlist)
      obj
}

observer(deb_ibm) <- function (statelist) {
    X <- statelist$X
    Z <- statelist$Z
    refVol <- statelist$refVol
    hist(Z$length, col="red", breaks=seq(0.5,3,0.25), ylim=c(0,150))
    c(X["x1"], X["x2"], X["x3"], abundance = nrow(Z)/refVol,
        age = mean(Z$age), weight = mean(Z$weight), eggs = mean(Z$egg),
        length = mean(Z$length), refVol = refVol)
}

deb_ibm <- initialize(deb_ibm)

times(deb_ibm)["to"] <- 200
parms(deb_ibm)$samplesize <- c(100, 200)
parms(deb_ibm)$life.span <- 60

deb_ibm <- sim(deb_ibm)

o <- out(deb_ibm)

### plot results
par(mfrow=c(3,1))
plot(o$time, o$x1, type="l",
  main="Phytoplankton", col="red", ylim=c(0,0.5),xlab="Day",ylab="mg C / L")
lines(o$time, o$x2, col="blue")
lines(o$time, o$x3, col="green")
plot(o$time, o$abundance, type="l", main="Abundance",xlab="Day", ylab="Ind / L")
bm.mgc <- o$weight * o$abund / 1000
plot(o$time, bm.mgc , type="l", main="Biomass",xlab="Day", ylab="mg C / L")

