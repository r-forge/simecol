library("simecol")

source("scheffer2001.R")


q <- c(0.812, 1.0, 1.231)
q <- seq(0.2, 1.4, 0.01)

mlist <- lapply(vector("list", length(q)), function(l) l <- scheffer2001())

for (i in 1:length(q))  {
  parms(mlist[[i]])["q"]       <- q[i]
  times(mlist[[i]])["to"]      <- 3650 %/% 2
  parms(mlist[[i]])["equinox"] <- 365
  init(mlist[[i]]) <- c(A=1, Z=0.5)
}


lapply(mlist, function(l) parms(l)["q"])


for(i in 1:length(q)) {
  mlist[[i]] <- sim(mlist[[i]])
  #plot(mlist[[i]])
}

#pdf()
#for(i in 1:length(q)) 
#plot(mlist[[i]]@out, #which=c("A", "Z", "sigma_t", "q"), 
#  ylim=list(c(0,10), c(0,10), c(0,2), c(0, 1.4)))
#dev.off()


for(i in 1:length(q)) {
  png(paste("img", i+1000, ".png", sep=""), width=800, height=400)
  par(mar=c(5.1,4,5,1.5))
  par(cex.axis=2)
  par(cex.lab=2)
  par(cex.main=2)
  par(las=1)
  plot(mlist[[i]]@out, which=c("q", "A", "Z"), 
    #col=list(list("blue"), list("green"), list("brown")), ## does not work
    main=c("q=10^Temp", "Algae", "Zooplankton"), #cex=1.4,
    ylim=list(c(0.2, 1.5), c(0,10), c(0,10)), mfrow=c(1,3)#,
    #par(mar=list(c(5.1,4,0,0.5), c(5.1,0,4,0.5), c(5.1,0,4,0.5)) ) # does not work yet
    )
  dev.off()
}

