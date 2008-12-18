library("simecol")

source("scheffer2001.R")

mlist <- lapply(vector("list", 3), function(l) l <- scheffer2001())

q <- c(0.812, 1.0, 1.231)
for (i in 1:3)  {
  parms(mlist[[i]])["q"]       <- q[i]
  times(mlist[[i]])["to"]      <- 3650
  parms(mlist[[i]])["equinox"] <- 365
  init(mlist[[i]]) <- c(A=1, Z=0.5)
}


lapply(mlist, function(l) parms(l)["q"])


for(i in 1:3) mlist[[i]] <- sim(mlist[[i]])

pdf()
for(i in 1:3) plot(mlist[[i]])
dev.off()