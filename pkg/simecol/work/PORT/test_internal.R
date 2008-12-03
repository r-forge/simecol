


#res <- fitOdeModel(cs2, whichpar=c("vm", "km"), obstime, yobs,
#  debuglevel=2, fn = ssqOdeModel,
#  method = "PORT", lower = c(vm=0, km=0), upper=c(vm=4, km=20),
#  control=list(trace=TRUE),
#  atol=1e-4, rtol=1e-4)
#



m <- nlminb(start=c(vm=1, km=5), objective = ssqOdeModel, #gradient = NULL, hessian = NULL,
         simObj = cs2, obstime = obstime,
         yobs = yobs, sd.yobs = 1,#sd.yobs,
         pnames = c("vm", "km"),
         initialize = TRUE,
         debuglevel = 0,
         scale = 1, control = list(trace=TRUE))#, lower = lower, upper = upper)
