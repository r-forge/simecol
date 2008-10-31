## This class extends class simObj.
## It is intended for analytically solved ODE models
## or empirical regression-type models over time.
setClass("timedepModel",
         representation(
           parms  = "numericOrlist",
           init   = "numeric"
         ),
         contains = "odeModel"
)
