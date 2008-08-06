setClass("timedepModel",
         representation(
           parms  = "numericOrlist",
           init   = "numeric"
         ),
         contains = "odeModel"
)
