setClass("indbasedModel",
         representation(
           parms  = "list",
           init   = "listOrdata.frame",
           observer = "functionOrNULL"
         ),
         contains = "simObj"
)

setClass("timedepModel",
         representation(
           parms  = "numericOrlist",
           init   = "numeric"
         ),
         contains = "odeModel"
)
